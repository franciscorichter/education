# Plataforma ENLA de Análisis de Datos Educativos
# Versión simplificada - análisis por cuestionario

suppressPackageStartupMessages({
  library(shiny)
  library(readxl)
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(igraph)
  library(RColorBrewer)
  library(tibble)
  library(scales)
  library(DT)
  library(htmltools)
  library(ggplot2)
})

# ===== CARGAR DATOS =====

# Obtener el directorio donde está este script
get_script_dir <- function() {
  tryCatch({
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      return(normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path), mustWork = TRUE))
    }
  }, error = function(e) {
    # Alternativa: usar directorio de trabajo actual
  })

  return(normalizePath(getwd(), mustWork = TRUE))
}

app_dir <- get_script_dir()

# Cargar función de integración dinámica
source(file.path(app_dir, "../scripts/data_integration.R"))

# Cargar datos procesados
raw_data_path <- file.path(app_dir, "../data/enla_processed_data.rds")
if (file.exists(raw_data_path)) {
  enla_data <- tryCatch(readRDS(raw_data_path), error = function(e) list())
} else {
  enla_data <- list()
}

# Construir resúmenes EM al igual que el pipeline
em_summaries <- NULL
if (!is.null(enla_data$em_data)) {
  # La función aggregate_em_data está en scripts/data_integration.R
  em_summaries <- tryCatch(aggregate_em_data(enla_data$em_data), error = function(e) NULL)
}

# Obtener nombres de cuestionarios
questionnaire_names <- names(enla_data$questionnaire_data)
if (is.null(questionnaire_names)) {
  questionnaire_names <- character(0)
}
# Excluir cuestionarios base_web2
questionnaire_names <- questionnaire_names[!grepl("(?i)base_web2", questionnaire_names)]

# Título amigable desde nombre del archivo
friendly_title <- function(p) {
  b <- p
  bl <- tolower(b)
  if (grepl("estudiante", bl)) return("Estudiante")
  if (grepl("docente(mat|matemat)", bl)) return("Docente Matemática")
  if (grepl("docente(com|comunic)", bl)) return("Docente Comunicación")
  if (grepl("docente(tutor)", bl)) return("Docente Tutor")
  if (grepl("director.*f1", bl)) return("Director F1")
  if (grepl("director.*f2", bl)) return("Director F2")
  if (grepl("familia", bl)) return("Familia")
  b
}

# ===== Diccionario: lectura de la hoja 2 (o 'Diccionario') =====
get_any_dict <- function(xlsx_path) {
  if (!file.exists(xlsx_path)) return(list(wide = tibble::tibble(), best = tibble::tibble()))
  sheets <- tryCatch(readxl::excel_sheets(xlsx_path), error = function(e) character())
  if (length(sheets) < 1) return(list(wide = tibble::tibble(), best = tibble::tibble()))
  # Elegir hoja de diccionario si existe
  sheet_name <- if (any(grepl("(?i)^diccionario$", sheets))) sheets[grepl("(?i)^diccionario$", sheets)][1] else sheets[min(2, length(sheets))]
  dict_raw <- tryCatch(readxl::read_excel(xlsx_path, sheet = sheet_name, col_names = TRUE), error = function(e) NULL)
  if (is.null(dict_raw)) return(list(wide = tibble::tibble(), best = tibble::tibble()))
  dict_raw <- suppressMessages(tibble::as_tibble(dict_raw))

  nm <- names(dict_raw)
  nm_l <- tolower(nm)
  # Intentar detectar columnas clave
  # código del ítem (pXX o pXX_YY)
  is_code_match <- function(v) {
    vv <- stringr::str_trim(as.character(v))
    m <- grepl("(?i)^p\\d{1,2}(?:_\\d{1,2})?$", vv)
    mean(m, na.rm = TRUE)
  }
  code_scores <- vapply(dict_raw, is_code_match, numeric(1))
  code_idx <- if (any(code_scores > 0)) which.max(code_scores) else 1
  # pregunta (enunciado)
  pref_col <- function(tokens) which.max(ifelse(nm_l %in% nm_l, sapply(nm_l, function(x) max(grepl(paste(tokens, collapse="|"), x, ignore.case = TRUE))), 0))
  q_idx <- suppressWarnings(pref_col(c("pregunta","enunciado","preg","stem")))
  if (length(q_idx) == 0 || q_idx < 1 || q_idx > ncol(dict_raw)) q_idx <- min(2, ncol(dict_raw))
  # descripción/descriptor
  d_idx <- suppressWarnings(pref_col(c("descriptor","descripcion","desc","def")))
  if (length(d_idx) == 0 || d_idx < 1 || d_idx > ncol(dict_raw)) d_idx <- min(3, ncol(dict_raw))
  # columnas de opciones
  opt_idx <- which(
    grepl("^(cat|op|opt|opc|opcion|resp|alt)[_. ]?\\d+", nm_l) |
      grepl("^cat_?\\d+$", nm_l) |
      grepl("^v[0-9]+$", nm_l) |
      grepl("^op_?\\d+$", nm_l)
  )
  opt_idx <- setdiff(opt_idx, c(code_idx, q_idx, d_idx))

  # Construir dataframe normalizado
  code_vec <- stringr::str_trim(as.character(dict_raw[[code_idx]]))
  normalize_code <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    m <- regmatches(x, regexpr("(?i)^p\\d{1,2}(?:_\\d{1,2})?$", x))
    if (length(m) == 0 || !nzchar(m)) return(NA_character_)
    m <- tolower(m)
    parts <- strsplit(sub("^p", "", m), "_")[[1]]
    top <- sprintf("p%02d", suppressWarnings(as.integer(parts[1])))
    if (length(parts) > 1) paste0(top, "_", sprintf("%02d", suppressWarnings(as.integer(parts[2])))) else top
  }
  norm_codes <- vapply(code_vec, normalize_code, character(1))
  wide <- tibble(
    code = norm_codes,
    pregunta = stringr::str_squish(as.character(dict_raw[[q_idx]])),
    descripcion = stringr::str_squish(as.character(dict_raw[[d_idx]]))
  )
  # columna top
  wide <- wide %>% mutate(top = sub("^(p\\d{2}).*", "\\1", tolower(code)))
  # Adjuntar opciones
  # ---- EDA Grouped Plots (comparative) ----
  # Helpers to detect and standardize grouping columns
  eda_detect_group_cols <- reactive({
    df <- enla_data$em_data
    if (is.null(df)) return(list(gender = NA_character_, language = NA_character_, school = NA_character_))
    nm <- names(df)
    gender <- c("SEXO","sexo","genero","genero_alumno","genero_estudiante","gender","sex")
    language <- c("lengua_materna","idioma_materno","lengua_madre","lengua","idioma","lengua_estudiante","lengua_principal")
    school <- c("gestion2","GESTION","gestion","gestion_escuela","gestion_colegio","gestion_ie","tipo_gestion","sector","sector_escuela")
    list(
      gender = if (any(gender %in% nm)) (gender[gender %in% nm])[1] else NA_character_,
      language = if (any(language %in% nm)) (language[language %in% nm])[1] else NA_character_,
      school = if ("gestion2" %in% nm) "gestion2" else if (any(school %in% nm)) (school[school %in% nm])[1] else NA_character_
    )
  })
  # Build grouped df for a given type
  eda_build_df_for <- function(group_type) {
    df <- enla_data$em_data
    if (is.null(df)) return(NULL)
    cols <- eda_detect_group_cols()
    grp_col <- switch(group_type,
                      gender = cols$gender,
                      language = cols$language,
                      school = cols$school,
                      NA_character_)
    if (is.na(grp_col) || !(grp_col %in% names(df))) return(NULL)
    graw <- df[[grp_col]]
    gchr <- trimws(tolower(as.character(graw)))
    if (group_type == "gender") {
      boy_tokens <- c("m","masculino","male","nino","niño","hombre","1","varon","varón")
      girl_tokens <- c("f","femenino","female","nina","niña","mujer","2")
      group <- ifelse(gchr %in% boy_tokens, "boy",
                      ifelse(gchr %in% girl_tokens, "girl", NA_character_))
    } else if (group_type == "language") {
      spanish_tokens <- c("castellano","espanol","español","espaniol","castellana","cast","esp")
      group <- ifelse(grepl(paste(spanish_tokens, collapse = "|"), gchr), "spanish", "other")
    } else {
      public_tokens <- c("publico","pública","publica","público","estatal","nacional","municipal")
      private_tokens <- c("privado","privada","particular","parroquial")
      group <- ifelse(grepl(paste(public_tokens, collapse = "|"), gchr), "public",
                      ifelse(grepl(paste(private_tokens, collapse = "|"), gchr), "private", NA_character_))
    }
    out <- tibble::tibble(
      group = factor(group),
      medida500_M = suppressWarnings(as.numeric(df$medida500_M)),
      medida500_L = suppressWarnings(as.numeric(df$medida500_L))
    )
    out <- dplyr::filter(out, !is.na(group) & !is.na(medida500_M) & !is.na(medida500_L))
    if (nrow(out) == 0) return(NULL)
    out
  }

  # Renderers for each comparative section
  render_comp_plot <- function(df, plot_type) {
    validate(need(!is.null(df) && nrow(df) > 0, "Sin datos suficientes"))
    if (plot_type == "violin") {
      long <- tidyr::pivot_longer(df, cols = c("medida500_M","medida500_L"), names_to = "measure", values_to = "value")
      ggplot(long, aes(x = group, y = value, fill = group)) +
        geom_violin(trim = FALSE, alpha = 0.6, color = "grey60") +
        geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.9, fill = "white") +
        facet_wrap(~ measure, scales = "free_y") +
        labs(x = NULL, y = NULL) +
        theme_bw(base_size = 12) + theme(legend.position = "none")
    } else {
      set.seed(123)
      n_keep <- min(60000, nrow(df))
      if (nrow(df) > n_keep) df <- df[sample.int(nrow(df), n_keep), , drop = FALSE]
      ggplot(df, aes(x = medida500_M, y = medida500_L, color = group)) +
        geom_point(alpha = 0.12, size = 0.5) +
        stat_density_2d(linewidth = 0.8) +
        labs(x = "Math (medida500_M)", y = "Language (medida500_L)", color = "Grupo") +
        theme_bw(base_size = 12) +
        scale_color_viridis_d(option = "C", end = 0.85)
    }
  }

  output$eda_plot_gender <- renderPlot({
    df <- eda_build_df_for("gender")
    render_comp_plot(df, input$eda_plot_type)
  })

  output$eda_plot_language <- renderPlot({
    df <- eda_build_df_for("language")
    render_comp_plot(df, input$eda_plot_type)
  })

  output$eda_plot_school <- renderPlot({
    df <- eda_build_df_for("school")
    render_comp_plot(df, input$eda_plot_type)
  })

  # L/M integration diagnostics
  lm_diag <- eventReactive(input$check_lm, {
    sel <- input$questionnaire_tab
    if (is.null(sel) || !(sel %in% questionnaire_names)) return(list(msg = "Selecciona un cuestionario"))
    q <- enla_data$questionnaire_data[[sel]]
    if (is.null(q) || !q$success) return(list(msg = "No hay datos disponibles"))
    fd <- q$full_data
    hasL <- "medida500_L" %in% names(fd)
    hasM <- "medida500_M" %in% names(fd)
    n <- nrow(fd)
    pctL <- if (hasL) mean(!is.na(fd$medida500_L)) * 100 else NA_real_
    pctM <- if (hasM) mean(!is.na(fd$medida500_M)) * 100 else NA_real_
    # Sample rows missing both L and M
    miss_both <- if (hasL && hasM) which(is.na(fd$medida500_L) & is.na(fd$medida500_M)) else integer()
    sample_idx <- head(miss_both, 3)
    id_cols <- intersect(c("ID_ESTUDIANTE","cod_mod7","anexo","ID_seccion"), names(fd))
    examples <- if (length(sample_idx) > 0 && length(id_cols) > 0) as.data.frame(fd[sample_idx, id_cols, drop = FALSE]) else NULL
    list(n = n, hasL = hasL, hasM = hasM, pctL = pctL, pctM = pctM, examples = examples)
  })

  output$lm_integration_status <- renderPrint({
    d <- lm_diag()
    if (is.null(d)) return()
    if (!is.null(d$msg)) { cat(d$msg); return() }
    cat("=== L/M Integration Check ===\n\n")
    cat(sprintf("Filas: %d\n", d$n))
    cat(sprintf("L presente: %s | M presente: %s\n", ifelse(d$hasL, "sí", "no"), ifelse(d$hasM, "sí", "no")))
    if (!is.na(d$pctL)) cat(sprintf("Cobertura L (no NA): %.1f%%\n", d$pctL))
    if (!is.na(d$pctM)) cat(sprintf("Cobertura M (no NA): %.1f%%\n", d$pctM))
    if (!is.null(d$examples)) {
      cat("\nEjemplos sin L/M (hasta 3):\n")
      print(d$examples)
    }
  })

  # ====== EDA Outputs ======
  output$eda_em_summary <- renderPrint({
    if (is.null(enla_data$em_data)) {
      cat("❌ Datos EM no disponibles")
      return()
    }
    df <- enla_data$em_data
    cat("=== RESUMEN EM ===\n\n")
    cat(sprintf("Filas: %d\n", nrow(df)))
    cat(sprintf("Columnas: %d\n", ncol(df)))
    cat("\nPorcentaje de NA por columna:\n")
    na_pct <- sapply(df, function(x) mean(is.na(x))*100)
    for (nm in names(na_pct)) {
      cat(sprintf(" - %s: %.1f%%\n", nm, na_pct[[nm]]))
    }
    if (all(c("medida500_L","medida500_M") %in% names(df))) {
      rngL <- range(df$medida500_L, na.rm = TRUE)
      rngM <- range(df$medida500_M, na.rm = TRUE)
      cat(sprintf("\nRangos:\n - L: %.1f a %.1f\n - M: %.1f a %.1f\n", rngL[1], rngL[2], rngM[1], rngM[2]))
    }
  })

  output$eda_em_structure <- renderDT({
    if (is.null(enla_data$em_data)) return(data.frame(Mensaje = "No hay datos EM"))
    head(enla_data$em_data, 100)
  }, options = list(pageLength = 10, scrollX = TRUE))

  output$eda_hist_L <- renderPlot({
    if (is.null(enla_data$em_data) || !("medida500_L" %in% names(enla_data$em_data))) return()
    x <- enla_data$em_data$medida500_L
    x <- x[!is.na(x)]
    hist(x, breaks = 40, col = "#4F46E5", border = NA, main = "Lectura (L)", xlab = "medida500_L")
    abline(v = mean(x, na.rm = TRUE), col = "#111827", lwd = 2, lty = 2)
  })

  output$eda_hist_M <- renderPlot({
    if (is.null(enla_data$em_data) || !("medida500_M" %in% names(enla_data$em_data))) return()
    x <- enla_data$em_data$medida500_M
    x <- x[!is.na(x)]
    hist(x, breaks = 40, col = "#059669", border = NA, main = "Matemática (M)", xlab = "medida500_M")
    abline(v = mean(x, na.rm = TRUE), col = "#111827", lwd = 2, lty = 2)
  })

  output$eda_integration_quality <- renderPrint({
    ir <- enla_data$integration_results
    if (is.null(ir) || length(ir) == 0) {
      cat("❌ No hay resultados de integración en el RDS")
      return()
    }
    cat("=== CALIDAD DE INTEGRACIÓN (pipeline) ===\n\n")
    for (nm in names(ir)) {
      r <- ir[[nm]]
      if (isTRUE(r$success)) {
        cat(sprintf(" - %s: %d/%d (%.1f%%)\n", nm, r$matched_rows, r$total_rows, r$match_rate*100))
      } else {
        cat(sprintf(" - %s: ❌ %s\n", nm, r$error))
      }
    }
    if (!is.null(enla_data$pipeline_stats) && !is.null(enla_data$pipeline_stats$match_rate_summary)) {
      s <- enla_data$pipeline_stats$match_rate_summary
      cat(sprintf("\nResumen tasas: min %.1f%% | max %.1f%% | media %.1f%%\n", s$min*100, s$max*100, s$mean*100))
    }
  })

  return(normalizePath(getwd(), mustWork = TRUE))
}

app_dir <- get_script_dir()

# Cargar función de integración dinámica
source(file.path(app_dir, "../scripts/data_integration.R"))

# Cargar datos procesados
raw_data_path <- file.path(app_dir, "../data/enla_processed_data.rds")
if (file.exists(raw_data_path)) {
  enla_data <- tryCatch(readRDS(raw_data_path), error = function(e) list())
} else {
  enla_data <- list()
}

# Construir resúmenes EM al igual que el pipeline
em_summaries <- NULL
if (!is.null(enla_data$em_data)) {
  # La función aggregate_em_data está en scripts/data_integration.R
  em_summaries <- tryCatch(aggregate_em_data(enla_data$em_data), error = function(e) NULL)
}

# Obtener nombres de cuestionarios
questionnaire_names <- names(enla_data$questionnaire_data)
if (is.null(questionnaire_names)) {
  questionnaire_names <- character(0)
}
# Excluir cuestionarios base_web2
questionnaire_names <- questionnaire_names[!grepl("(?i)base_web2", questionnaire_names)]

# Título amigable desde nombre del archivo
friendly_title <- function(p) {
  b <- p
  bl <- tolower(b)
  if (grepl("estudiante", bl)) return("Estudiante")
  if (grepl("docente(mat|matemat)", bl)) return("Docente Matemática")
  if (grepl("docente(com|comunic)", bl)) return("Docente Comunicación")
  if (grepl("docente(tutor)", bl)) return("Docente Tutor")
  if (grepl("director.*f1", bl)) return("Director F1")
  if (grepl("director.*f2", bl)) return("Director F2")
  if (grepl("familia", bl)) return("Familia")
  b
}

# ===== Diccionario: lectura de la hoja 2 (o 'Diccionario') =====
get_any_dict <- function(xlsx_path) {
  if (!file.exists(xlsx_path)) return(list(wide = tibble::tibble(), best = tibble::tibble()))
  sheets <- tryCatch(readxl::excel_sheets(xlsx_path), error = function(e) character())
  if (length(sheets) < 1) return(list(wide = tibble::tibble(), best = tibble::tibble()))
  # Elegir hoja de diccionario si existe
  sheet_name <- if (any(grepl("(?i)^diccionario$", sheets))) sheets[grepl("(?i)^diccionario$", sheets)][1] else sheets[min(2, length(sheets))]
  dict_raw <- tryCatch(readxl::read_excel(xlsx_path, sheet = sheet_name, col_names = TRUE), error = function(e) NULL)
  if (is.null(dict_raw)) return(list(wide = tibble::tibble(), best = tibble::tibble()))
  dict_raw <- suppressMessages(tibble::as_tibble(dict_raw))

  nm <- names(dict_raw)
  nm_l <- tolower(nm)
  # Intentar detectar columnas clave
  # código del ítem (pXX o pXX_YY)
  is_code_match <- function(v) {
    vv <- stringr::str_trim(as.character(v))
    m <- grepl("(?i)^p\\d{1,2}(?:_\\d{1,2})?$", vv)
    mean(m, na.rm = TRUE)
  }
  code_scores <- vapply(dict_raw, is_code_match, numeric(1))
  code_idx <- if (any(code_scores > 0)) which.max(code_scores) else 1
  # pregunta (enunciado)
  pref_col <- function(tokens) which.max(ifelse(nm_l %in% nm_l, sapply(nm_l, function(x) max(grepl(paste(tokens, collapse="|"), x, ignore.case = TRUE))), 0))
  q_idx <- suppressWarnings(pref_col(c("pregunta","enunciado","preg","stem")))
  if (length(q_idx) == 0 || q_idx < 1 || q_idx > ncol(dict_raw)) q_idx <- min(2, ncol(dict_raw))
  # descripción/descriptor
  d_idx <- suppressWarnings(pref_col(c("descriptor","descripcion","desc","def")))
  if (length(d_idx) == 0 || d_idx < 1 || d_idx > ncol(dict_raw)) d_idx <- min(3, ncol(dict_raw))
  # columnas de opciones
  opt_idx <- which(
    grepl("^(cat|op|opt|opc|opcion|resp|alt)[_. ]?\\d+", nm_l) |
      grepl("^cat_?\\d+$", nm_l) |
      grepl("^v[0-9]+$", nm_l) |
      grepl("^op_?\\d+$", nm_l)
  )
  opt_idx <- setdiff(opt_idx, c(code_idx, q_idx, d_idx))

  # Construir dataframe normalizado
  code_vec <- stringr::str_trim(as.character(dict_raw[[code_idx]]))
  normalize_code <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    m <- regmatches(x, regexpr("(?i)^p\\d{1,2}(?:_\\d{1,2})?$", x))
    if (length(m) == 0 || !nzchar(m)) return(NA_character_)
    m <- tolower(m)
    parts <- strsplit(sub("^p", "", m), "_")[[1]]
    top <- sprintf("p%02d", suppressWarnings(as.integer(parts[1])))
    if (length(parts) > 1) paste0(top, "_", sprintf("%02d", suppressWarnings(as.integer(parts[2])))) else top
  }
  norm_codes <- vapply(code_vec, normalize_code, character(1))
  wide <- tibble(
    code = norm_codes,
    pregunta = stringr::str_squish(as.character(dict_raw[[q_idx]])),
    descripcion = stringr::str_squish(as.character(dict_raw[[d_idx]]))
  )
  # columna top
  wide <- wide %>% mutate(top = sub("^(p\\d{2}).*", "\\1", tolower(code)))
  # Adjuntar opciones
  if (length(opt_idx) > 0) {
    extras <- dict_raw[opt_idx]
    names(extras) <- make.names(names(extras), unique = TRUE)
    wide <- dplyr::bind_cols(wide, extras)
  }
  # Filtrar y ordenar
  wide <- wide %>%
    filter(!is.na(code) & grepl("^p\\d{2}(_\\d{2})?$", tolower(code))) %>%
    filter(!is.na(pregunta) | !is.na(descripcion)) %>%
    distinct(code, .keep_all = TRUE) %>%
    arrange(top, code)

  # Resumen por top: stem, sub-items y opciones colapsadas
  option_cols <- setdiff(names(wide), c("code","pregunta","descripcion","top"))
  collapse_opts_vec <- function(df_top) {
    if (length(option_cols) == 0) return(NA_character_)
    vals <- as.character(unlist(df_top[option_cols], use.names = FALSE))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0) NA_character_ else paste(unique(vals), collapse = " | ")
  }
  safe_first <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x) > 0) x[1] else NA_character_
  }
  summarize_top <- function(df_top) {
    top_code <- unique(df_top$top)[1]
    cand1 <- safe_first(df_top$pregunta[df_top$code == top_code])
    cand2 <- safe_first(df_top$pregunta)
    cand3 <- safe_first(df_top$descripcion)
    stem <- safe_first(c(cand1, cand2, cand3))
    sub_rows <- df_top %>% dplyr::filter(grepl("_\\d{2}$", code) & !is.na(descripcion) & nzchar(descripcion))
    subitems <- unique(sub_rows$descripcion)
    options <- collapse_opts_vec(df_top)
    tibble::tibble(top = top_code, question = stem, subitems = list(subitems), options = options)
  }
  best <- if (nrow(wide) == 0) tibble::tibble(top = character(), question = character(), subitems = I(list()), options = character())
  else wide %>% dplyr::group_by(top) %>% dplyr::group_modify(~ summarize_top(.x)) %>% dplyr::ungroup()
  list(wide = wide, best = best)
}

# Ubicar el XLSX original del cuestionario
find_questionnaire_xlsx <- function(q_name) {
  # Directorio típico
  candidate <- file.path(app_dir, "../data/xlsx", paste0(q_name, ".xlsx"))
  if (file.exists(candidate)) return(normalizePath(candidate, mustWork = TRUE))
  # Búsqueda recursiva si no está en el lugar esperado
  root <- normalizePath(file.path(app_dir, ".."), mustWork = TRUE)
  hits <- tryCatch(list.files(root, pattern = paste0("^", q_name, "\\\\.xlsx$"), recursive = TRUE, full.names = TRUE, ignore.case = TRUE), error = function(e) character())
  if (length(hits) > 0) return(hits[1])
  return(NA_character_)
}

# ===== UI =====

# Construir pestañas por cuestionario (solo títulos/valores)
questionnaire_tabs <- lapply(questionnaire_names, function(name) {
  tabPanel(title = friendly_title(name), value = name)
})

ui <- fluidPage(
  title = "Plataforma ENLA de Análisis de Datos Educativos",
  tags$head(tags$style(HTML(
    "body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif;}
     .muted{color:#6b7280}
     table.dataTable td, table.dataTable th { white-space: normal !important; word-wrap: break-word; }
     .dataTables_wrapper .dataTables_scrollBody{ overflow: auto; }")
  )),

  h1("Plataforma ENLA de Análisis de Datos Educativos", style = "color: #2c3e50; font-weight: bold;"),
  p("Análisis de datos educativos ENLA - selecciona un cuestionario para comenzar",
    style = "color: #7f8c8d; font-size: 16px; margin-bottom: 20px;"),

  # Estado de datos
  wellPanel(
    h4("📊 Estado de Datos", style = "margin-top: 0;"),
    fluidRow(
      column(6,
        h5("Datos de Cuestionarios"),
        textOutput("data_status", inline = TRUE)
      ),
      column(6,
        h5("Datos de Rendimiento EM"),
        textOutput("em_status", inline = TRUE)
      )
    )
  ),

  # Pestañas principales (usar do.call para pasar correctamente los tabPanel)
  do.call(tabsetPanel, c(
    list(
      id = "main_tabs",
      type = "tabs",

      # Nueva pestaña: Análisis Exploratorio de Datos (EDA)
      tabPanel(
        title = "📊 EDA",
        value = "eda",
        h3("Exploratory Data Analysis — EM"),
        p("Comparative plots and dataset summary."),
        tabsetPanel(
          id = "eda_tabs",
          tabPanel(
            title = "Plots",
            value = "plots",
            wellPanel(
              fluidRow(
                column(12,
                  selectInput("eda_plot_type", "Tipo de gráfico:",
                              choices = c("Violin" = "violin",
                                          "Scatter + Contour" = "scatter"),
                              selected = "violin", width = "100%")
                )
              )
            ),
            fluidRow(
              column(12, h4("Gender")),
              column(12, plotOutput("eda_plot_gender", height = "300px"))
            ),
            fluidRow(
              column(12, h4("Language")),
              column(12, plotOutput("eda_plot_language", height = "300px"))
            ),
            fluidRow(
              column(12, h4("School")),
              column(12, plotOutput("eda_plot_school", height = "300px"))
            ),
            fluidRow(
              column(12, h4("Area")),
              column(12, plotOutput("eda_plot_area", height = "300px"))
            )
          ),
          tabPanel(
            title = "Summary",
            value = "summary",
            h4("Dataset Dimensions"),
            DTOutput("eda_summary_table")
          )
        )
      ),
      # Pestaña de análisis de cuestionarios y redes
      tabPanel(
        title = "🔗 Network & Questionnaire Analysis",
        value = "nqa",
        h3("Correlation Networks and Questionnaire Exploration"),
        # Pestañas de cuestionarios (arriba)
        do.call(tabsetPanel, c(list(id = "questionnaire_tab", selected = if (length(questionnaire_names)>0) questionnaire_names[1] else NULL), questionnaire_tabs)),
        br(),
        tags$div(class = "muted", style = "margin-bottom:12px;",
                 HTML("<strong>Descripción:</strong> Selecciona un cuestionario arriba. Usa la pestaña <em>Network</em> para construir y explorar la red de ítems, y la pestaña <em>Questions</em> para ver las preguntas.")),
        tags$div(style = "margin:-6px 0 8px 0; color:#374151;",
                 textOutput("tab_counts", inline = TRUE)),
        hr(),
        tabsetPanel(
          id = "content_tabs",
          tabPanel(
            title = "Network",
            value = "network",
            wellPanel(
              fluidRow(
                column(4, sliderInput("thr", "Edge threshold |r|:", min = 0, max = 0.8, value = 0.2, step = 0.01, width = "100%")),
                column(5, radioButtons("level", "Network level:", inline = TRUE,
                                      choices = c("Second level: items pXX_YY" = "item",
                                                  "Top level: aggregates pXX" = "top"), selected = "item")),
                column(3, br(), actionButton("build_network", "Build network", class = "btn btn-primary", width = "100%"))
              ),
              conditionalPanel(
                condition = "input.level == 'top'",
                selectInput("agg", "Aggregation method:", choices = c("Mean" = "mean", "Median" = "median", "Z-Mean" = "zmean", "PCA-1" = "pca1"), selected = "mean")
              ),
              hr(),
              fluidRow(
                column(4, checkboxInput("include_lm", "Include L/M nodes (from full_data)", value = TRUE)),
                column(4, actionButton("check_lm", "Check L/M integration", class = "btn btn-secondary"))
              ),
              verbatimTextOutput("lm_integration_status")
            ),
            plotOutput("network_plot", height = "600px")
          ),
          tabPanel(
            title = "Questions",
            value = "questions",
            DTOutput("questions_table")
          )
        )
      ),
      # Pestaña de modelamiento avanzado (placeholder)
      tabPanel(
        title = "🤖 Advanced Modeling",
        value = "adv_model",
        h3("Advanced Modeling (coming soon)"),
        p("Modelos avanzados y análisis predictivo sobre datos integrados.")
      )
    ),
    list(
      # Pestaña de información
      tabPanel(
        title = "ℹ️ Información",
        value = "info",
        h3("Información del Sistema"),

        wellPanel(
          h4("📁 Archivos de Datos:"),
          verbatimTextOutput("file_info")
        ),

        wellPanel(
          h4("📊 Resumen General:"),
          fluidRow(
            column(6, h5("Cuestionarios disponibles:")),
            column(6, textOutput("total_questionnaires"))
          ),
          fluidRow(
            column(6, h5("Datos de EM:")),
            column(6, textOutput("em_summary"))
          )
        )
      )
    )
  ))
)

# ===== SERVER =====

server <- function(input, output, session) {

  # Estado de datos
  output$data_status <- renderText({
    if (length(questionnaire_names) == 0) {
      "❌ No hay datos de cuestionarios"
    } else {
      sprintf("✅ %d cuestionarios cargados", length(questionnaire_names))
    }
  })

  output$em_status <- renderText({
    if (is.null(enla_data$em_data)) {
      "❌ Datos EM no disponibles"
    } else {
      sprintf("✅ %d filas, %d columnas", nrow(enla_data$em_data), ncol(enla_data$em_data))
    }
  })

  # Información de archivos
  output$file_info <- renderPrint({
    cat("Cuestionarios disponibles:\n")
    for (name in questionnaire_names) {
      cat("• ", friendly_title(name), " (", name, ")\n")
    }
  })

  output$total_questionnaires <- renderText({
    length(questionnaire_names)
  })

  # ---- EDA (inside server): comparative plots and summary ----
  eda_detect_group_cols_v2 <- reactive({
    df <- enla_data$em_data
    if (is.null(df)) return(list(gender = NA_character_, language = NA_character_, school = NA_character_, area = NA_character_))
    nm <- names(df)
    list(
      gender = c("SEXO","sexo","genero","genero_alumno","genero_estudiante","gender","sex")[c("SEXO","sexo","genero","genero_alumno","genero_estudiante","gender","sex") %in% nm][1],
      language = c("lengua_materna","idioma_materno","lengua_madre","lengua","idioma","lengua_estudiante","lengua_principal")[c("lengua_materna","idioma_materno","lengua_madre","lengua","idioma","lengua_estudiante","lengua_principal") %in% nm][1],
      school = if ("gestion2" %in% nm) "gestion2" else c("GESTION","gestion","gestion_escuela","gestion_colegio","gestion_ie","tipo_gestion","sector","sector_escuela")[c("GESTION","gestion","gestion_escuela","gestion_colegio","gestion_ie","tipo_gestion","sector","sector_escuela") %in% nm][1],
      area = c("area","área","ambito","ámbito","zona","area_geografica","ambito_geografico")[c("area","área","ambito","ámbito","zona","area_geografica","ambito_geografico") %in% nm][1]
    )
  })

  eda_build_df_for_v2 <- function(group_type) {
    df <- enla_data$em_data
    if (is.null(df)) return(NULL)
    cols <- eda_detect_group_cols_v2()
    grp_col <- switch(group_type, gender = cols$gender, language = cols$language, school = cols$school, area = cols$area, NA_character_)
    if (is.na(grp_col) || !(grp_col %in% names(df))) return(NULL)
    gchr <- trimws(tolower(as.character(df[[grp_col]])))
    if (group_type == "gender") {
      boy <- c("m","masculino","male","nino","niño","hombre","1","varon","varón")
      girl <- c("f","femenino","female","nina","niña","mujer","2")
      group <- ifelse(gchr %in% boy, "boy", ifelse(gchr %in% girl, "girl", NA_character_))
    } else if (group_type == "language") {
      spanish <- c("castellano","espanol","español","espaniol","castellana","cast","esp")
      group <- ifelse(grepl(paste(spanish, collapse = "|"), gchr), "spanish", "other")
    } else if (group_type == "school") {
      public <- c("publico","pública","publica","público","estatal","nacional","municipal")
      private <- c("privado","privada","particular","parroquial")
      group <- ifelse(grepl(paste(public, collapse = "|"), gchr), "public",
                      ifelse(grepl(paste(private, collapse = "|"), gchr), "private", NA_character_))
    } else {
      rural <- c("rural")
      urban <- c("urbana","urbano","urban")
      group <- ifelse(grepl(paste(rural, collapse = "|"), gchr), "rural",
                      ifelse(grepl(paste(urban, collapse = "|"), gchr), "urban", NA_character_))
    }
    out <- tibble::tibble(
      group = factor(group),
      medida500_M = suppressWarnings(as.numeric(df$medida500_M)),
      medida500_L = suppressWarnings(as.numeric(df$medida500_L))
    )
    out <- dplyr::filter(out, !is.na(group) & !is.na(medida500_M) & !is.na(medida500_L))
    if (nrow(out) == 0) return(NULL)
    out
  }

  render_comp_plot_v2 <- function(df, plot_type) {
    validate(need(!is.null(df) && nrow(df) > 0, "Sin datos suficientes"))
    if (plot_type == "violin") {
      long <- tidyr::pivot_longer(df, cols = c("medida500_M","medida500_L"), names_to = "measure", values_to = "value")
      ggplot(long, aes(x = group, y = value, fill = group)) +
        geom_violin(trim = FALSE, alpha = 0.6, color = "grey60") +
        geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.9, fill = "white") +
        facet_wrap(~ measure, scales = "free_y") +
        labs(x = NULL, y = NULL) +
        theme_bw(base_size = 12) + theme(legend.position = "none")
    } else {
      set.seed(123)
      n_keep <- min(60000, nrow(df))
      if (nrow(df) > n_keep) df <- df[sample.int(nrow(df), n_keep), , drop = FALSE]
      ggplot(df, aes(x = medida500_M, y = medida500_L, color = group)) +
        geom_point(alpha = 0.12, size = 0.5) +
        stat_density_2d(linewidth = 0.8) +
        labs(x = "Math (medida500_M)", y = "Language (medida500_L)", color = "Grupo") +
        theme_bw(base_size = 12) +
        scale_color_viridis_d(option = "C", end = 0.85)
    }
  }

  output$eda_plot_gender <- renderPlot({ render_comp_plot_v2(eda_build_df_for_v2("gender"), input$eda_plot_type) })
  output$eda_plot_language <- renderPlot({ render_comp_plot_v2(eda_build_df_for_v2("language"), input$eda_plot_type) })
  output$eda_plot_school <- renderPlot({ render_comp_plot_v2(eda_build_df_for_v2("school"), input$eda_plot_type) })
  output$eda_plot_area   <- renderPlot({ render_comp_plot_v2(eda_build_df_for_v2("area"), input$eda_plot_type) })

  output$eda_summary_table <- renderDT({
    # EM
    rows <- if (!is.null(enla_data$em_data)) nrow(enla_data$em_data) else 0
    cols <- if (!is.null(enla_data$em_data)) ncol(enla_data$em_data) else 0
    em_row <- tibble::tibble(dataset = "EM (performance)", rows = rows, cols = cols, items = NA_integer_)
    # Questionnaires
    q_rows <- lapply(questionnaire_names, function(nm) {
      q <- enla_data$questionnaire_data[[nm]]
      if (is.null(q) || !q$success) return(tibble::tibble(dataset = nm, rows = NA_integer_, cols = NA_integer_, items = NA_integer_))
      tibble::tibble(dataset = nm, rows = nrow(q$data), cols = ncol(q$full_data), items = length(q$columns))
    })
    df <- dplyr::bind_rows(em_row, dplyr::bind_rows(q_rows))
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  # Conteo y resumen bajo el tab de NQA
  output$tab_counts <- renderText({
    sel <- input$questionnaire_tab
    if (is.null(sel) || !(sel %in% questionnaire_names)) return("")
    q <- enla_data$questionnaire_data[[sel]]
    if (is.null(q) || !q$success) return("")
    sprintf("%s: %d filas | %d ítems (pXX)", friendly_title(sel), nrow(q$data), length(q$columns))
  })

  # Análisis de red reactivo
  network_data <- reactive({
    sel <- input$questionnaire_tab
    if (is.null(sel) || !(sel %in% questionnaire_names)) return(NULL)

    req(input$build_network)

    questionnaire <- enla_data$questionnaire_data[[sel]]
    if (is.null(questionnaire) || !questionnaire$success) {
      return(list(error = "Datos del cuestionario no disponibles"))
    }

    # Usar datos del cuestionario
    X <- questionnaire$data
    item_cols <- questionnaire$columns

    if (nrow(X) == 0 || length(item_cols) < 2) {
      return(list(error = "Datos insuficientes para análisis de red"))
    }

    # Nivel de análisis
    level <- input$level
    if (level == "top") {
      items_detected <- length(unique(stringr::str_match(item_cols, "^(p\\d{2})")[,2]))
    } else {
      items_detected <- length(item_cols)
    }

    # Agregación a nivel constructo
    if (level == "top") {
      top_codes <- stringr::str_match(item_cols, "^(p\\d{2})")[,2]
      top_codes <- tolower(top_codes)
      split_cols <- split(item_cols, top_codes)
      split_cols <- split_cols[sapply(split_cols, length) > 1]

      if (length(split_cols) > 0) {
        agg_method <- input$agg
        X_top <- purrr::map_dfc(names(split_cols), function(tp) {
          cols <- split_cols[[tp]]
          vals <- dplyr::select(X, dplyr::all_of(cols))
          m <- as.matrix(vals)
          out <- if (agg_method == "mean") {
            rowMeans(m, na.rm = TRUE)
          } else if (agg_method == "median") {
            apply(m, 1, median, na.rm = TRUE)
          } else {
            rowMeans(m, na.rm = TRUE)
          }
          tibble::tibble(!!tp := out)
        })
        X <- X_top
        item_cols <- names(X_top)
      }
    }

    # Adjuntar nodos L/M desde full_data si corresponde
    if (isTRUE(input$include_lm) && !is.null(questionnaire$full_data)) {
      fd <- questionnaire$full_data
      if ("medida500_L" %in% names(fd)) {
        X$L <- suppressWarnings(as.numeric(fd$medida500_L))
      }
      if ("medida500_M" %in% names(fd)) {
        X$M <- suppressWarnings(as.numeric(fd$medida500_M))
      }
    }

    # Filtrar columnas de baja calidad
    sds <- map_dbl(X, ~ sd(.x, na.rm = TRUE))
    na_pct <- map_dbl(X, ~ mean(is.na(.x)))
    keep <- names(X)[sds > 0 & !is.na(sds) & na_pct < 0.9]
    X <- dplyr::select(X, dplyr::all_of(keep))

    # Calcular correlaciones
    cor_mat <- suppressWarnings(cor(X, use = "pairwise.complete.obs", method = "pearson"))
    inds <- which(upper.tri(cor_mat), arr.ind = TRUE)
    cor_df <- tibble(
      var1 = colnames(cor_mat)[inds[, 1]],
      var2 = colnames(cor_mat)[inds[, 2]],
      cor  = cor_mat[inds]
    ) %>% filter(!is.na(cor) & abs(cor) >= input$thr)

    # Grupos de nodos y colores
    node_names <- colnames(cor_mat)
    if (level == "top") {
      node_group <- node_names
    } else {
      node_group <- stringr::str_match(node_names, "^(p\\d{2})")[,2]
    }
    node_df <- tibble(name = node_names, group = node_group)

    groups <- sort(unique(node_df$group))
    base_pal <- RColorBrewer::brewer.pal(8, "Set2")
    palette <- grDevices::colorRampPalette(base_pal)(max(8, length(groups)))
    col_map <- setNames(rep(palette, length.out = length(groups)), groups)

    G <- igraph::graph_from_data_frame(cor_df, directed = FALSE, vertices = node_df)
    V(G)$color <- col_map[V(G)$group]
    E(G)$weight <- abs(E(G)$cor)

    list(G = G, col_map = col_map, base = friendly_title(sel),
         vars = item_cols, items_detected = items_detected)
  })

  # Gráfico de red
  output$network_plot <- renderPlot({
    req(network_data())
    if (!is.null(network_data()$error)) {
      plot(1, type = "n", xlab = "", ylab = "", main = network_data()$error)
      text(1, 1, network_data()$error, cex = 1.5)
      return()
    }

    G <- network_data()$G
    col_map <- network_data()$col_map
    set.seed(123)
    coords <- igraph::layout_with_fr(G)
    labels_pretty <- V(G)$name
    par(bg = "white", mar = c(1,1,1,1))
    title_suffix <- if (input$level == "top") "Nivel constructo (agregados pXX)" else "Nivel ítem (pXX_YY)"
    plot(G,
         layout = coords,
         vertex.label = labels_pretty,
         vertex.label.cex = 0.7,
         vertex.size = 8,
         vertex.color = V(G)$color,
         edge.width = scales::rescale(E(G)$weight, to = c(0.4, 3)),
         edge.color = rgb(0.2,0.2,0.2,0.25),
         main = paste0(network_data()$base, " — ", title_suffix, " (|r| ≥ ", input$thr, ")"))
    legend(
      "topleft",
      legend = names(col_map),
      col = unname(col_map),
      pch = 16,
      pt.cex = 1.2,
      bty = "n",
      ncol = 2,
      title = if (input$level == "top") "Constructo (pXX)" else "Grupo (pXX)"
    )
  })

  # Tabla de preguntas (ítems pXX con stem, sub-items y opciones)
  output$questions_table <- renderDT({
    sel <- input$questionnaire_tab
    if (is.null(sel) || !(sel %in% questionnaire_names)) {
      return(data.frame(Mensaje = "Selecciona un cuestionario"))
    }
    xlsx <- find_questionnaire_xlsx(sel)
    if (is.na(xlsx)) {
      return(data.frame(Mensaje = "Diccionario no encontrado para este cuestionario"))
    }
    d <- get_any_dict(xlsx)
    if (is.null(d$best) || nrow(d$best) == 0) {
      return(data.frame(Mensaje = "Diccionario vacío o no legible"))
    }
    df <- d$best %>% transmute(
      `Item (pXX)` = top,
      `Pregunta (stem)` = question,
      `Sub-items` = purrr::map_chr(subitems, function(xs) {
        if (is.null(xs) || length(xs) == 0 || all(is.na(xs))) return("")
        xs <- xs[!is.na(xs) & nzchar(xs)]
        if (length(xs) == 0) return("")
        paste0("<ul>", paste0("<li>", htmlEscape(xs), "</li>", collapse = ""), "</ul>")
      }),
      `Opciones` = d$best$options
    )
    datatable(df, options = list(pageLength = 27, scrollX = TRUE), escape = FALSE, rownames = FALSE)
  })
}

shinyApp(ui, server)
