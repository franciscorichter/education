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
  library(mgcv)
  library(shinyWidgets)
  suppressWarnings(suppressPackageStartupMessages(require(pcalg, quietly = TRUE)))
  suppressWarnings(suppressPackageStartupMessages(require(glasso, quietly = TRUE)))
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

# Ordenar cuestionarios: Estudiante y Familia primero, luego el resto
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
q_priority <- function(x) {
  xl <- tolower(x)
  if (grepl("estudiante", xl)) return(1L)
  if (grepl("familia", xl)) return(2L)
  return(3L)
}
ord_idx <- order(vapply(questionnaire_names, q_priority, integer(1)), questionnaire_names)
questionnaire_ids_ordered <- questionnaire_names[ord_idx]
questionnaire_labels <- vapply(questionnaire_ids_ordered, friendly_title, character(1))
if (any(duplicated(questionnaire_labels))) questionnaire_labels <- make.unique(questionnaire_labels)
am_select_choices <- stats::setNames(questionnaire_ids_ordered, questionnaire_labels)

# Ordenar: Estudiante y Familia primero, luego el resto; crear etiquetas legibles
q_priority <- function(x) {
  xl <- tolower(x)
  if (grepl("estudiante", xl)) return(1L)
  if (grepl("familia", xl)) return(2L)
  return(3L)
}
ord_idx <- order(vapply(questionnaire_names, q_priority, integer(1)), questionnaire_names)
questionnaire_ids_ordered <- questionnaire_names[ord_idx]
questionnaire_labels <- vapply(questionnaire_ids_ordered, friendly_title, character(1))
if (any(duplicated(questionnaire_labels))) questionnaire_labels <- make.unique(questionnaire_labels)
am_select_choices <- stats::setNames(questionnaire_ids_ordered, questionnaire_labels)


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

# (Removed duplicate later blocks and duplicate friendly_title definition)

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
            fluidRow(
              column(8, plotOutput("network_plot", height = "600px")),
              column(4,
                h5("Links to L/M"),
                DTOutput("lm_links_table"),
                div(class = "muted", style = "margin-top:6px;", textOutput("lm_links_info"))
              )
            )
          ),
          tabPanel(
            title = "Questions",
            value = "questions",
            DTOutput("questions_table")
          )
        )
      ),
      # Pestaña de Causal Discovery
      tabPanel(
        title = "🧭 Causal Discovery",
        value = "adv_model",
        h3("Causal Discovery"),
        tabsetPanel(
          id = "am_tabs",
          tabPanel(
            title = "PC",
            value = "am_estimation",
            wellPanel(
              fluidRow(
                column(4,
                  selectInput("am_q", "Questionnaire:", choices = am_select_choices, selected = if (length(questionnaire_ids_ordered)>0) questionnaire_ids_ordered[1] else NULL, width = "100%")
                ),
                column(4,
                  sliderInput("am_alpha", "PC alpha (significance):", min = 0.001, max = 0.2, value = 0.05, step = 0.001, width = "100%")
                ),
                column(4,
                  switchInput(inputId = "am_target_switch", label = "Target: L / M", value = TRUE, onLabel = "L", offLabel = "M")
                )
              ),
              fluidRow(
                column(4,
                  sliderInput("am_mmax", "Max conditioning set size (m.max):", min = 0, max = 6, value = 3, step = 1, width = "100%")
                )
              ),
              h5("Variables"),
              uiOutput("am_var_ui"),
              fluidRow(
                column(8, br(), actionButton("am_run", "Run PC algorithm", class = "btn btn-primary")),
                column(4, br(), textOutput("am_status"))
              )
            ),
            fluidRow(
              column(7, plotOutput("am_dag_plot", height = "580px")),
              column(5,
                h5("Edges into target (L/M)"),
                DTOutput("am_edges_table"),
                div(class = "muted", style = "margin-top:6px;", textOutput("am_edges_info"))
              )
            )
          ),
          tabPanel(
            title = "GAM",
            value = "am_prediction",
            h4("Prediction — Generalized Additive Model (GAM)"),
            wellPanel(
              fluidRow(
                column(4,
                  selectInput("pr_q", "Questionnaire:", choices = am_select_choices, selected = if (length(questionnaire_ids_ordered)>0) questionnaire_ids_ordered[1] else NULL, width = "100%")
                ),
                column(4,
                  switchInput(inputId = "pr_target_switch", label = "Target: L / M", value = TRUE, onLabel = "L", offLabel = "M")
                ),
                column(4, br(), actionButton("pr_run", "Fit GAM", class = "btn btn-primary", width = "100%"))
              ),
              h5("Variables"),
              uiOutput("pr_var_ui"),
              div(class = "muted", style = "margin-top:6px;", textOutput("pr_status"))
            ),
            fluidRow(
              column(6,
                     h5("Model summary"),
                     verbatimTextOutput("pr_summary")
              ),
              column(6,
                     h5("Diagnostics / partial effects"),
                     plotOutput("pr_plot", height = "520px")
              )
            )
          ),
          tabPanel(
            title = "GL",
            value = "am_gl",
            h4("Graphical Lasso (GL) — Sparse Gaussian Graphical Model"),
            wellPanel(
              fluidRow(
                column(4,
                  selectInput("gl_q", "Questionnaire:", choices = am_select_choices, selected = if (length(questionnaire_ids_ordered)>0) questionnaire_ids_ordered[1] else NULL, width = "100%")
                ),
                column(4,
                  sliderInput("gl_lambda", "Regularization (rho):", min = 0.01, max = 0.5, value = 0.10, step = 0.01, width = "100%")
                ),
                column(4,
                  sliderInput("gl_max_edges_plot", "Max edges to plot:", min = 50, max = 2000, value = 300, step = 50, width = "100%"),
                  actionButton("gl_run", "Run Graphical Lasso", class = "btn btn-primary", width = "100%")
                )
              ),
              h5("Variables"),
              uiOutput("gl_var_ui"),
              div(class = "muted", style = "margin-top:6px;", textOutput("gl_status"))
            ),
            fluidRow(
              column(7,
                plotOutput("gl_plot", height = "580px")
              ),
              column(5,
                h5("Edges"),
                DTOutput("gl_edges_table"),
                h5("Summary"),
                verbatimTextOutput("gl_summary")
              )
            )
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

  # ===== Advanced Modeling: Estimation (PC algorithm) =====
  am_choices <- reactive({
    sel <- input$am_q
    if (is.null(sel) || !(sel %in% questionnaire_names)) return(list(vars = character(0), full_data = NULL))
    q <- enla_data$questionnaire_data[[sel]]
    if (is.null(q) || !q$success) return(list(vars = character(0), full_data = NULL))
    fd <- q$full_data
    nms <- colnames(fd)
    if (is.null(nms)) nms <- character(0)
    # Simple and robust p-variable detection: pXX or pXX_YY
    vars <- grep("^p\\d{1,2}(_\\d{1,2})?$", nms, ignore.case = TRUE, value = TRUE)
    if (length(vars) == 0) {
      # fallback: numeric columns excluding IDs and L/M
      num_cols <- names(fd)[vapply(fd, is.numeric, logical(1))]
      exclude <- c("medida500_L","medida500_M","ID_ESTUDIANTE","ID_estudiante","id_estudiante",
                   "cod_mod7","cod_mod7.x","cod_mod7.y","COD_MOD7","anexo","anexo.x","anexo.y","ANEXO",
                   "ID_seccion","ID_seccion.x","ID_seccion.y","ID_SECCION","id_seccion",
                   "ID_ESTUDIANTE.x","ID_ESTUDIANTE.y")
      vars <- setdiff(num_cols, exclude)
    }
    list(vars = vars, full_data = fd)
  })

  output$am_var_ui <- renderUI({
    ch <- am_choices(); vars <- ch$vars
    tagList(
      fluidRow(
        column(6, textInput("am_filter", "Filter items (regex):", value = "", placeholder = "e.g., ^p2[0-9]_|_01$")),
        column(3, br(), actionButton("am_sel_all", "Select all", class = "btn btn-light btn-sm", width = "100%")),
        column(3, br(), actionButton("am_sel_none", "Clear all", class = "btn btn-light btn-sm", width = "100%"))
      ),
      div(class = "muted", if (length(vars) > 0) paste0(length(vars), " variables detected") else "No variables detected"),
      div(style = "max-height: 260px; overflow-y: auto; border: 1px solid #e5e7eb; padding: 8px;",
          checkboxGroupInput("am_vars", NULL, choices = vars, selected = character(0), inline = FALSE))
    )
  })

  observeEvent(input$am_sel_all, {
    ch <- am_choices(); vars <- ch$vars
    if (!is.null(input$am_filter) && nzchar(input$am_filter)) {
      keep <- tryCatch(grepl(input$am_filter, vars), error = function(e) rep(TRUE, length(vars)))
      vars <- vars[keep]
    }
    updateCheckboxGroupInput(session, "am_vars", selected = vars)
  })

  observeEvent(input$am_sel_none, {
    updateCheckboxGroupInput(session, "am_vars", selected = character(0))
  })

  observeEvent(input$am_filter, {
    ch <- am_choices(); vars <- ch$vars
    if (!is.null(input$am_filter) && nzchar(input$am_filter)) {
      keep <- tryCatch(grepl(input$am_filter, vars), error = function(e) rep(TRUE, length(vars)))
      vars <- vars[keep]
    }
    updateCheckboxGroupInput(session, "am_vars", choices = vars)
  }, ignoreInit = TRUE)

  am_status <- reactiveVal("")

  am_pc_result <- eventReactive(input$am_run, {
    if (!requireNamespace("pcalg", quietly = TRUE)) {
      am_status("Package 'pcalg' is required. Please install.packages('pcalg').")
      return(list(error = "Package 'pcalg' not installed"))
    }
    ch <- am_choices(); vars <- input$am_vars
    if (length(vars) < 2 || is.null(ch$full_data)) { am_status(""); return(list(error = "Select at least 2 variables")) }
    X <- dplyr::select(ch$full_data, dplyr::all_of(vars))
    target <- if (isTRUE(input$am_target_switch)) "L" else "M"
    tcol <- if (target == "L") "medida500_L" else "medida500_M"
    if (!(tcol %in% names(ch$full_data))) return(list(error = paste0("Target column ", tcol, " not found in full_data")))
    X[[target]] <- suppressWarnings(as.numeric(ch$full_data[[tcol]]))

    # Progress bar and notification
    am_status("Preparing data...")
    prog <- shiny::Progress$new(min = 0, max = 1)
    on.exit(prog$close(), add = TRUE)
    notif_id <- showNotification("Running PC algorithm...", type = "message", duration = NULL)
    on.exit(removeNotification(notif_id), add = TRUE)

    prog$set(value = 0.2, message = "Imputing missing values")
    X <- X[rowSums(is.na(X)) < ncol(X), , drop = FALSE]
    if (nrow(X) < 10) return(list(error = "Not enough rows after NA filtering"))
    X[] <- lapply(X, function(col) suppressWarnings(as.numeric(col)))
    for (j in seq_len(ncol(X))) {
      m <- mean(X[[j]], na.rm = TRUE); if (is.na(m)) m <- 0
      idx <- which(is.na(X[[j]])); if (length(idx) > 0) X[[j]][idx] <- m
    }

    prog$set(value = 0.5, message = "Computing correlation matrix")
    suffStat <- list(C = stats::cor(X), n = nrow(X))
    alpha <- if (is.null(input$am_alpha)) 0.05 else input$am_alpha
    t0 <- Sys.time(); am_status("Running PC algorithm...")

    prog$set(value = 0.8, message = "PC search")
    mm <- if (is.null(input$am_mmax) || is.na(input$am_mmax)) Inf else as.integer(input$am_mmax)
    g <- tryCatch(pcalg::pc(suffStat = suffStat, indepTest = pcalg::gaussCItest,
                             alpha = alpha, labels = colnames(X), verbose = FALSE, m.max = mm), error = function(e) e)
    if (inherits(g, "error")) return(list(error = paste("PC error:", g$message)))
    amat <- as(g@graph, "matrix")
    edges <- which(amat != 0, arr.ind = TRUE)
    edge_df <- tibble::tibble(from = rownames(amat)[edges[,1]], to = colnames(amat)[edges[,2]])
    elapsed <- difftime(Sys.time(), t0, units = "secs")
    prog$set(value = 1, message = "Done")
    am_status(paste0("Done in ", round(as.numeric(elapsed), 2), "s (n=", nrow(X), ", p=", ncol(X), ", m.max=", ifelse(is.finite(mm), mm, NA), ")"))
    list(graph = g, edges = edge_df)
  })

  output$am_dag_plot <- renderPlot({
    res <- am_pc_result(); if (is.null(res)) return(); if (!is.null(res$error)) { plot.new(); title(res$error); return() }
    ig <- igraph::graph_from_adjacency_matrix(as(res$graph@graph, "matrix"), mode = "directed")
    cols <- rep("#4E79A7", igraph::vcount(ig)); tgt <- if (isTRUE(input$am_target_switch)) "L" else "M"
    idx <- which(igraph::V(ig)$name == tgt); if (length(idx) > 0) cols[idx] <- "#E15759"
    plot(ig, vertex.size = 18, vertex.label.cex = 0.8, edge.arrow.size = 0.4, vertex.color = cols, layout = igraph::layout_with_fr)
  })

  output$am_edges_table <- renderDT({
    res <- am_pc_result()
    if (is.null(res)) return(datatable(data.frame()))
    if (!is.null(res$error)) return(datatable(data.frame(Info = res$error), rownames = FALSE))
    tgt <- if (isTRUE(input$am_target_switch)) "L" else "M"
    df <- res$edges
    into_tgt <- dplyr::filter(df, .data$to == tgt)
    datatable(into_tgt, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$am_edges_info <- renderText({
    res <- am_pc_result(); if (is.null(res) || !is.null(res$error)) return("")
    tgt <- if (isTRUE(input$am_target_switch)) "L" else "M"
    n_all <- nrow(res$edges)
    n_tgt <- sum(res$edges$to == tgt)
    paste0("Edges total: ", n_all, " | into ", tgt, ": ", n_tgt)
  })

  output$am_status <- renderText({ am_status() })

  # ===== Advanced Modeling: Prediction (GAM) =====
  pr_status <- reactiveVal("")

  pr_choices <- reactive({
    sel <- input$pr_q
    if (is.null(sel) || !(sel %in% questionnaire_names)) return(list(vars = character(0), full_data = NULL))
    q <- enla_data$questionnaire_data[[sel]]
    if (is.null(q) || !q$success) return(list(vars = character(0), full_data = NULL))
    fd <- q$full_data
    nms <- colnames(fd)
    if (is.null(nms)) nms <- character(0)
    vars <- grep("^p\\d{1,2}(_\\d{1,2})?$", nms, ignore.case = TRUE, value = TRUE)
    if (length(vars) == 0) {
      num_cols <- names(fd)[vapply(fd, is.numeric, logical(1))]
      exclude <- c("medida500_L","medida500_M","ID_ESTUDIANTE","ID_estudiante","id_estudiante",
                   "cod_mod7","cod_mod7.x","cod_mod7.y","COD_MOD7","anexo","anexo.x","anexo.y","ANEXO",
                   "ID_seccion","ID_seccion.x","ID_seccion.y","ID_SECCION","id_seccion",
                   "ID_ESTUDIANTE.x","ID_ESTUDIANTE.y")
      vars <- setdiff(num_cols, exclude)
    }
    list(vars = vars, full_data = fd)
  })

  output$pr_var_ui <- renderUI({
    ch <- pr_choices(); vars <- ch$vars
    tagList(
      fluidRow(
        column(6, textInput("pr_filter", "Filter items (regex):", value = "", placeholder = "e.g., ^p2[0-9]_|_01$")),
        column(3, br(), actionButton("pr_sel_all", "Select all", class = "btn btn-light btn-sm", width = "100%")),
        column(3, br(), actionButton("pr_sel_none", "Clear all", class = "btn btn-light btn-sm", width = "100%"))
      ),
      div(class = "muted", if (length(vars) > 0) paste0(length(vars), " variables detected") else "No variables detected"),
      div(style = "max-height: 260px; overflow-y: auto; border: 1px solid #e5e7eb; padding: 8px;",
          checkboxGroupInput("pr_vars", NULL, choices = vars, selected = character(0), inline = FALSE))
    )
  })

  observeEvent(input$pr_sel_all, {
    ch <- pr_choices(); vars <- ch$vars
    if (!is.null(input$pr_filter) && nzchar(input$pr_filter)) {
      keep <- tryCatch(grepl(input$pr_filter, vars), error = function(e) rep(TRUE, length(vars)))
      vars <- vars[keep]
    }
    updateCheckboxGroupInput(session, "pr_vars", selected = vars)
  })

  observeEvent(input$pr_sel_none, {
    updateCheckboxGroupInput(session, "pr_vars", selected = character(0))
  })

  observeEvent(input$pr_filter, {
    ch <- pr_choices(); vars <- ch$vars
    if (!is.null(input$pr_filter) && nzchar(input$pr_filter)) {
      keep <- tryCatch(grepl(input$pr_filter, vars), error = function(e) rep(TRUE, length(vars)))
      vars <- vars[keep]
    }
    updateCheckboxGroupInput(session, "pr_vars", choices = vars)
  }, ignoreInit = TRUE)

  pr_result <- eventReactive(input$pr_run, {
    ch <- pr_choices()
    vars <- input$pr_vars
    # Auto-select a small set if none chosen
    if (length(vars) < 1) {
      if (length(ch$vars) == 0) {
        pr_status("No predictor variables found for this questionnaire (expecting pXX or numeric columns).")
        showNotification("No predictor variables found.", type = "error")
        return(NULL)
      }
      vars <- head(ch$vars, 10)
      pr_status(sprintf("No variables selected. Auto-selected %d predictors.", length(vars)))
    } else {
      pr_status(sprintf("Selected %d predictors. Preparing data...", length(vars)))
    }
    if (is.null(ch$full_data)) { pr_status("Questionnaire full_data is not available."); showNotification("No full_data available for selected questionnaire.", type = "error"); return(NULL) }

    fd <- ch$full_data
    target <- if (isTRUE(input$pr_target_switch)) "L" else "M"
    tcol <- if (target == "L") "medida500_L" else "medida500_M"
    if (!(tcol %in% names(fd))) { pr_status(paste0("Target ", tcol, " not found in full_data.")); showNotification(sprintf("Target %s not found.", tcol), type = "error"); return(NULL) }

    notif_id <- showNotification("Fitting GAM...", type = "message", duration = NULL)
    on.exit(removeNotification(notif_id), add = TRUE)
    prog <- shiny::Progress$new(min = 0, max = 1)
    on.exit(prog$close(), add = TRUE)

    prog$set(value = 0.10, message = "Building modeling frame")
    keep_cols <- intersect(c(vars, tcol), names(fd))
    if (length(keep_cols) < 2) { pr_status("Could not assemble modeling frame (no overlap of selected vars with data)."); showNotification("No overlap of selected variables with data.", type = "error"); return(NULL) }
    df <- fd[, keep_cols, drop = FALSE]
    n0 <- nrow(df)
    df <- df[rowSums(is.na(df)) < ncol(df), , drop = FALSE]
    pr_status(sprintf("Dropped %d rows with all-NA predictors/target. Remaining: %d", n0 - nrow(df), nrow(df)))
    if (nrow(df) < 20) { pr_status("Not enough rows after NA filtering"); return(NULL) }

    prog$set(value = 0.30, message = "Coercing types")
    df[[tcol]] <- suppressWarnings(as.numeric(df[[tcol]]))
    names(df)[names(df) == tcol] <- "Y"
    # Determine numeric vs factor predictors, adapt k by unique levels
    predictors <- setdiff(names(df), "Y")
    is_num <- vapply(df[predictors], function(x) is.numeric(suppressWarnings(as.numeric(x))), logical(1))
    num_vars0 <- names(is_num)[is_num]
    fac_vars <- setdiff(predictors, num_vars0)
    # Coerce baseline factors
    for (v in fac_vars) df[[v]] <- as.factor(df[[v]])

    # For numeric variables, compute unique counts and adaptively set k or coerce to factor if too few uniques
    adj_s_terms <- c()
    coerced_to_factor <- c()
    kept_numeric <- c()
    for (v in num_vars0) {
      xv <- df[[v]]
      u <- length(unique(xv[!is.na(xv)]))
      if (u < 4) {
        # treat as factor
        df[[v]] <- as.factor(xv)
        fac_vars <- c(fac_vars, v)
        coerced_to_factor <- c(coerced_to_factor, sprintf("%s(u=%d)", v, u))
      } else {
        # k must be < unique and >=3
        k_i <- min(6L, max(3L, u - 1L))
        adj_s_terms <- c(adj_s_terms, sprintf("s(%s, k=%d)", v, k_i))
        kept_numeric <- c(kept_numeric, sprintf("%s(u=%d,k=%d)", v, u, k_i))
      }
    }
    pr_status(sprintf("Numeric (kept as smooth): %d | Coerced to factor: %d | Factor (original): %d",
                      length(kept_numeric), length(coerced_to_factor), length(setdiff(fac_vars, names(is_num)[is_num]))))

    prog$set(value = 0.55, message = "Building formula")
    rhs <- c(if (length(adj_s_terms)) adj_s_terms else NULL,
             if (length(fac_vars)) fac_vars else NULL)
    if (!length(rhs)) { pr_status("No valid predictors after preprocessing."); return(NULL) }
    form <- as.formula(paste("Y ~", paste(rhs, collapse = " + ")))
    pr_status(paste("Formula:", deparse(form)))

    prog$set(value = 0.80, message = "Fitting model (REML)")
    t0 <- Sys.time()
    fit <- tryCatch(mgcv::gam(form, data = df, method = "REML"), error = function(e) e)
    if (inherits(fit, "error")) { pr_status(paste("Model error:", fit$message)); return(NULL) }
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    prog$set(value = 1.0, message = "Done")
    pr_status(sprintf("Done in %.2fs. n=%d, p=%d. Use the tabs below for summary/plots.", elapsed, nrow(df), length(vars)))
    # Return the vector of smooth terms for plotting count and elapsed time
    list(fit = fit, form = form, num_vars = adj_s_terms, elapsed = elapsed, n = nrow(df), p = length(vars))
  })

  output$pr_status <- renderText({ pr_status() })

  output$pr_summary <- renderPrint({
    res <- pr_result(); if (is.null(res)) return(cat("No model fitted yet. Click 'Fit GAM'."))
    cat("GAM formula:\n"); print(res$form)
    if (!is.null(res$elapsed)) cat(sprintf("\nCompute time: %.2f s  |  n = %d  p = %d\n", res$elapsed, res$n %||% NA_integer_, res$p %||% NA_integer_))
    cat("\nSummary:\n"); print(summary(res$fit))
    cat("\nBasis dimension check (gam.check):\n"); print(try(gam.check(res$fit), silent = TRUE))
  })

  output$pr_plot <- renderPlot({
    res <- pr_result(); if (is.null(res)) return()
    fit <- res$fit
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(mfrow = c(2,2))
    try(gam.check(fit), silent = TRUE)
    if (length(res$num_vars) > 0) {
      par(mfrow = c(1, min(3, length(res$num_vars))))
      for (i in seq_along(res$num_vars)) {
        try(plot(fit, select = i, shade = TRUE, shade.col = "#c6dbef", col = "#08519c", seWithMean = TRUE), silent = TRUE)
      }
    }
  })

  # ===== Advanced Modeling: Graphical Lasso (GL) =====
  gl_status <- reactiveVal("")

  gl_choices <- reactive({
    sel <- input$gl_q
    if (is.null(sel) || !(sel %in% questionnaire_names)) return(list(vars = character(0), data = NULL))
    q <- enla_data$questionnaire_data[[sel]]
    if (is.null(q) || !q$success) return(list(vars = character(0), data = NULL))
    X <- q$data
    nms <- colnames(X); if (is.null(nms)) nms <- character(0)
    vars <- grep("^p\\d{1,2}(_\\d{1,2})?$", nms, ignore.case = TRUE, value = TRUE)
    if (length(vars) == 0) {
      num_cols <- names(X)[vapply(X, is.numeric, logical(1))]
      vars <- num_cols
    }
    list(vars = vars, data = X)
  })

  output$gl_var_ui <- renderUI({
    ch <- gl_choices(); vars <- ch$vars
    tagList(
      fluidRow(
        column(6, textInput("gl_filter", "Filter items (regex):", value = "", placeholder = "e.g., ^p2[0-9]_|_01$")),
        column(3, br(), actionButton("gl_sel_all", "Select all", class = "btn btn-light btn-sm", width = "100%")),
        column(3, br(), actionButton("gl_sel_none", "Clear all", class = "btn btn-light btn-sm", width = "100%"))
      ),
      div(class = "muted", if (length(vars) > 0) paste0(length(vars), " variables detected") else "No variables detected"),
      div(style = "max-height: 260px; overflow-y: auto; border: 1px solid #e5e7eb; padding: 8px;",
          checkboxGroupInput("gl_vars", NULL, choices = vars, selected = character(0), inline = FALSE))
    )
  })

  observeEvent(input$gl_sel_all, {
    ch <- gl_choices(); vars <- ch$vars
    if (!is.null(input$gl_filter) && nzchar(input$gl_filter)) {
      keep <- tryCatch(grepl(input$gl_filter, vars), error = function(e) rep(TRUE, length(vars)))
      vars <- vars[keep]
    }
    updateCheckboxGroupInput(session, "gl_vars", selected = vars)
  })

  observeEvent(input$gl_sel_none, {
    updateCheckboxGroupInput(session, "gl_vars", selected = character(0))
  })

  observeEvent(input$gl_filter, {
    ch <- gl_choices(); vars <- ch$vars
    if (!is.null(input$gl_filter) && nzchar(input$gl_filter)) {
      keep <- tryCatch(grepl(input$gl_filter, vars), error = function(e) rep(TRUE, length(vars)))
      vars <- vars[keep]
    }
    updateCheckboxGroupInput(session, "gl_vars", choices = vars)
  }, ignoreInit = TRUE)

  observeEvent(input$gl_q, {
    # When questionnaire changes, refresh choices and clear status/selection
    gl_status("")
    ch <- gl_choices(); vars <- ch$vars
    updateCheckboxGroupInput(session, "gl_vars", choices = vars, selected = character(0))
  }, ignoreInit = TRUE)

  gl_result <- eventReactive(input$gl_run, {
    if (!requireNamespace("glasso", quietly = TRUE)) {
      gl_status("Package 'glasso' is required. Please install.packages('glasso').")
      return(NULL)
    }
    ch <- gl_choices(); vars <- input$gl_vars
    # Auto-select a small set if none chosen
    if (length(vars) < 2) {
      if (length(ch$vars) < 2) { gl_status("No suitable variables found in this questionnaire."); return(NULL) }
      vars <- head(ch$vars, min(15, length(ch$vars)))
      updateCheckboxGroupInput(session, "gl_vars", selected = vars)
      gl_status(sprintf("No variables selected. Auto-selected %d variables.", length(vars)))
    } else {
      gl_status(sprintf("Selected %d variables. Preparing data...", length(vars)))
    }
    if (is.null(ch$data)) { gl_status("Questionnaire data not available."); return(NULL) }
    X <- dplyr::select(ch$data, dplyr::all_of(vars))
    # Remove rows with all NA; impute remaining NA with column means
    X <- X[rowSums(is.na(X)) < ncol(X), , drop = FALSE]
    if (nrow(X) < 10) { gl_status("Not enough rows after NA filtering"); return(NULL) }
    X[] <- lapply(X, function(col) suppressWarnings(as.numeric(col)))
    for (j in seq_len(ncol(X))) {
      m <- mean(X[[j]], na.rm = TRUE); if (is.na(m)) m <- 0
      idx <- which(is.na(X[[j]])); if (length(idx) > 0) X[[j]][idx] <- m
    }
    # Filter zero-variance columns
    sds <- vapply(X, function(x) sd(x, na.rm = TRUE), numeric(1))
    keep <- names(sds)[sds > 0 & !is.na(sds)]
    X <- dplyr::select(X, dplyr::all_of(keep))
    if (ncol(X) < 2) { gl_status("All selected variables have zero variance"); return(NULL) }

    rho <- if (is.null(input$gl_lambda)) 0.10 else as.numeric(input$gl_lambda)
    gl_status(sprintf("Running GL on n=%d, p=%d (rho=%.3f)...", nrow(X), ncol(X), rho))
    notif_id <- showNotification("Running Graphical Lasso...", type = "message", duration = NULL)
    on.exit(removeNotification(notif_id), add = TRUE)
    prog <- shiny::Progress$new(min = 0, max = 1)
    on.exit(prog$close(), add = TRUE)
    prog$set(value = 0.3, message = "Computing covariance")
    S <- stats::cov(X)
    prog$set(value = 0.6, message = "Solving glasso")
    fit <- tryCatch(glasso::glasso(S, rho = rho), error = function(e) e)
    if (inherits(fit, "error")) { gl_status(paste("glasso error:", fit$message)); return(NULL) }
    Theta <- fit$wi
    # Build adjacency from non-zero off-diagonals in precision matrix
    adj <- (abs(Theta) > .Machine$double.eps)
    diag(adj) <- FALSE
    edges <- which(adj, arr.ind = TRUE)
    if (nrow(edges) > 0) edges <- edges[edges[,1] < edges[,2], , drop = FALSE]
    
    # Extract weights properly using matrix indexing
    if (nrow(edges) > 0) {
      weights <- numeric(nrow(edges))
      for (i in seq_len(nrow(edges))) {
        weights[i] <- abs(Theta[edges[i, 1], edges[i, 2]])
      }
      edge_df <- data.frame(
        from = colnames(Theta)[edges[,1]],
        to   = colnames(Theta)[edges[,2]],
        weight = weights,
        stringsAsFactors = FALSE
      )
    } else {
      edge_df <- data.frame(from = character(0), to = character(0), weight = numeric(0), stringsAsFactors = FALSE)
    }
    
    gl_status(sprintf("Done. Edges: %d", nrow(edge_df)))
    list(Theta = Theta, edges = edge_df)
  })

  output$gl_status <- renderText({ gl_status() })

  output$gl_plot <- renderPlot({
    res <- gl_result()
    if (is.null(res)) { plot.new(); title("GL: no result yet. Click 'Run Graphical Lasso'."); return() }
    Theta <- res$Theta
    node_names <- colnames(Theta)
    # Group by top code if pattern present
    groups <- stringr::str_match(node_names, "^(p\\d{2})")[,2]
    groups[is.na(groups)] <- node_names[is.na(groups)]
    df_nodes <- data.frame(name = node_names, group = groups, stringsAsFactors = FALSE)
    adj <- (abs(Theta) > .Machine$double.eps); diag(adj) <- FALSE
    edges <- which(adj, arr.ind = TRUE)
    if (nrow(edges) > 0) edges <- edges[edges[,1] < edges[,2], , drop = FALSE]
    
    # Extract weights properly using matrix indexing
    if (nrow(edges) > 0) {
      weights <- numeric(nrow(edges))
      for (i in seq_len(nrow(edges))) {
        weights[i] <- abs(Theta[edges[i, 1], edges[i, 2]])
      }
      df_edges <- data.frame(
        from = node_names[edges[,1]],
        to   = node_names[edges[,2]],
        weight = weights,
        stringsAsFactors = FALSE
      )
    } else {
      df_edges <- data.frame(from = character(0), to = character(0), weight = numeric(0), stringsAsFactors = FALSE)
    }
    
    if (nrow(df_edges) == 0) { plot.new(); title("No edges selected by GL"); return() }
    # Limit edges to top weights for plotting to avoid overplotting
    max_e <- if (is.null(input$gl_max_edges_plot)) 300 else as.integer(input$gl_max_edges_plot)
    df_edges <- dplyr::arrange(df_edges, dplyr::desc(.data$weight))
    if (nrow(df_edges) > max_e) df_edges <- df_edges[seq_len(max_e), , drop = FALSE]
    # Keep only nodes that appear in the selected edge set for plotting
    used_nodes <- sort(unique(c(df_edges$from, df_edges$to)))
    df_nodes_plot <- dplyr::filter(df_nodes, .data$name %in% used_nodes)
    G <- igraph::graph_from_data_frame(df_edges, directed = FALSE, vertices = df_nodes_plot)
    base_pal <- RColorBrewer::brewer.pal(8, "Set2")
    palette <- grDevices::colorRampPalette(base_pal)(max(8, length(unique(df_nodes_plot$group))))
    col_map <- setNames(rep(palette, length.out = length(unique(df_nodes_plot$group))), sort(unique(df_nodes_plot$group)))
    V(G)$color <- col_map[V(G)$group]
    E(G)$weight <- df_edges$weight
    set.seed(123)
    coords <- igraph::layout_with_fr(G)
    try({
      plot(G,
           layout = coords,
           vertex.size = 10,
           vertex.label.cex = 0.7,
           edge.width = scales::rescale(E(G)$weight, to = c(0.5, 3)),
           edge.color = rgb(0.2,0.2,0.2,0.3),
           main = sprintf("Graphical Lasso (rho=%.2f) — plotted edges: %d", ifelse(is.null(input$gl_lambda), 0.10, input$gl_lambda), igraph::ecount(G)))
      legend("topleft", legend = names(col_map), col = unname(col_map), pch = 16, bty = "n", ncol = 2, title = "Group")
    }, silent = TRUE)
  })

  output$gl_summary <- renderPrint({
    res <- gl_result()
    if (is.null(res)) { cat("No GL result yet."); return() }
    
    Theta <- res$Theta
    edges <- res$edges
    p <- ncol(Theta)
    m <- nrow(edges)
    
    cat(sprintf("Nodes: %d\nEdges: %d\n", p, m))
    
    if (m > 0 && p > 0) {
      # Build graph for degree calculation
      edge_df <- data.frame(
        from = as.character(edges$from),
        to = as.character(edges$to),
        stringsAsFactors = FALSE
      )
      node_df <- data.frame(name = colnames(Theta), stringsAsFactors = FALSE)
      g_summary <- tryCatch(
        igraph::graph_from_data_frame(edge_df, directed = FALSE, vertices = node_df),
        error = function(e) NULL
      )
      
      if (!is.null(g_summary)) {
        deg <- sort(igraph::degree(g_summary), decreasing = TRUE)
        dens <- igraph::graph.density(g_summary, loops = FALSE)
        cat(sprintf("Density: %.4f\n", dens))
        cat("\nTop degrees:\n")
        print(head(deg, 15))
      }
    }
  })

  output$gl_edges_table <- renderDT({
    res <- gl_result()
    if (is.null(res)) {
      return(datatable(data.frame(Info = "No result yet", stringsAsFactors = FALSE), rownames = FALSE))
    }
    
    edge_tbl <- res$edges
    if (is.null(edge_tbl) || nrow(edge_tbl) == 0) {
      return(datatable(data.frame(Info = "No edges", stringsAsFactors = FALSE), rownames = FALSE))
    }
    
    # Convert tibble to data.frame explicitly
    df <- data.frame(
      from = as.character(edge_tbl$from),
      to = as.character(edge_tbl$to),
      weight = as.numeric(edge_tbl$weight),
      stringsAsFactors = FALSE
    )
    
    # Sort by weight descending
    df <- df[order(-df$weight), ]
    
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
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
      private <- c("privado","privada","particular","parroquial","no estatal")
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

  output$lm_links_table <- renderDT({
    nd <- network_data()
    if (is.null(nd) || is.null(nd$cor_df)) return(datatable(data.frame(Info = "Build a network first"), rownames = FALSE))
    if (!isTRUE(input$include_lm)) return(datatable(data.frame(Info = "L/M nodes not included"), rownames = FALSE))
    thr <- if (is.null(input$thr)) 0 else input$thr
    df <- nd$cor_df
    has_LM <- any(df$var1 %in% c("L","M") | df$var2 %in% c("L","M"))
    if (!has_LM) return(datatable(data.frame(Info = "No L/M variables present in network data"), rownames = FALSE))
    # Identify correlation column and coerce to numeric
    rcol <- if ("r" %in% names(df)) "r" else if ("cor" %in% names(df)) "cor" else NULL
    if (is.null(rcol)) return(datatable(data.frame(Info = "Correlation column not found"), rownames = FALSE))
    rnum <- suppressWarnings(as.numeric(df[[rcol]]))
    is_LM <- (df$var1 %in% c("L","M")) | (df$var2 %in% c("L","M"))
    keep <- is_LM & !is.na(rnum) & abs(rnum) >= thr
    df2 <- df[keep, , drop = FALSE]
    if (nrow(df2) == 0) return(datatable(data.frame(Info = "No links above threshold"), rownames = FALSE))
    target <- ifelse(df2$var1 %in% c("L","M"), df2$var1, df2$var2)
    node <- ifelse(df2$var1 %in% c("L","M"), df2$var2, df2$var1)
    out <- tibble::tibble(
      target = target,
      node = node,
      r = suppressWarnings(as.numeric(df2[[rcol]]))
    )
    out$abs_r <- abs(out$r)
    out <- dplyr::arrange(out, dplyr::desc(abs_r))
    datatable(out, options = list(pageLength = 10, order = list(list(3, 'desc'))), rownames = FALSE)
  })

  output$lm_links_info <- renderText({
    nd <- network_data()
    if (is.null(nd) || is.null(nd$cor_df)) return("")
    df <- nd$cor_df
    n_edges <- nrow(df)
    n_with_L <- sum(df$var1 == "L" | df$var2 == "L")
    n_with_M <- sum(df$var1 == "M" | df$var2 == "M")
    paste0("Edges: ", n_edges, " | with L: ", n_with_L, " | with M: ", n_with_M)
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
      if (nrow(fd) == nrow(X)) {
        if ("medida500_L" %in% names(fd)) {
          X$L <- suppressWarnings(as.numeric(fd$medida500_L))
        }
        if ("medida500_M" %in% names(fd)) {
          X$M <- suppressWarnings(as.numeric(fd$medida500_M))
        }
      } # else: skip appending L/M to avoid row-size mismatch
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
         vars = item_cols, items_detected = items_detected,
         cor_df = cor_df)
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
