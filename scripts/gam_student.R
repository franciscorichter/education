# Minimal GAM analysis on student questionnaire data
# - Tries to read data from 'data/students.csv' with columns:
#   Y (numeric performance), and several predictors (numeric or categorical)
# - If file is not found, simulates a small dataset
# - Fits a GAM with smooth terms for numeric predictors and parametric terms for factors
# - Saves summary to 'reports/gam_summary.txt' and plots to 'reports/figures/'

suppressPackageStartupMessages({
  if (!requireNamespace("mgcv", quietly = TRUE)) stop("Package 'mgcv' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("readr", quietly = TRUE)) stop("Package 'readr' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' is required.")
})

library(mgcv)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# Paths (assume working directory is repository root)
root <- getwd()
input_csv <- file.path(root, "data", "students.csv")
reports_dir <- file.path(root, "reports")
fig_dir <- file.path(reports_dir, "figures")
out_txt <- file.path(reports_dir, "gam_summary.txt")

# Ensure output dirs exist
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# Load or simulate data
set.seed(123)
if (file.exists(input_csv)) {
  message("Reading data from ", input_csv)
  df <- readr::read_csv(input_csv, show_col_types = FALSE)
} else {
  message("students.csv not found. Simulating a minimal dataset...")
  n <- 400
  sex <- factor(sample(c("F","M"), n, TRUE))
  first_language <- factor(sample(c("Spanish","Indigenous"), n, TRUE, prob = c(0.7, 0.3)))
  absences_30d <- rpois(n, lambda = ifelse(sex=="M", 1.8, 1.5))
  read_hours <- pmax(0, round(rnorm(n, mean = ifelse(first_language=="Spanish", 3, 2), sd = 1.2),1))
  math_anxiety <- rnorm(n, 0, 1)
  teacher_listen_often <- factor(sample(c("Never","Sometimes","Often","Always"), n, TRUE, prob = c(0.1,0.4,0.35,0.15)), ordered = TRUE)
  # True signal
  eta <- 50 +
    5*(sex=="M") +
    6*(first_language=="Spanish") -
    1.2*absences_30d +
    2.0*sqrt(pmax(read_hours,0)) -
    3.0*pmax(math_anxiety,0) +
    c(0,2,3,3.5)[as.integer(teacher_listen_often)]
  Y <- eta + rnorm(n, sd = 5)
  df <- tibble(
    Y, sex, first_language, absences_30d, read_hours, math_anxiety, teacher_listen_often
  )
}

# Select a minimal set of predictors that likely exist or were simulated
preds <- c("sex","first_language","absences_30d","read_hours","math_anxiety","teacher_listen_often")

# Keep only available columns
preds <- preds[preds %in% names(df)]

# Coerce common types
coerce_factor <- intersect(c("sex","first_language","teacher_listen_often"), preds)
for (v in coerce_factor) df[[v]] <- as.factor(df[[v]])

# Basic formula: smooths for numeric vars, parametric for factors
smooth_terms <- preds[sapply(preds, function(v) is.numeric(df[[v]]))]
factor_terms <- setdiff(preds, smooth_terms)

rhs <- c(
  if (length(smooth_terms)) paste0("s(", smooth_terms, ", k=6)") else NULL,
  if (length(factor_terms)) factor_terms else NULL
)

form <- as.formula(paste("Y ~", paste(rhs, collapse = " + ")))
message("Fitting GAM: ", deparse(form))

# Fit with REML for smoothness selection
fit <- mgcv::gam(form, data = df, method = "REML")

# Save summary
sink(out_txt)
cat("GAM formula:\n"); print(form)
cat("\nSummary:\n"); print(summary(fit))
cat("\nANOVA:\n"); print(anova(fit))
cat("\nChecks:\n"); print(gam.check(fit))
sink()
message("Saved summary to ", out_txt)

# Partial effect plots (base plot from mgcv)
png(file.path(fig_dir, "gam_diagnostics.png"), width = 1200, height = 800)
par(mfrow = c(2,2))
gam.check(fit)
dev.off()

# ggplot smooths for numeric terms
num_terms <- smooth_terms
if (length(num_terms)) {
  for (v in num_terms) {
    grid <- data.frame(v = seq(min(df[[v]], na.rm=TRUE), max(df[[v]], na.rm=TRUE), length.out = 100))
    names(grid) <- v
    # Set other vars to typical values
    base <- df[1, , drop=FALSE]
    for (nm in names(df)) {
      if (nm == v) next
      if (is.numeric(df[[nm]])) base[[nm]] <- median(df[[nm]], na.rm=TRUE)
      else base[[nm]] <- levels(as.factor(df[[nm]]))[1]
    }
    newd <- cbind(base[rep(1, 100), setdiff(names(df), "Y")], grid)
    # Reorder columns to match model frame
    newd <- newd[, setdiff(all.vars(form), "Y"), drop=FALSE]
    pred <- predict(fit, newdata = newd, se.fit = TRUE)
    pdat <- data.frame(x = grid[[v]], fit = pred$fit, se = pred$se.fit)
    p <- ggplot(pdat, aes(x, fit)) +
      geom_ribbon(aes(ymin = fit - 2*se, ymax = fit + 2*se), fill = "#c6dbef", alpha = 0.7) +
      geom_line(color = "#08519c", linewidth = 1) +
      labs(x = v, y = "Partial effect on Y", title = paste("GAM partial for", v)) +
      theme_minimal(base_size = 12)
    ggsave(filename = file.path(fig_dir, paste0("gam_partial_", v, ".png")), plot = p, width = 7, height = 4, dpi = 150)
  }
}

message("GAM analysis completed.")
