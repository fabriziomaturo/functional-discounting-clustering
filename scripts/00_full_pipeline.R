###############################################################################
######################## DESCRIPTIVE FIGURES FOR THE PAPER ####################
###############################################################################

##############################
# 1. Reproducibility
##############################
set.seed(1234)

##############################
# 2. Packages
##############################
required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "fda",
  "fda.usc"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    paste(
      "Install the following packages before running the script:",
      paste(missing_packages, collapse = ", ")
    )
  )
}

##############################
# 3. Helper functions
##############################
get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_path <- sub(file_arg, "", cmd_args[grep(file_arg, cmd_args)])
  
  if (length(file_path) > 0) {
    return(dirname(normalizePath(file_path[1])))
  }
  
  return(getwd())
}

save_plot_dual <- function(plot_object, filename_no_ext, script_dir, width = 10, height = 7) {
  ggplot2::ggsave(
    filename = file.path(script_dir, paste0(filename_no_ext, ".png")),
    plot = plot_object,
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    bg = "white"
  )
  
  ggplot2::ggsave(
    filename = file.path(script_dir, paste0(filename_no_ext, ".pdf")),
    plot = plot_object,
    width = width,
    height = height,
    units = "in",
    device = grDevices::cairo_pdf,
    bg = "white"
  )
}

save_base_plot_dual <- function(filename_no_ext, script_dir, plot_fun, width_png = 2400, height_png = 1600,
                                width_pdf = 10, height_pdf = 7, res_png = 300) {
  grDevices::png(
    filename = file.path(script_dir, paste0(filename_no_ext, ".png")),
    width = width_png,
    height = height_png,
    res = res_png,
    bg = "white"
  )
  plot_fun()
  grDevices::dev.off()
  
  grDevices::cairo_pdf(
    filename = file.path(script_dir, paste0(filename_no_ext, ".pdf")),
    width = width_pdf,
    height = height_pdf,
    bg = "white"
  )
  plot_fun()
  grDevices::dev.off()
}

build_group_from_available_columns <- function(df) {
  if ("Primotratto" %in% names(df)) {
    group_vec <- dplyr::case_when(
      df$Primotratto == "ARTIGIANO" ~ "ARTISAN",
      df$Primotratto == "GUARDIANO" ~ "GUARDIAN",
      df$Primotratto == "IDEALISTA" ~ "IDEALIST",
      df$Primotratto == "RAZIONALE" ~ "RATIONAL",
      TRUE ~ NA_character_
    )
    return(group_vec)
  }
  
  if ("g" %in% names(df)) {
    g_num <- suppressWarnings(as.numeric(df$g))
    if (!all(is.na(g_num))) {
      levels_map <- c("ARTISAN", "GUARDIAN", "IDEALIST", "RATIONAL")
      group_vec <- levels_map[g_num]
      group_vec[g_num < 1 | g_num > 4] <- NA_character_
      return(group_vec)
    }
  }
  
  stop("Neither 'Primotratto' nor a usable numeric column 'g' was found in the Excel file.")
}

##############################
# 4. Input settings
##############################
script_dir <- get_script_dir()
input_file <- file.path(script_dir, "data.xlsx")

if (!file.exists(input_file)) {
  stop("File 'data.xlsx' was not found in the same folder as the R script.")
}

time_labels <- c("D0", "D2", "D4", "D7", "D10", "D14", "D20", "D30", "D45", "D60", "D90")
time_days <- c(0, 2, 4, 7, 10, 14, 20, 30, 45, 60, 90)

##############################
# 5. Read data
##############################
raw_data <- readxl::read_excel(input_file)

required_discount_columns <- c("f(0)", "f(2)", "f(4)", "f(7)", "f(10)", "f(14)", "f(20)", "f(30)", "f(45)", "f(60)", "f(90)")
missing_discount_columns <- setdiff(required_discount_columns, names(raw_data))

if (length(missing_discount_columns) > 0) {
  stop(
    paste(
      "The following discount columns are missing from data.xlsx:",
      paste(missing_discount_columns, collapse = ", ")
    )
  )
}

##############################
# 6. Build analysis dataset
##############################
data <- dplyr::transmute(
  raw_data,
  AGE = if ("AGE" %in% names(raw_data)) AGE else NA_real_,
  GENDER = if ("GENDER" %in% names(raw_data)) as.character(GENDER) else NA_character_,
  Group = build_group_from_available_columns(raw_data),
  D0  = as.numeric(`f(0)`),
  D2  = as.numeric(`f(2)`),
  D4  = as.numeric(`f(4)`),
  D7  = as.numeric(`f(7)`),
  D10 = as.numeric(`f(10)`),
  D14 = as.numeric(`f(14)`),
  D20 = as.numeric(`f(20)`),
  D30 = as.numeric(`f(30)`),
  D45 = as.numeric(`f(45)`),
  D60 = as.numeric(`f(60)`),
  D90 = as.numeric(`f(90)`)
)

data <- dplyr::mutate(
  data,
  Group = factor(Group, levels = c("ARTISAN", "GUARDIAN", "IDEALIST", "RATIONAL"))
)

##############################
# 7. Keep complete cases for figures and FDA
##############################
analysis_data <- dplyr::filter(
  data,
  !is.na(Group),
  dplyr::if_all(dplyr::all_of(time_labels), ~ !is.na(.x))
)

if (nrow(analysis_data) == 0) {
  stop("No complete cases are available after filtering.")
}

##############################
# 8. Core objects used later
#    These were implicit or missing in the original code.
#    Here they are created explicitly.
##############################
dd <- analysis_data[, time_labels, drop = FALSE]
dd_matrix <- as.matrix(dd)
storage.mode(dd_matrix) <- "numeric"

x_labels <- time_labels
arg <- seq_len(ncol(dd_matrix))
range_arg <- range(arg)

group_factor <- analysis_data$Group
g <- factor(group_factor, levels = c("ARTISAN", "GUARDIAN", "IDEALIST", "RATIONAL"))

color_map <- c(
  "ARTISAN" = "black",
  "GUARDIAN" = "red",
  "IDEALIST" = "blue",
  "RATIONAL" = "green4"
)
color <- unname(color_map[as.character(g)])
tipo <- rep(1, nrow(dd_matrix))

f <- fda.usc::fdata(
  dd_matrix,
  argvals = arg,
  rangeval = range_arg,
  names = list(main = "Functions", xlab = "Days", ylab = "f(t)")
)

##############################
# 9. Long dataset for boxplots
##############################
discount_long <- tidyr::pivot_longer(
  data = analysis_data,
  cols = dplyr::all_of(time_labels),
  names_to = "Day",
  values_to = "f_value"
)

discount_long <- dplyr::mutate(
  discount_long,
  Day = factor(Day, levels = time_labels)
)

##############################
# 10. Common plotting theme
##############################
paper_theme <- ggplot2::theme_bw(base_size = 16) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
    axis.title = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5),
    legend.title = ggplot2::element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank()
  )

group_colors <- c(
  "ARTISAN"  = "salmon",
  "GUARDIAN" = "yellowgreen",
  "IDEALIST" = "turquoise3",
  "RATIONAL" = "mediumpurple"
)

##############################
# 11. Figure 1
#     Box plots on f(t) values at different days
##############################
fig1 <- ggplot2::ggplot(
  discount_long,
  ggplot2::aes(x = Day, y = f_value, fill = Day)
) +
  ggplot2::geom_boxplot(
    width = 0.75,
    outlier.size = 1.8,
    linewidth = 0.4
  ) +
  ggplot2::labs(
    title = "Box plots on f(t) values at different days",
    x = "Days",
    y = "F(t) value"
  ) +
  paper_theme +
  ggplot2::theme(legend.position = "right")

save_plot_dual(fig1, "Figure 1", script_dir, width = 10, height = 7)

##############################
# 12. Figure 2
#     Box plots on f(t) values at different instants of time for each temperament
##############################
fig2 <- ggplot2::ggplot(
  discount_long,
  ggplot2::aes(x = Day, y = f_value, fill = Group)
) +
  ggplot2::geom_boxplot(
    position = ggplot2::position_dodge(width = 0.8),
    width = 0.75,
    outlier.size = 1.6,
    linewidth = 0.35
  ) +
  ggplot2::scale_fill_manual(values = group_colors, drop = FALSE) +
  ggplot2::labs(
    title = "Box plots on f(t) values at different instants of time for each temperament",
    x = "Days",
    y = "F(t) value",
    fill = "Group"
  ) +
  paper_theme

save_plot_dual(fig2, "Figure 2", script_dir, width = 12, height = 7)

###############################################################################
############ FDA OBJECT WITH BSPLINE BASIS SELECTED BY GRID SEARCH ###########
###############################################################################

##############################
# 13. Fixed settings
##############################
norder_bspline <- 4
nbasis_grid <- 5:9

##############################
# 14. Grid search over nbasis = 5,...,9
#     Criterion:
#     mean squared reconstruction error on observed points
##############################
grid_results <- data.frame(
  nbasis = integer(0),
  mse = numeric(0)
)

fd_candidates <- vector("list", length = length(nbasis_grid))
names(fd_candidates) <- as.character(nbasis_grid)

for (i in seq_along(nbasis_grid)) {
  nbasis_i <- nbasis_grid[i]
  
  basis_i <- fda::create.bspline.basis(
    rangeval = range_arg,
    nbasis = nbasis_i,
    norder = norder_bspline
  )
  
  fd_i <- fda::Data2fd(
    argvals = arg,
    y = t(dd_matrix),
    basisobj = basis_i
  )
  
  fitted_i <- t(fda::eval.fd(evalarg = arg, fdobj = fd_i))
  mse_i <- mean((dd_matrix - fitted_i)^2, na.rm = TRUE)
  
  grid_results <- rbind(
    grid_results,
    data.frame(
      nbasis = nbasis_i,
      mse = mse_i
    )
  )
  
  fd_candidates[[as.character(nbasis_i)]] <- fd_i
}

##############################
# 15. Select best number of basis functions
#     In case of ties, choose the smallest nbasis
##############################
best_row <- grid_results[order(grid_results$mse, grid_results$nbasis), ][1, , drop = FALSE]
best_nbasis <- best_row$nbasis

cat("\nGrid search results for nbasis:\n")
print(grid_results)
cat("\nSelected nbasis:", best_nbasis, "\n")

##############################
# 16. Final FDA object using selected nbasis
##############################
best_basis <- fda::create.bspline.basis(
  rangeval = range_arg,
  nbasis = best_nbasis,
  norder = norder_bspline
)

f_fd_orig <- fda::Data2fd(
  argvals = arg,
  y = t(dd_matrix),
  basisobj = best_basis
)

##############################
# 17. Compatibility object from fdata
#     Kept because your old code used both fda and fda.usc objects
##############################
f_fd_from_fdata <- fda.usc::fdata2fd(
  fdataobj = f,
  nbasis = best_nbasis,
  lambda = 1,
  nderiv = 0
)

##############################
# 18. Evaluate fitted curves on observed grid
##############################
fitted_best <- t(fda::eval.fd(evalarg = arg, fdobj = f_fd_orig))

##############################
# 19. Discount functions without monotone smoothing
##############################

# Colors aligned with the original Figure 5
group_colors_original <- c(
  "ARTISAN"  = "black",
  "GUARDIAN" = "#d95a6f",
  "IDEALIST" = "#6ecf5f",
  "RATIONAL" = "#33a3dc"
)

# Fine grid to actually display smooth B-spline curves
arg_fine <- seq(min(arg), max(arg), length.out = 400)

# Evaluate non-monotone FDA curves on the fine grid
# rows = subjects, columns = fine-grid points
f_fd_orig_eval_fine <- t(fda::eval.fd(evalarg = arg_fine, fdobj = f_fd_orig))

# Individual curve colors by group
individual_colors <- unname(group_colors_original[as.character(g)])

# Group mean curves on the fine grid
mean_curve_by_group <- lapply(levels(g), function(group_name) {
  colMeans(f_fd_orig_eval_fine[g == group_name, , drop = FALSE], na.rm = TRUE)
})
names(mean_curve_by_group) <- levels(g)

plot_discount_functions_without_monotone_smoothing <- function() {
  
  # Layout with dedicated legend panel, coherent with the original logic
  graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(0.74, 0.26))
  
  ##############################
  # Left panel: curves
  ##############################
  graphics::par(mar = c(6, 5, 5, 2) + 0.1)
  
  y_limits <- range(c(f_fd_orig_eval_fine, -0.02), na.rm = TRUE)
  
  graphics::plot(
    x = arg_fine,
    y = mean_curve_by_group[[1]],
    type = "n",
    ylim = y_limits,
    xaxt = "n",
    xlab = "Day",
    ylab = "Functional Mean",
    main = "Mean Discount Functions by Groups"
  )
  
  # Horizontal reference line at zero
  graphics::abline(h = 0, lty = 3, col = "grey50")
  
  # Thin individual smooth curves
  for (i in seq_len(nrow(f_fd_orig_eval_fine))) {
    graphics::lines(
      x = arg_fine,
      y = f_fd_orig_eval_fine[i, ],
      col = grDevices::adjustcolor(individual_colors[i], alpha.f = 0.40),
      lty = 3,
      lwd = 0.8
    )
  }
  
  # Thick group mean smooth curves
  for (group_name in levels(g)) {
    graphics::lines(
      x = arg_fine,
      y = mean_curve_by_group[[group_name]],
      col = group_colors_original[group_name],
      lwd = 3.2,
      lty = 1
    )
  }
  
  # Axis on observed time locations
  graphics::axis(
    side = 1,
    at = arg,
    labels = x_labels
  )
  
  ##############################
  # Right panel: legend only
  ##############################
  graphics::par(mar = c(6, 0, 5, 0) + 0.1)
  graphics::plot.new()
  
  graphics::legend(
    "topleft",
    legend = c(
      "ARTISAN",
      "GUARDIAN",
      "IDEALIST",
      "RATIONAL",
      "ARTISANS Mean",
      "GUARDIAN Mean",
      "IDEALISTS Mean",
      "RATIONALS Mean"
    ),
    col = c(
      group_colors_original["ARTISAN"],
      group_colors_original["GUARDIAN"],
      group_colors_original["IDEALIST"],
      group_colors_original["RATIONAL"],
      group_colors_original["ARTISAN"],
      group_colors_original["GUARDIAN"],
      group_colors_original["IDEALIST"],
      group_colors_original["RATIONAL"]
    ),
    lty = c(3, 3, 3, 3, 1, 1, 1, 1),
    lwd = c(0.8, 0.8, 0.8, 0.8, 3.2, 3.2, 3.2, 3.2),
    bty = "o",
    cex = 0.95
  )
}

save_base_plot_dual(
  filename_no_ext = "funzioni di sconto senza lo smoothing monotono",
  script_dir = script_dir,
  plot_fun = plot_discount_functions_without_monotone_smoothing,
  width_png = 2800,
  height_png = 1600,
  width_pdf = 12,
  height_pdf = 7,
  res_png = 300
)

##############################
# 20. Store outputs for later steps
##############################
f_fd <- f_fd_orig
selected_nbasis <- best_nbasis
basis_search_results <- grid_results

##############################
# 21. Console checks
##############################
cat("\nFigures successfully exported in:\n", normalizePath(script_dir), "\n")
cat("\nSample size used in this script:", nrow(analysis_data), "\n")
cat("\nGroup counts:\n")
print(table(analysis_data$Group))

cat("\nObjects created explicitly for later FDA steps:\n")
cat(" - analysis_data\n")
cat(" - dd\n")
cat(" - dd_matrix\n")
cat(" - g\n")
cat(" - color\n")
cat(" - tipo\n")
cat(" - x_labels\n")
cat(" - arg\n")
cat(" - f\n")
cat(" - f_fd_orig\n")
cat(" - f_fd_from_fdata\n")
cat(" - f_fd\n")
cat(" - fitted_best\n")
cat(" - basis_search_results\n")
cat(" - selected_nbasis\n")











###############################################################################
################ MONOTONE SMOOTHING OF DISCOUNT FUNCTIONS #####################
###############################################################################

##############################
# 1. Required existing objects
##############################
required_objects <- c(
  "dd_matrix",
  "time_days",
  "x_labels",
  "selected_nbasis",
  "script_dir",
  "g"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]
if (length(missing_objects) > 0) {
  stop(
    paste(
      "The following objects must already exist before running this block:",
      paste(missing_objects, collapse = ", ")
    )
  )
}

##############################
# 2. Basic checks
##############################
if (!is.matrix(dd_matrix)) {
  stop("'dd_matrix' must be a numeric matrix.")
}

if (!is.numeric(time_days)) {
  stop("'time_days' must be numeric.")
}

if (ncol(dd_matrix) != length(time_days)) {
  stop("The number of columns in 'dd_matrix' must match the length of 'time_days'.")
}

if (abs(time_days[1]) > 1e-12) {
  stop("The first time point must be 0 to enforce f(0) = 1 coherently.")
}

##############################
# 3. Monotone smoothing settings
##############################
n_subjects <- nrow(dd_matrix)
n_time <- ncol(dd_matrix)
norder_monotone <- 4
lambda_candidates <- c(1e-4, 1e-3, 1e-2)
time_fine <- seq(min(time_days), max(time_days), length.out = 400)

# Use exactly the same number of spline basis functions selected earlier
nbasis_monotone <- selected_nbasis

##############################
# 4. Input data for monotone smoothing
#    Force the baseline to be exactly 1 in the input used for fitting
##############################
dd_monotone_input <- dd_matrix
dd_monotone_input[, 1] <- 1

##############################
# 5. Common monotone basis
##############################
Wbasis <- fda::create.bspline.basis(
  rangeval = range(time_days),
  nbasis = nbasis_monotone,
  norder = norder_monotone
)

Wfd0 <- fda::fd(
  coef = matrix(0, nbasis_monotone, 1),
  basisobj = Wbasis
)

##############################
# 6. Helper: fit one monotone curve on g(t) = -log(f(t))
#    Then reconstruct f(t) = exp(-g(t))
##############################
fit_one_monotone_curve <- function(y_vec, time_obs, time_fine_grid, Wfd0_obj, lambda_grid, eps = 1e-6) {
  
  # Safety: discount function must be strictly positive before log
  y_vec <- as.numeric(y_vec)
  y_vec[y_vec <= 0] <- eps
  
  # Build transformed target
  # g(t) = -log(f(t)) should be monotone increasing
  g_obs <- -log(y_vec)
  
  fit_success <- FALSE
  fit_result <- NULL
  lambda_used <- NA_real_
  
  for (lambda_i in lambda_grid) {
    WfdPar_i <- fda::fdPar(fdobj = Wfd0_obj, Lfdobj = 1, lambda = lambda_i)
    
    try_fit <- try(
      fda::smooth.monotone(time_obs, g_obs, WfdPar_i),
      silent = TRUE
    )
    
    if (!inherits(try_fit, "try-error")) {
      fit_success <- TRUE
      fit_result <- try_fit
      lambda_used <- lambda_i
      break
    }
  }
  
  if (!fit_success) {
    stop("Monotone smoothing failed for at least one subject.")
  }
  
  Wfd_i <- fit_result$Wfd
  beta_i <- as.numeric(fit_result$beta)
  
  # Monotone estimate of g on observed grid
  ghat_obs <- beta_i[1] + beta_i[2] * fda::eval.monfd(time_obs, Wfd_i, Lfdobj = 0)
  
  # Monotone estimate of g on fine grid
  ghat_fine <- beta_i[1] + beta_i[2] * fda::eval.monfd(time_fine_grid, Wfd_i, Lfdobj = 0)
  
  # First and second derivatives of g
  ghat_d1_fine <- beta_i[2] * fda::eval.monfd(time_fine_grid, Wfd_i, Lfdobj = 1)
  ghat_d2_fine <- beta_i[2] * fda::eval.monfd(time_fine_grid, Wfd_i, Lfdobj = 2)
  
  # Normalize to ensure f(0) = 1 exactly
  ghat_obs <- ghat_obs - ghat_obs[1]
  ghat_fine <- ghat_fine - ghat_fine[1]
  
  # Reconstruct discount function
  fitted_obs <- exp(-ghat_obs)
  fitted_fine <- exp(-ghat_fine)
  
  # Derivatives of f from chain rule
  # f'(t) = -g'(t) exp(-g(t))
  # f''(t) = (g'(t)^2 - g''(t)) exp(-g(t))
  deriv1_fine <- -ghat_d1_fine * fitted_fine
  deriv2_fine <- (ghat_d1_fine^2 - ghat_d2_fine) * fitted_fine
  
  return(list(
    Wfd = Wfd_i,
    beta = beta_i,
    lambda = lambda_used,
    ghat_obs = ghat_obs,
    ghat_fine = ghat_fine,
    ghat_d1_fine = ghat_d1_fine,
    ghat_d2_fine = ghat_d2_fine,
    fitted_obs = fitted_obs,
    fitted_fine = fitted_fine,
    deriv1_fine = deriv1_fine,
    deriv2_fine = deriv2_fine
  ))
}

##############################
# 7. Fit all monotone discount curves
##############################
monotone_fit_list <- vector("list", n_subjects)

mat_monotone_obs <- matrix(NA_real_, nrow = n_subjects, ncol = n_time)
mat_monotone_fine <- matrix(NA_real_, nrow = n_subjects, ncol = length(time_fine))
d1_monotone_fine <- matrix(NA_real_, nrow = n_subjects, ncol = length(time_fine))
d2_monotone_fine <- matrix(NA_real_, nrow = n_subjects, ncol = length(time_fine))
ghat_monotone_obs <- matrix(NA_real_, nrow = n_subjects, ncol = n_time)
ghat_monotone_fine <- matrix(NA_real_, nrow = n_subjects, ncol = length(time_fine))
lambda_used_vec <- numeric(n_subjects)

for (i in seq_len(n_subjects)) {
  fit_i <- fit_one_monotone_curve(
    y_vec = dd_monotone_input[i, ],
    time_obs = time_days,
    time_fine_grid = time_fine,
    Wfd0_obj = Wfd0,
    lambda_grid = lambda_candidates,
    eps = 1e-6
  )
  
  monotone_fit_list[[i]] <- fit_i
  mat_monotone_obs[i, ] <- fit_i$fitted_obs
  mat_monotone_fine[i, ] <- fit_i$fitted_fine
  d1_monotone_fine[i, ] <- fit_i$deriv1_fine
  d2_monotone_fine[i, ] <- fit_i$deriv2_fine
  ghat_monotone_obs[i, ] <- fit_i$ghat_obs
  ghat_monotone_fine[i, ] <- fit_i$ghat_fine
  lambda_used_vec[i] <- fit_i$lambda
}



##############################
# 8. Quality checks
##############################
start_deviation <- max(abs(mat_monotone_obs[, 1] - 1), na.rm = TRUE)

nonpositive_values <- sum(mat_monotone_fine <= 0, na.rm = TRUE)

monotone_violations <- apply(
  mat_monotone_obs,
  1,
  function(z) any(diff(z) > 1e-8)
)

positive_first_derivative <- apply(
  d1_monotone_fine,
  1,
  function(z) any(z > 1e-6)
)

cat("\nMaximum deviation from f(0) = 1 after normalization:", start_deviation, "\n")
cat("Number of non-positive values on fine grid:", nonpositive_values, "\n")
cat("Number of curves with increasing segments on observed grid:", sum(monotone_violations), "\n")
cat("Number of curves with positive first derivative on fine grid:", sum(positive_first_derivative), "\n")

##############################
# 9. Store core monotone objects
##############################
# Observed-grid matrix, same role that 'mat' had before, now reproducible
mat <- mat_monotone_obs
colnames(mat) <- x_labels

# Fine-grid functional objects for downstream FDA work
d0_monotone <- fda.usc::fdata(
  mdata = mat_monotone_fine,
  argvals = time_fine,
  rangeval = range(time_fine),
  names = list(main = "Monotone discount functions", xlab = "Days", ylab = "f(t)")
)

d1_monotone <- fda.usc::fdata(
  mdata = d1_monotone_fine,
  argvals = time_fine,
  rangeval = range(time_fine),
  names = list(main = "First derivatives", xlab = "Days", ylab = "f'(t)")
)

d2_monotone <- fda.usc::fdata(
  mdata = d2_monotone_fine,
  argvals = time_fine,
  rangeval = range(time_fine),
  names = list(main = "Second derivatives", xlab = "Days", ylab = "f''(t)")
)

# Observed-grid functional object for checks and descriptive comparisons
f_monotone_obs <- fda.usc::fdata(
  mdata = mat,
  argvals = time_days,
  rangeval = range(time_days),
  names = list(main = "Monotone discount functions on observed grid", xlab = "Days", ylab = "f(t)")
)

##############################
# 9b. Functional object for g(t) = -log(f(t))
##############################
g_monotone <- fda.usc::fdata(
  mdata = ghat_monotone_fine,
  argvals = time_fine,
  rangeval = range(time_fine),
  names = list(main = "Monotone transformed functions", xlab = "Days", ylab = "g(t) = -log f(t)")
)

##############################
# 10. Optional compatibility objects
#     These are approximate fd representations built from the
#     already-smoothed monotone curves, only for compatibility later.
##############################
compat_basis_fine <- fda::create.bspline.basis(
  rangeval = range(time_fine),
  nbasis = nbasis_monotone,
  norder = norder_monotone
)

d0_fd_compat <- fda::Data2fd(
  argvals = time_fine,
  y = t(mat_monotone_fine),
  basisobj = compat_basis_fine
)

d1_fd_compat <- fda::Data2fd(
  argvals = time_fine,
  y = t(d1_monotone_fine),
  basisobj = compat_basis_fine
)

d2_fd_compat <- fda::Data2fd(
  argvals = time_fine,
  y = t(d2_monotone_fine),
  basisobj = compat_basis_fine
)

##############################
# 11. Fit summary table
##############################
monotone_fit_summary <- data.frame(
  id = seq_len(n_subjects),
  Group = as.character(g),
  lambda_used = lambda_used_vec,
  stringsAsFactors = FALSE
)

##############################
# 12. Figure: monotone discount functions
##############################
group_colors_original <- c(
  "ARTISAN"  = "black",
  "GUARDIAN" = "#d95a6f",
  "IDEALIST" = "#6ecf5f",
  "RATIONAL" = "#33a3dc"
)

individual_colors <- unname(group_colors_original[as.character(g)])

mean_curve_monotone_by_group <- lapply(levels(g), function(group_name) {
  colMeans(mat_monotone_fine[g == group_name, , drop = FALSE], na.rm = TRUE)
})
names(mean_curve_monotone_by_group) <- levels(g)

plot_monotone_discount_functions <- function() {
  
  graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(0.74, 0.26))
  
  ##############################
  # Left panel
  ##############################
  graphics::par(mar = c(6, 5, 5, 2) + 0.1)
  
  y_limits <- range(c(mat_monotone_fine, 0), na.rm = TRUE)
  
  graphics::plot(
    x = time_fine,
    y = mean_curve_monotone_by_group[[1]],
    type = "n",
    ylim = y_limits,
    xaxt = "n",
    xlab = "Day",
    ylab = "Functional Mean",
    main = "Discount Functions using the Monotone Smoothing"
  )
  
  graphics::abline(h = 0, lty = 3, col = "grey50")
  
  for (i in seq_len(nrow(mat_monotone_fine))) {
    graphics::lines(
      x = time_fine,
      y = mat_monotone_fine[i, ],
      col = grDevices::adjustcolor(individual_colors[i], alpha.f = 0.40),
      lty = 3,
      lwd = 0.8
    )
  }
  
  for (group_name in levels(g)) {
    graphics::lines(
      x = time_fine,
      y = mean_curve_monotone_by_group[[group_name]],
      col = group_colors_original[group_name],
      lwd = 3.2,
      lty = 1
    )
  }
  
  graphics::axis(
    side = 1,
    at = time_days,
    labels = x_labels
  )
  
  ##############################
  # Right panel
  ##############################
  graphics::par(mar = c(6, 0, 5, 0) + 0.1)
  graphics::plot.new()
  
  graphics::legend(
    "topleft",
    legend = c(
      "ARTISAN",
      "GUARDIAN",
      "IDEALIST",
      "RATIONAL",
      "ARTISANS Mean",
      "GUARDIAN Mean",
      "IDEALISTS Mean",
      "RATIONALS Mean"
    ),
    col = c(
      group_colors_original["ARTISAN"],
      group_colors_original["GUARDIAN"],
      group_colors_original["IDEALIST"],
      group_colors_original["RATIONAL"],
      group_colors_original["ARTISAN"],
      group_colors_original["GUARDIAN"],
      group_colors_original["IDEALIST"],
      group_colors_original["RATIONAL"]
    ),
    lty = c(3, 3, 3, 3, 1, 1, 1, 1),
    lwd = c(0.8, 0.8, 0.8, 0.8, 3.2, 3.2, 3.2, 3.2),
    bty = "o",
    cex = 0.95
  )
}

save_base_plot_dual(
  filename_no_ext = "funzioni di sconto con lo smoothing monotono",
  script_dir = script_dir,
  plot_fun = plot_monotone_discount_functions,
  width_png = 2800,
  height_png = 1600,
  width_pdf = 12,
  height_pdf = 7,
  res_png = 300
)

##############################
# 13. Console output
##############################
cat("\nMonotone smoothing completed successfully.\n")
cat("Number of subjects:", n_subjects, "\n")
cat("Number of basis functions used in monotone smoothing:", nbasis_monotone, "\n")
cat("Unique lambda values used:\n")
print(sort(unique(lambda_used_vec)))

cat("\nObjects created in this block:\n")
cat(" - mat\n")
cat(" - monotone_fit_list\n")
cat(" - monotone_fit_summary\n")
cat(" - f_monotone_obs\n")
cat(" - d0_monotone\n")
cat(" - d1_monotone\n")
cat(" - d2_monotone\n")
cat(" - d0_fd_compat\n")
cat(" - d1_fd_compat\n")
cat(" - d2_fd_compat\n")






###############################################################################
#################### FIRST AND SECOND DERIVATIVE FIGURES ######################
###############################################################################

##############################
# 1. Safety checks
##############################
required_objects_derivatives <- c(
  "d1_monotone_fine",
  "d2_monotone_fine",
  "time_fine",
  "time_days",
  "x_labels",
  "g",
  "script_dir"
)

missing_objects_derivatives <- required_objects_derivatives[
  !vapply(required_objects_derivatives, exists, logical(1))
]

if (length(missing_objects_derivatives) > 0) {
  stop(
    paste(
      "The following objects must already exist before running the derivative plots:",
      paste(missing_objects_derivatives, collapse = ", ")
    )
  )
}

##############################
# 2. Colors
##############################
group_colors_original <- c(
  "ARTISAN"  = "black",
  "GUARDIAN" = "#d95a6f",
  "IDEALIST" = "#6ecf5f",
  "RATIONAL" = "#33a3dc"
)

individual_colors <- unname(group_colors_original[as.character(g)])

##############################
# 3. Group mean curves
##############################
mean_curve_d1_by_group <- lapply(levels(g), function(group_name) {
  colMeans(d1_monotone_fine[g == group_name, , drop = FALSE], na.rm = TRUE)
})
names(mean_curve_d1_by_group) <- levels(g)

mean_curve_d2_by_group <- lapply(levels(g), function(group_name) {
  colMeans(d2_monotone_fine[g == group_name, , drop = FALSE], na.rm = TRUE)
})
names(mean_curve_d2_by_group) <- levels(g)

##############################
# 4. First derivative plot
##############################
plot_first_derivative_monotone <- function() {
  
  graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(0.74, 0.26))
  
  ##############################
  # Left panel
  ##############################
  graphics::par(mar = c(6, 5, 5, 2) + 0.1)
  
  y_limits <- range(c(-0.4, 0), na.rm = TRUE)
  
  graphics::plot(
    x = time_fine,
    y = mean_curve_d1_by_group[[1]],
    type = "n",
    ylim = y_limits,
    xaxt = "n",
    xlab = "Day",
    ylab = "Functional Mean",
    main = "First Derivatives using the Monotone Smoothing"
  )
  
  graphics::abline(h = 0, lty = 3, col = "grey50")
  
  for (i in seq_len(nrow(d1_monotone_fine))) {
    graphics::lines(
      x = time_fine,
      y = d1_monotone_fine[i, ],
      col = grDevices::adjustcolor(individual_colors[i], alpha.f = 0.40),
      lty = 3,
      lwd = 0.8
    )
  }
  
  for (group_name in levels(g)) {
    graphics::lines(
      x = time_fine,
      y = mean_curve_d1_by_group[[group_name]],
      col = group_colors_original[group_name],
      lwd = 3.2,
      lty = 1
    )
  }
  
  graphics::axis(
    side = 1,
    at = time_days,
    labels = x_labels
  )
  
  ##############################
  # Right panel
  ##############################
  graphics::par(mar = c(6, 0, 5, 0) + 0.1)
  graphics::plot.new()
  
  graphics::legend(
    "topleft",
    legend = c(
      "ARTISAN",
      "GUARDIAN",
      "IDEALIST",
      "RATIONAL",
      "ARTISANS Mean",
      "GUARDIAN Mean",
      "IDEALISTS Mean",
      "RATIONALS Mean"
    ),
    col = c(
      group_colors_original["ARTISAN"],
      group_colors_original["GUARDIAN"],
      group_colors_original["IDEALIST"],
      group_colors_original["RATIONAL"],
      group_colors_original["ARTISAN"],
      group_colors_original["GUARDIAN"],
      group_colors_original["IDEALIST"],
      group_colors_original["RATIONAL"]
    ),
    lty = c(3, 3, 3, 3, 1, 1, 1, 1),
    lwd = c(0.8, 0.8, 0.8, 0.8, 3.2, 3.2, 3.2, 3.2),
    bty = "o",
    cex = 0.95
  )
}

save_base_plot_dual(
  filename_no_ext = "derivata prima funzioni di sconto con lo smoothing monotono",
  script_dir = script_dir,
  plot_fun = plot_first_derivative_monotone,
  width_png = 2800,
  height_png = 1600,
  width_pdf = 12,
  height_pdf = 7,
  res_png = 300
)

##############################
# 5. Second derivative plot
##############################
plot_second_derivative_monotone <- function() {
  
  graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(0.74, 0.26))
  
  ##############################
  # Left panel
  ##############################
  graphics::par(mar = c(6, 5, 5, 2) + 0.1)
  
  y_limits <- range(c(1, 0), na.rm = TRUE)
  
  graphics::plot(
    x = time_fine,
    y = mean_curve_d2_by_group[[1]],
    type = "n",
    ylim = y_limits,
    xaxt = "n",
    xlab = "Day",
    ylab = "Functional Mean",
    main = "Second Derivatives using the Monotone Smoothing"
  )
  
  graphics::abline(h = 0, lty = 3, col = "grey50")
  
  for (i in seq_len(nrow(d2_monotone_fine))) {
    graphics::lines(
      x = time_fine,
      y = d2_monotone_fine[i, ],
      col = grDevices::adjustcolor(individual_colors[i], alpha.f = 0.40),
      lty = 3,
      lwd = 0.8
    )
  }
  
  for (group_name in levels(g)) {
    graphics::lines(
      x = time_fine,
      y = mean_curve_d2_by_group[[group_name]],
      col = group_colors_original[group_name],
      lwd = 3.2,
      lty = 1
    )
  }
  
  graphics::axis(
    side = 1,
    at = time_days,
    labels = x_labels
  )
  
  ##############################
  # Right panel
  ##############################
  graphics::par(mar = c(6, 0, 5, 0) + 0.1)
  graphics::plot.new()
  
  graphics::legend(
    "topleft",
    legend = c(
      "ARTISAN",
      "GUARDIAN",
      "IDEALIST",
      "RATIONAL",
      "ARTISANS Mean",
      "GUARDIAN Mean",
      "IDEALISTS Mean",
      "RATIONALS Mean"
    ),
    col = c(
      group_colors_original["ARTISAN"],
      group_colors_original["GUARDIAN"],
      group_colors_original["IDEALIST"],
      group_colors_original["RATIONAL"],
      group_colors_original["ARTISAN"],
      group_colors_original["GUARDIAN"],
      group_colors_original["IDEALIST"],
      group_colors_original["RATIONAL"]
    ),
    lty = c(3, 3, 3, 3, 1, 1, 1, 1),
    lwd = c(0.8, 0.8, 0.8, 0.8, 3.2, 3.2, 3.2, 3.2),
    bty = "o",
    cex = 0.95
  )
}

save_base_plot_dual(
  filename_no_ext = "derivata seconda funzioni di sconto con lo smoothing monotono",
  script_dir = script_dir,
  plot_fun = plot_second_derivative_monotone,
  width_png = 2800,
  height_png = 1600,
  width_pdf = 12,
  height_pdf = 7,
  res_png = 300
)




###############################################################################
###################### WITHIN-GROUP FUNCTIONAL VARIANCE #######################
########################### FIGURES 8, 9, AND 10 ##############################
###############################################################################

##############################
# 1. Safety checks
##############################
required_objects_variance <- c(
  "mat_monotone_fine",
  "d1_monotone_fine",
  "d2_monotone_fine",
  "time_fine",
  "time_days",
  "x_labels",
  "g",
  "script_dir",
  "save_base_plot_dual"
)

missing_objects_variance <- required_objects_variance[
  !vapply(required_objects_variance, exists, logical(1))
]

if (length(missing_objects_variance) > 0) {
  stop(
    paste(
      "The following objects must already exist before running the variance plots:",
      paste(missing_objects_variance, collapse = ", ")
    )
  )
}

##############################
# 2. Colors
##############################
group_colors_original <- c(
  "ARTISAN"  = "black",
  "GUARDIAN" = "red",
  "IDEALIST" = "blue",
  "RATIONAL" = "green3"
)

##############################
# 3. Helper function
##############################
compute_within_group_variance_curve <- function(curve_matrix, group_factor, group_name) {
  group_matrix <- curve_matrix[group_factor == group_name, , drop = FALSE]
  
  if (nrow(group_matrix) < 2) {
    warning(paste("Group", group_name, "has fewer than 2 observations. Variance set to zero."))
    return(rep(0, ncol(group_matrix)))
  }
  
  apply(group_matrix, 2, stats::var, na.rm = TRUE)
}

##############################
# 4. Compute variance curves
##############################
var_d0_by_group <- lapply(levels(g), function(group_name) {
  compute_within_group_variance_curve(
    curve_matrix = mat_monotone_fine,
    group_factor = g,
    group_name = group_name
  )
})
names(var_d0_by_group) <- levels(g)

var_d1_by_group <- lapply(levels(g), function(group_name) {
  compute_within_group_variance_curve(
    curve_matrix = d1_monotone_fine,
    group_factor = g,
    group_name = group_name
  )
})
names(var_d1_by_group) <- levels(g)

var_d2_by_group <- lapply(levels(g), function(group_name) {
  compute_within_group_variance_curve(
    curve_matrix = d2_monotone_fine,
    group_factor = g,
    group_name = group_name
  )
})
names(var_d2_by_group) <- levels(g)

##############################
# 5. Figure 8
#    Within-group functional variance
##############################
plot_figure_8 <- function() {
  graphics::par(mfrow = c(1, 1))
  graphics::par(mar = c(6, 5, 5, 2) + 0.1)
  
  y_limits <- range(unlist(var_d0_by_group), na.rm = TRUE)
  
  graphics::plot(
    x = time_fine,
    y = var_d0_by_group[[1]],
    type = "l",
    lwd = 2.5,
    col = group_colors_original[levels(g)[1]],
    ylim = y_limits,
    xaxt = "n",
    xlab = "Days",
    ylab = "Within-Group Variance",
    main = "Within-Group Variance for Each Temperament"
  )
  
  for (group_name in levels(g)[-1]) {
    graphics::lines(
      x = time_fine,
      y = var_d0_by_group[[group_name]],
      lwd = 2.5,
      col = group_colors_original[group_name]
    )
  }
  
  graphics::axis(
    side = 1,
    at = time_days,
    labels = x_labels
  )
  
  graphics::legend(
    "bottomright",
    legend = levels(g),
    col = group_colors_original[levels(g)],
    lty = 1,
    lwd = 2.5,
    bty = "o",
    cex = 0.85
  )
}

save_base_plot_dual(
  filename_no_ext = "Figure 8",
  script_dir = script_dir,
  plot_fun = plot_figure_8,
  width_png = 2200,
  height_png = 1600,
  width_pdf = 8,
  height_pdf = 7,
  res_png = 300
)

##############################
# 6. Figure 9
#    Within-group functional variance of first derivative
##############################
plot_figure_9 <- function() {
  graphics::par(mfrow = c(1, 1))
  graphics::par(mar = c(6, 5, 5, 2) + 0.1)
  
  y_limits <- c(0, 0.01)
  
  graphics::plot(
    x = time_fine,
    y = var_d1_by_group[[1]],
    type = "l",
    lwd = 2.5,
    col = group_colors_original[levels(g)[1]],
    ylim = y_limits,
    xaxt = "n",
    xlab = "Days",
    ylab = "Within-Group Variance (D1)",
    main = "Within-Group Variance for Each Temperament First Derivative"
  )
  
  for (group_name in levels(g)[-1]) {
    graphics::lines(
      x = time_fine,
      y = var_d1_by_group[[group_name]],
      lwd = 2.5,
      col = group_colors_original[group_name]
    )
  }
  
  graphics::axis(
    side = 1,
    at = time_days,
    labels = x_labels
  )
  
  graphics::legend(
    "topright",
    legend = levels(g),
    col = group_colors_original[levels(g)],
    lty = 1,
    lwd = 2.5,
    bty = "o",
    cex = 0.85
  )
}

save_base_plot_dual(
  filename_no_ext = "Figure 9",
  script_dir = script_dir,
  plot_fun = plot_figure_9,
  width_png = 2200,
  height_png = 1600,
  width_pdf = 8,
  height_pdf = 7,
  res_png = 300
)

##############################
# 7. Figure 10
#    Within-group functional variance of second derivative
##############################
plot_figure_10 <- function() {
  graphics::par(mfrow = c(1, 1))
  graphics::par(mar = c(6, 5, 5, 2) + 0.1)
  
  y_limits <- c(0, 0.01)
  
  graphics::plot(
    x = time_fine,
    y = var_d2_by_group[[1]],
    type = "l",
    lwd = 2.5,
    col = group_colors_original[levels(g)[1]],
    ylim = y_limits,
    xaxt = "n",
    xlab = "Days",
    ylab = "Within-Group Variance (D2)",
    main = "Within-Group Variance for Each Temperament Second Derivative"
  )
  
  for (group_name in levels(g)[-1]) {
    graphics::lines(
      x = time_fine,
      y = var_d2_by_group[[group_name]],
      lwd = 2.5,
      col = group_colors_original[group_name]
    )
  }
  
  graphics::axis(
    side = 1,
    at = time_days,
    labels = x_labels
  )
  
  graphics::legend(
    "topright",
    legend = levels(g),
    col = group_colors_original[levels(g)],
    lty = 1,
    lwd = 2.5,
    bty = "o",
    cex = 0.85
  )
}

save_base_plot_dual(
  filename_no_ext = "Figure 10",
  script_dir = script_dir,
  plot_fun = plot_figure_10,
  width_png = 2200,
  height_png = 1600,
  width_pdf = 8,
  height_pdf = 7,
  res_png = 300
)

##############################
# 8. Store outputs
##############################
within_group_variance_d0 <- var_d0_by_group
within_group_variance_d1 <- var_d1_by_group
within_group_variance_d2 <- var_d2_by_group

cat("\nWithin-group variance figures exported successfully.\n")
cat("Files created:\n")
cat(" - Figure 8.png / Figure 8.pdf\n")
cat(" - Figure 9.png / Figure 9.pdf\n")
cat(" - Figure 10.png / Figure 10.pdf\n")


###############################################################################
######################## FIGURES 11, 12, AND 13 ###############################
################ CONDITIONAL FUNCTIONAL CLUSTERING BY BIT #####################
###############################################################################

##############################
# 1. Required packages
##############################
required_packages_conditional <- c(
  "fdaoutlier"
)

missing_packages_conditional <- required_packages_conditional[
  !vapply(required_packages_conditional, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages_conditional) > 0) {
  stop(
    paste(
      "Install the following packages before running this block:",
      paste(missing_packages_conditional, collapse = ", ")
    )
  )
}

##############################
# 2. Required existing objects
##############################
required_objects_conditional <- c(
  "mat_monotone_obs",
  "mat_monotone_fine",
  "time_days",
  "time_fine",
  "x_labels",
  "g",
  "script_dir",
  "save_base_plot_dual"
)

missing_objects_conditional <- required_objects_conditional[
  !vapply(required_objects_conditional, exists, logical(1))
]

if (length(missing_objects_conditional) > 0) {
  stop(
    paste(
      "The following objects must already exist before running this block:",
      paste(missing_objects_conditional, collapse = ", ")
    )
  )
}

##############################
# 3. Basic checks
##############################
if (!is.matrix(mat_monotone_obs)) {
  stop("'mat_monotone_obs' must be a numeric matrix.")
}

if (!is.matrix(mat_monotone_fine)) {
  stop("'mat_monotone_fine' must be a numeric matrix.")
}

if (nrow(mat_monotone_obs) != length(g)) {
  stop("The number of rows in 'mat_monotone_obs' must match the length of 'g'.")
}

if (nrow(mat_monotone_fine) != length(g)) {
  stop("The number of rows in 'mat_monotone_fine' must match the length of 'g'.")
}

if (ncol(mat_monotone_obs) != length(time_days)) {
  stop("The number of columns in 'mat_monotone_obs' must match 'time_days'.")
}

if (ncol(mat_monotone_fine) != length(time_fine)) {
  stop("The number of columns in 'mat_monotone_fine' must match 'time_fine'.")
}

##############################
# 4. Settings
##############################
set.seed(1234)

group_names <- levels(g)

# Final number of conditional clusters as described in the manuscript
final_k_by_group <- c(
  "ARTISAN"  = 4,
  "GUARDIAN" = 2,
  "IDEALIST" = 4,
  "RATIONAL" = 4
)

max_clusters_to_try <- 6

# We keep the 60% central region reference from the manuscript,
# but we also protect against pathological over-removal.
central_region_mbd <- 0.60
max_outlier_fraction <- 0.15

cluster_palette <- c(
  "#1b9e77",
  "#d95f02",
  "#7570b3",
  "#e7298a",
  "#66a61e",
  "#e6ab02"
)

##############################
# 5. Helper functions
##############################
safe_kmeans <- function(x, centers, nstart = 50, iter.max = 200) {
  stats::kmeans(
    x = x,
    centers = centers,
    nstart = nstart,
    iter.max = iter.max
  )
}

compute_wcss_curve <- function(data_matrix, max_clusters = 6) {
  n_obs <- nrow(data_matrix)
  
  if (n_obs < 2) {
    return(data.frame(k = integer(0), wcss = numeric(0)))
  }
  
  max_k <- min(max_clusters, n_obs)
  k_values <- seq_len(max_k)
  wcss_values <- numeric(length(k_values))
  
  for (i in seq_along(k_values)) {
    k_i <- k_values[i]
    km_i <- safe_kmeans(data_matrix, centers = k_i, nstart = 50, iter.max = 200)
    wcss_values[i] <- km_i$tot.withinss
  }
  
  data.frame(
    k = k_values,
    wcss = wcss_values
  )
}

compute_apn_curve <- function(data_matrix, max_clusters = 6, seed = 1234) {
  n_obs <- nrow(data_matrix)
  n_time <- ncol(data_matrix)
  
  if (n_obs < 3 || n_time < 2) {
    return(data.frame(k = integer(0), apn = numeric(0)))
  }
  
  max_k <- min(max_clusters, n_obs - 1)
  
  if (max_k < 2) {
    return(data.frame(k = integer(0), apn = numeric(0)))
  }
  
  k_values <- 2:max_k
  apn_values <- numeric(length(k_values))
  
  for (i in seq_along(k_values)) {
    k_i <- k_values[i]
    
    set.seed(seed + k_i)
    km_full <- safe_kmeans(data_matrix, centers = k_i, nstart = 50, iter.max = 200)
    cl_full <- km_full$cluster
    same_full <- outer(cl_full, cl_full, "==")
    
    apn_leave_one_time <- numeric(n_time)
    
    for (j in seq_len(n_time)) {
      reduced_matrix <- data_matrix[, -j, drop = FALSE]
      
      set.seed(seed + 1000 * k_i + j)
      km_reduced <- safe_kmeans(reduced_matrix, centers = k_i, nstart = 50, iter.max = 200)
      cl_reduced <- km_reduced$cluster
      same_reduced <- outer(cl_reduced, cl_reduced, "==")
      
      upper_index <- upper.tri(same_full)
      apn_leave_one_time[j] <- mean(
        same_full[upper_index] != same_reduced[upper_index]
      )
    }
    
    apn_values[i] <- mean(apn_leave_one_time)
  }
  
  data.frame(
    k = k_values,
    apn = apn_values
  )
}

compute_centroids_from_labels <- function(data_matrix, cluster_labels) {
  cluster_ids <- sort(unique(cluster_labels))
  
  centroid_matrix <- sapply(cluster_ids, function(k_i) {
    colMeans(data_matrix[cluster_labels == k_i, , drop = FALSE], na.rm = TRUE)
  })
  
  if (is.vector(centroid_matrix)) {
    centroid_matrix <- matrix(centroid_matrix, ncol = 1)
  }
  
  centroid_matrix
}

detect_outliers_mbd <- function(curve_matrix_obs, central_region = 0.30, max_fraction = 0.40) {
  n_curves <- nrow(curve_matrix_obs)
  
  if (n_curves < 4) {
    return(list(
      outlier_indices = integer(0),
      non_outlier_indices = seq_len(n_curves),
      method_used = "no_removal_small_group"
    ))
  }
  
  # fdaoutlier::functional_boxplot is fed in the same orientation used in the old workflow:
  # time points in rows, curves in columns
  fb_obj <- try(
    fdaoutlier::functional_boxplot(
      t(curve_matrix_obs),
      depth_method = "mbd",
      central_region = central_region
    ),
    silent = TRUE
  )
  
  if (inherits(fb_obj, "try-error")) {
    return(list(
      outlier_indices = integer(0),
      non_outlier_indices = seq_len(n_curves),
      method_used = "no_removal_fbplot_failed"
    ))
  }
  
  outlier_indices <- fb_obj$outliers
  
  if (is.null(outlier_indices) || length(outlier_indices) == 0) {
    outlier_indices <- integer(0)
  } else {
    outlier_indices <- sort(unique(as.integer(outlier_indices)))
    outlier_indices <- outlier_indices[outlier_indices >= 1 & outlier_indices <= n_curves]
  }
  
  non_outlier_indices <- setdiff(seq_len(n_curves), outlier_indices)
  
  # Safety rule:
  # if the procedure becomes degenerate and removes too many curves,
  # we keep all curves rather than destroy the sample.
  if (length(non_outlier_indices) < 3 || (length(outlier_indices) / n_curves) > max_fraction) {
    return(list(
      outlier_indices = integer(0),
      non_outlier_indices = seq_len(n_curves),
      method_used = "no_removal_overflagging"
    ))
  }
  
  list(
    outlier_indices = outlier_indices,
    non_outlier_indices = non_outlier_indices,
    method_used = "mbd_30pct"
  )
}

##############################
# 6. Build group-specific clean datasets
##############################
conditional_group_data <- vector("list", length(group_names))
names(conditional_group_data) <- group_names

for (grp in group_names) {
  idx_grp <- which(g == grp)
  
  group_obs <- mat_monotone_obs[idx_grp, , drop = FALSE]
  group_fine <- mat_monotone_fine[idx_grp, , drop = FALSE]
  
  outlier_info <- detect_outliers_mbd(
    curve_matrix_obs = group_obs,
    central_region = central_region_mbd,
    max_fraction = max_outlier_fraction
  )
  
  keep_local <- outlier_info$non_outlier_indices
  drop_local <- outlier_info$outlier_indices
  
  conditional_group_data[[grp]] <- list(
    group_name = grp,
    all_indices_global = idx_grp,
    all_obs = group_obs,
    all_fine = group_fine,
    keep_local = keep_local,
    drop_local = drop_local,
    clean_indices_global = idx_grp[keep_local],
    clean_obs = group_obs[keep_local, , drop = FALSE],
    clean_fine = group_fine[keep_local, , drop = FALSE],
    outlier_method = outlier_info$method_used
  )
}

##############################
# 7. Figure 11 data, Elbow curves by group
##############################
figure_11_results <- vector("list", length(group_names))
names(figure_11_results) <- group_names

for (grp in group_names) {
  clean_obs <- conditional_group_data[[grp]]$clean_obs
  
  figure_11_results[[grp]] <- compute_wcss_curve(
    data_matrix = clean_obs,
    max_clusters = max_clusters_to_try
  )
}

##############################
# 8. Figure 12 data, APN curves by group
##############################
figure_12_results <- vector("list", length(group_names))
names(figure_12_results) <- group_names

for (grp in group_names) {
  clean_obs <- conditional_group_data[[grp]]$clean_obs
  
  figure_12_results[[grp]] <- compute_apn_curve(
    data_matrix = clean_obs,
    max_clusters = max_clusters_to_try,
    seed = 1234
  )
}

##############################
# 9. Figure 13 data, final conditional clustering
##############################
figure_13_results <- vector("list", length(group_names))
names(figure_13_results) <- group_names

for (grp in group_names) {
  clean_obs <- conditional_group_data[[grp]]$clean_obs
  clean_fine <- conditional_group_data[[grp]]$clean_fine
  
  n_clean <- nrow(clean_obs)
  k_target <- unname(final_k_by_group[grp])
  
  if (n_clean < k_target) {
    stop(
      paste(
        "Group", grp,
        "has", n_clean, "non-outlier curves, but Figure 13 requires",
        k_target, "clusters as in the manuscript."
      )
    )
  }
  
  # Clustering on the observed discretized grid
  set.seed(1234 + which(group_names == grp))
  km_grp <- safe_kmeans(clean_obs, centers = k_target, nstart = 100, iter.max = 300)
  
  # Smooth centroids for plotting on the fine grid,
  # computed as the mean of the fine-grid monotone curves by cluster.
  centroid_fine <- compute_centroids_from_labels(
    data_matrix = clean_fine,
    cluster_labels = km_grp$cluster
  )
  
  figure_13_results[[grp]] <- list(
    k_final = k_target,
    cluster_labels = km_grp$cluster,
    clean_obs = clean_obs,
    clean_fine = clean_fine,
    centroid_fine = centroid_fine
  )
}

##############################
# 10. Figure 11, Elbow method
##############################
plot_figure_11 <- function() {
  graphics::par(mfrow = c(2, 2))
  graphics::par(mar = c(5, 4, 4, 1) + 0.1)
  
  panel_colors <- c(
    "ARTISAN"  = "black",
    "GUARDIAN" = "red",
    "IDEALIST" = "blue",
    "RATIONAL" = "darkgreen"
  )
  
  for (grp in group_names) {
    df_elbow <- figure_11_results[[grp]]
    
    if (nrow(df_elbow) == 0) {
      graphics::plot.new()
      graphics::title(main = grp)
      graphics::text(0.5, 0.5, "Not enough curves")
    } else {
      graphics::plot(
        df_elbow$k,
        df_elbow$wcss,
        type = "b",
        pch = 19,
        col = panel_colors[grp],
        xlab = "Subclusters",
        ylab = "WCSS",
        main = grp
      )
    }
  }
}

save_base_plot_dual(
  filename_no_ext = "Figure 11",
  script_dir = script_dir,
  plot_fun = plot_figure_11,
  width_png = 2200,
  height_png = 1600,
  width_pdf = 10,
  height_pdf = 7,
  res_png = 300
)


##############################
# 11. Figure 12, APN
##############################

plot_figure_12 <- function() {
  graphics::par(mfrow = c(2, 2))
  graphics::par(mar = c(5, 4, 4, 1) + 0.1)
  
  line_colors <- c(
    "ARTISAN"  = "black",
    "GUARDIAN" = "red",
    "IDEALIST" = "blue",
    "RATIONAL" = "darkgreen"
  )
  
  for (grp in group_names) {
    df_apn <- figure_12_results[[grp]]
    
    if (nrow(df_apn) == 0) {
      graphics::plot.new()
      graphics::title(main = grp)
      graphics::text(0.5, 0.5, "Not enough curves")
    } else {
      graphics::plot(
        df_apn$k,
        df_apn$apn,
        type = "l",
        col = line_colors[grp],
        xlab = "Number of Clusters",
        ylab = "APN",
        main = grp
      )
    }
  }
}

save_base_plot_dual(
  filename_no_ext = "Figure 12",
  script_dir = script_dir,
  plot_fun = plot_figure_12,
  width_png = 2200,
  height_png = 1600,
  width_pdf = 10,
  height_pdf = 7,
  res_png = 300
)


##############################
# 12. Figure 13, conditional functional k-means
##############################
plot_figure_13 <- function() {
  graphics::par(mfrow = c(2, 2))
  graphics::par(mar = c(5, 4, 4, 1) + 0.1)
  
  for (grp in group_names) {
    res_grp <- figure_13_results[[grp]]
    
    clean_fine <- res_grp$clean_fine
    cluster_labels <- res_grp$cluster_labels
    centroid_fine <- res_grp$centroid_fine
    k_final <- res_grp$k_final
    
    ylim_use <- range(clean_fine, centroid_fine, na.rm = TRUE)
    
    graphics::plot(
      x = time_fine,
      y = clean_fine[1, ],
      type = "n",
      ylim = ylim_use,
      xaxt = "n",
      xlab = "Days",
      ylab = "f(t)",
      main = grp
    )
    
    graphics::axis(
      side = 1,
      at = time_days,
      labels = x_labels
    )
    
    for (k_i in seq_len(k_final)) {
      idx_k <- which(cluster_labels == k_i)
      col_k <- cluster_palette[k_i]
      
      if (length(idx_k) > 0) {
        for (i in idx_k) {
          graphics::lines(
            x = time_fine,
            y = clean_fine[i, ],
            col = grDevices::adjustcolor(col_k, alpha.f = 0.45),
            lty = 3,
            lwd = 1
          )
        }
      }
      
      graphics::lines(
        x = time_fine,
        y = centroid_fine[, k_i],
        col = col_k,
        lwd = 3
      )
    }
    
    graphics::legend(
      "topright",
      legend = paste("Subgroup", seq_len(k_final)),
      col = cluster_palette[seq_len(k_final)],
      lty = 1,
      lwd = 3,
      bty = "o",
      cex = 0.8
    )
  }
}

save_base_plot_dual(
  filename_no_ext = "Figure 13",
  script_dir = script_dir,
  plot_fun = plot_figure_13,
  width_png = 2480,
  height_png = 3508,
  width_pdf = 8.27,
  height_pdf = 11.69,
  res_png = 300
)


##############################
# 13. Summary tables stored in memory
##############################
figure_11_table <- do.call(
  rbind,
  lapply(group_names, function(grp) {
    df <- figure_11_results[[grp]]
    if (nrow(df) == 0) {
      return(NULL)
    }
    data.frame(
      Group = grp,
      k = df$k,
      WCSS = df$wcss,
      row.names = NULL
    )
  })
)

figure_12_table <- do.call(
  rbind,
  lapply(group_names, function(grp) {
    df <- figure_12_results[[grp]]
    if (nrow(df) == 0) {
      return(NULL)
    }
    data.frame(
      Group = grp,
      k = df$k,
      APN = df$apn,
      row.names = NULL
    )
  })
)

figure_13_cluster_sizes <- do.call(
  rbind,
  lapply(group_names, function(grp) {
    res <- figure_13_results[[grp]]
    tab <- table(res$cluster_labels)
    data.frame(
      Group = grp,
      Cluster = paste0("C", seq_along(tab)),
      Size = as.integer(tab),
      row.names = NULL
    )
  })
)

##############################
# 14. Store objects
##############################
conditional_group_data_final <- conditional_group_data
figure_11_results_final <- figure_11_results
figure_12_results_final <- figure_12_results
figure_13_results_final <- figure_13_results

cat("\nConditional clustering block completed successfully.\n")
cat("\nFiles exported:\n")
cat(" - Figure 11.png / Figure 11.pdf\n")
cat(" - Figure 12.png / Figure 12.pdf\n")
cat(" - Figure 13.png / Figure 13.pdf\n")

cat("\nOutlier handling summary by group:\n")
for (grp in group_names) {
  cat(
    grp, ": ",
    conditional_group_data[[grp]]$outlier_method,
    ", kept ", nrow(conditional_group_data[[grp]]$clean_obs),
    " of ", nrow(conditional_group_data[[grp]]$all_obs), "\n",
    sep = ""
  )
}





###############################################################################
######################## FIGURES 14 AND 15 ####################################
############ ELASTIC ROBUSTNESS CHECKS, CONDITIONAL AND GLOBAL ################
###############################################################################

##############################
# 1. Required packages
##############################
required_packages_elastic <- c(
  "fdasrvf",
  "mclust"
)

missing_packages_elastic <- required_packages_elastic[
  !vapply(required_packages_elastic, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages_elastic) > 0) {
  stop(
    paste(
      "Install the following packages before running this block:",
      paste(missing_packages_elastic, collapse = ", ")
    )
  )
}

##############################
# 2. Required existing objects
##############################
required_objects_elastic <- c(
  "mat_monotone_obs",
  "mat_monotone_fine",
  "time_days",
  "time_fine",
  "x_labels",
  "g",
  "script_dir",
  "conditional_group_data",
  "figure_13_results"
)

missing_objects_elastic <- required_objects_elastic[
  !vapply(required_objects_elastic, exists, logical(1))
]

if (length(missing_objects_elastic) > 0) {
  stop(
    paste(
      "The following objects must already exist before running Figures 14 and 15:",
      paste(missing_objects_elastic, collapse = ", ")
    )
  )
}

##############################
# 3. Global settings
##############################
set.seed(1234)

time_scaled_obs <- (time_days - min(time_days)) / (max(time_days) - min(time_days))
time_scaled_fine <- (time_fine - min(time_fine)) / (max(time_fine) - min(time_fine))

group_names <- c("ARTISAN", "GUARDIAN", "IDEALIST", "RATIONAL")

cluster_palette <- c(
  "#1b9e77",
  "#d95f02",
  "#7570b3",
  "#e7298a",
  "#66a61e",
  "#e6ab02"
)

##############################
# 4. Helper functions
##############################
safe_adjustcolor <- function(col, alpha = 0.25) {
  grDevices::adjustcolor(col, alpha.f = alpha)
}

extract_elastic_labels <- function(km_object) {
  if (!is.null(km_object$labels)) {
    return(as.integer(km_object$labels))
  }
  stop("Elastic clustering output does not contain 'labels'.")
}

extract_elastic_templates <- function(km_object) {
  templates_obj <- km_object$templates
  
  if (is.null(templates_obj)) {
    stop("Elastic clustering output does not contain 'templates'.")
  }
  
  if (is.list(templates_obj)) {
    template_matrix <- sapply(templates_obj, function(x) as.numeric(x))
    if (is.vector(template_matrix)) {
      template_matrix <- matrix(template_matrix, ncol = 1)
    }
    return(as.matrix(template_matrix))
  }
  
  if (length(dim(templates_obj)) == 3) {
    dims <- dim(templates_obj)
    
    if (dims[1] == 1) {
      return(t(templates_obj[1, , , drop = TRUE]))
    }
    
    if (dims[3] == 1) {
      return(as.matrix(templates_obj[, , 1, drop = TRUE]))
    }
    
    if (dims[2] == 1) {
      return(as.matrix(templates_obj[, 1, , drop = TRUE]))
    }
    
    stop("Unable to infer template dimensions from a 3D templates array.")
  }
  
  if (is.matrix(templates_obj)) {
    return(as.matrix(templates_obj))
  }
  
  stop("Unsupported structure for elastic templates.")
}

extract_aligned_cluster_curves <- function(km_object, cluster_index) {
  candidate_names <- c("fn", "fna", "aligned", "aligned_functions")
  
  for (nm in candidate_names) {
    if (!is.null(km_object[[nm]])) {
      out <- km_object[[nm]][[cluster_index]]
      if (!is.null(out)) {
        return(as.matrix(out))
      }
    }
  }
  
  return(NULL)
}

compute_l2_centroids_from_labels <- function(data_matrix, cluster_labels) {
  k_values <- sort(unique(cluster_labels))
  
  centroid_matrix <- sapply(k_values, function(k) {
    colMeans(data_matrix[cluster_labels == k, , drop = FALSE], na.rm = TRUE)
  })
  
  if (is.vector(centroid_matrix)) {
    centroid_matrix <- matrix(centroid_matrix, ncol = 1)
  }
  
  as.matrix(centroid_matrix)
}

order_clusters_by_middle_value <- function(centroid_matrix) {
  middle_index <- ceiling(nrow(centroid_matrix) / 2)
  order(centroid_matrix[middle_index, ], decreasing = TRUE)
}

run_elastic_kmeans <- function(curve_matrix, time_grid_scaled, k_clusters, seed = 1234) {
  if (!is.matrix(curve_matrix)) {
    curve_matrix <- as.matrix(curve_matrix)
  }
  
  if (nrow(curve_matrix) < k_clusters) {
    stop(
      paste(
        "Cannot run elastic k-means with",
        k_clusters, "clusters on", nrow(curve_matrix), "curves."
      )
    )
  }
  
  set.seed(seed)
  
  fit <- fdasrvf::kmeans_align(
    f = t(curve_matrix),
    time = time_grid_scaled,
    K = k_clusters
  )
  
  labels <- extract_elastic_labels(fit)
  templates <- extract_elastic_templates(fit)
  
  if (nrow(templates) != ncol(curve_matrix) && ncol(templates) == ncol(curve_matrix)) {
    templates <- t(templates)
  }
  
  if (nrow(templates) != ncol(curve_matrix)) {
    stop("Elastic templates are not aligned with the time grid dimension.")
  }
  
  list(
    fit = fit,
    labels = labels,
    templates = templates
  )
}

bootstrap_l2_cluster_bands <- function(curve_matrix, labels, B = 1000, alpha = 0.05, seed = 1234) {
  k_values <- sort(unique(labels))
  out <- vector("list", length(k_values))
  names(out) <- as.character(k_values)
  
  for (j in seq_along(k_values)) {
    k <- k_values[j]
    Yk <- curve_matrix[labels == k, , drop = FALSE]
    nk <- nrow(Yk)
    
    if (nk < 2) {
      mean_curve <- colMeans(Yk, na.rm = TRUE)
      out[[j]] <- list(
        mean = mean_curve,
        lower = mean_curve,
        upper = mean_curve,
        n = nk
      )
      next
    }
    
    boot_centroids <- matrix(NA_real_, nrow = B, ncol = ncol(curve_matrix))
    
    set.seed(seed + k)
    for (b in seq_len(B)) {
      idx <- sample.int(nk, size = nk, replace = TRUE)
      boot_centroids[b, ] <- colMeans(Yk[idx, , drop = FALSE], na.rm = TRUE)
    }
    
    out[[j]] <- list(
      mean = colMeans(Yk, na.rm = TRUE),
      lower = apply(boot_centroids, 2, stats::quantile, probs = alpha / 2, na.rm = TRUE),
      upper = apply(boot_centroids, 2, stats::quantile, probs = 1 - alpha / 2, na.rm = TRUE),
      n = nk
    )
  }
  
  out
}

bootstrap_elastic_cluster_bands <- function(curve_matrix,
                                            labels,
                                            fit_object,
                                            B = 1000,
                                            alpha = 0.05,
                                            seed = 1234) {
  
  k_values <- sort(unique(labels))
  out <- vector("list", length(k_values))
  names(out) <- as.character(k_values)
  
  for (j in seq_along(k_values)) {
    
    k <- k_values[j]
    
    # curve del cluster k (non allineate, solo per dimensioni)
    Yk <- curve_matrix[labels == k, , drop = FALSE]
    nk <- nrow(Yk)
    
    # 👉 QUI LA COSA IMPORTANTE:
    # uso le curve GIÀ ALLINEATE da fdasrvf
    fk <- extract_aligned_cluster_curves(fit_object, k)  # M x nk
    
    if (is.null(fk)) {
      stop(paste("Aligned curves not found for cluster", k))
    }
    
    boot_centroids <- matrix(NA, nrow = B, ncol = nrow(fk))
    
    set.seed(seed + k)
    
    for (b in 1:B) {
      idx <- sample(1:nk, size = nk, replace = TRUE)
      
      # 👉 MEDIA SOLO SU CURVE ALLINEATE
      boot_centroids[b, ] <- rowMeans(fk[, idx, drop = FALSE])
    }
    
    out[[j]] <- list(
      mean  = rowMeans(fk),
      lower = apply(boot_centroids, 2, quantile, probs = alpha / 2),
      upper = apply(boot_centroids, 2, quantile, probs = 1 - alpha / 2),
      n = nk
    )
  }
  
  out
}

##############################
# 5. Figure 14
#    Conditional elastic clustering within BIT groups
##############################

run_elastic_kmeans_robust <- function(curve_matrix,
                                      time_grid_scaled,
                                      k_clusters,
                                      seeds = c(1234, 4321, 9876, 2468, 1357),
                                      allow_k_reduction = TRUE) {
  if (!is.matrix(curve_matrix)) {
    curve_matrix <- as.matrix(curve_matrix)
  }
  
  if (nrow(curve_matrix) < 2) {
    stop("At least 2 curves are required.")
  }
  
  k_try_values <- k_clusters
  if (allow_k_reduction) {
    k_try_values <- seq(from = min(k_clusters, nrow(curve_matrix) - 1), to = 2, by = -1)
  }
  
  last_error <- NULL
  
  for (k_try in k_try_values) {
    for (s in seeds) {
      fit_try <- try(
        run_elastic_kmeans(
          curve_matrix = curve_matrix,
          time_grid_scaled = time_grid_scaled,
          k_clusters = k_try,
          seed = s
        ),
        silent = TRUE
      )
      
      if (!inherits(fit_try, "try-error")) {
        return(list(
          fit = fit_try$fit,
          labels = fit_try$labels,
          templates = fit_try$templates,
          k_used = k_try,
          seed_used = s
        ))
      }
      
      last_error <- fit_try
    }
  }
  
  stop(
    paste(
      "Elastic clustering failed for all attempted K values and seeds.",
      "Last error:", as.character(last_error)
    )
  )
}

figure_14_results <- vector("list", length(group_names))
names(figure_14_results) <- group_names

for (grp in group_names) {
  clean_obs_grp  <- conditional_group_data[[grp]]$clean_obs
  clean_fine_grp <- conditional_group_data[[grp]]$clean_fine
  
  k_grp <- figure_13_results[[grp]]$k_final
  
  # IMPORTANT:
  # Elastic clustering is run on the fine grid, not on the 11 observed points.
  elastic_fit_grp <- run_elastic_kmeans_robust(
    curve_matrix = clean_fine_grp,
    time_grid_scaled = time_scaled_fine,
    k_clusters = k_grp,
    seeds = c(1234, 4321, 9876, 2468, 1357),
    allow_k_reduction = TRUE
  )
  
  figure_14_results[[grp]] <- list(
    group_name = grp,
    k_target = k_grp,
    k_used = elastic_fit_grp$k_used,
    seed_used = elastic_fit_grp$seed_used,
    clean_obs = clean_obs_grp,
    clean_fine = clean_fine_grp,
    elastic_labels = elastic_fit_grp$labels,
    elastic_templates_fine = elastic_fit_grp$templates,
    fit_object = elastic_fit_grp$fit
  )
}

plot_figure_14 <- function() {
  graphics::par(mfrow = c(2, 2))
  graphics::par(mar = c(5, 5, 4, 1.5) + 0.1)
  
  ylim_global <- range(mat_monotone_fine, na.rm = TRUE)
  
  for (grp in group_names) {
    res_grp <- figure_14_results[[grp]]
    k_grp <- res_grp$k_used
    cols_grp <- cluster_palette[seq_len(k_grp)]
    
    graphics::plot(
      x = time_fine,
      y = res_grp$clean_fine[1, ],
      type = "n",
      ylim = ylim_global,
      xaxt = "n",
      xlab = "Days",
      ylab = "f(t)",
      main = paste0("Elastic clustering, ", grp, " (K=", k_grp, ")")
    )
    
    graphics::axis(1, at = time_days, labels = x_labels)
    
    for (i in seq_len(nrow(res_grp$clean_fine))) {
      graphics::lines(
        x = time_fine,
        y = res_grp$clean_fine[i, ],
        lty = 3,
        lwd = 0.8,
        col = safe_adjustcolor(cols_grp[res_grp$elastic_labels[i]], alpha = 0.35)
      )
    }
    
    for (k in seq_len(k_grp)) {
      graphics::lines(
        x = time_fine,
        y = res_grp$elastic_templates_fine[, k],
        lwd = 3,
        lty = 1,
        col = cols_grp[k]
      )
    }
    
    graphics::legend(
      "topright",
      legend = paste("Sub-cluster", seq_len(k_grp)),
      col = cols_grp,
      lty = 1,
      lwd = 3,
      bty = "o",
      cex = 0.8
    )
  }
}

save_base_plot_dual(
  filename_no_ext = "Figure 14",
  script_dir = script_dir,
  plot_fun = plot_figure_14,
  width_png = 2480,   # A4 a 300 dpi
  height_png = 3508,
  width_pdf = 8.27,
  height_pdf = 11.69,
  res_png = 300
)





###############################################################################
######################## CONDITIONAL ARI BY TEMPERAMENT ########################
###############################################################################

##############################
# 1. Safety checks
##############################
required_objects_ari <- c(
  "figure_13_results",
  "figure_14_results",
  "group_names"
)

missing_objects_ari <- required_objects_ari[
  !vapply(required_objects_ari, exists, logical(1))
]

if (length(missing_objects_ari) > 0) {
  stop(
    paste(
      "The following objects must already exist before computing conditional ARI:",
      paste(missing_objects_ari, collapse = ", ")
    )
  )
}

##############################
# 2. Compute ARI by group
##############################
ari_by_group <- data.frame(
  Group = character(0),
  K_L2 = integer(0),
  K_Elastic = integer(0),
  ARI = numeric(0),
  N = integer(0),
  stringsAsFactors = FALSE
)

contingency_tables_by_group <- vector("list", length(group_names))
names(contingency_tables_by_group) <- group_names

for (grp in group_names) {
  
  l2_labels <- figure_13_results[[grp]]$cluster_labels
  elastic_labels <- figure_14_results[[grp]]$elastic_labels
  
  if (length(l2_labels) != length(elastic_labels)) {
    stop(
      paste(
        "Length mismatch between L2 and elastic labels for group:",
        grp
      )
    )
  }
  
  contingency_tables_by_group[[grp]] <- table(
    L2 = l2_labels,
    Elastic = elastic_labels
  )
  
  ari_value <- mclust::adjustedRandIndex(
    l2_labels,
    elastic_labels
  )
  
  ari_by_group <- rbind(
    ari_by_group,
    data.frame(
      Group = grp,
      K_L2 = figure_13_results[[grp]]$k_final,
      K_Elastic = figure_14_results[[grp]]$k_used,
      ARI = ari_value,
      N = length(l2_labels),
      stringsAsFactors = FALSE
    )
  )
}

##############################
# 3. Console output
##############################
cat("\nConditional comparison between L2 and elastic clustering by temperament\n")
cat("\nAdjusted Rand Index by group:\n")
print(ari_by_group)

cat("\nContingency tables by group:\n")
for (grp in group_names) {
  cat("\n", grp, "\n", sep = "")
  print(contingency_tables_by_group[[grp]])
}

##############################
# 4. Store outputs
##############################
ari_conditional_by_group <- ari_by_group
contingency_conditional_by_group <- contingency_tables_by_group



##############################
# 6. Figure 15
#    Global L2 vs elastic clustering with bootstrap bands
##############################
K_global <- 5
B_l2 <- 1000
B_elastic <- 300
alpha_bands <- 0.05

# Global L2 clustering on the observed monotone grid
set.seed(4321)
kmeans_global_l2 <- stats::kmeans(
  x = mat_monotone_obs,
  centers = K_global,
  nstart = 100,
  iter.max = 300
)

cl_global_l2 <- kmeans_global_l2$cluster
centroids_global_l2 <- t(kmeans_global_l2$centers)

# Global elastic clustering on the same observed monotone grid
elastic_global <- run_elastic_kmeans(
  curve_matrix = mat_monotone_obs,
  time_grid_scaled = time_scaled_obs,
  k_clusters = K_global,
  seed = 9876
)

cl_global_elastic <- elastic_global$labels
centroids_global_elastic <- elastic_global$templates

# Bootstrap bands
bands_global_l2 <- bootstrap_l2_cluster_bands(
  curve_matrix = mat_monotone_obs,
  labels = cl_global_l2,
  B = B_l2,
  alpha = alpha_bands,
  seed = 500
)

bands_global_elastic <- bootstrap_elastic_cluster_bands(
  curve_matrix = mat_monotone_obs,
  labels = cl_global_elastic,
  fit_object = elastic_global$fit,
  B = B_elastic,
  alpha = alpha_bands,
  seed = 800
)

# Order clusters by central height for visual readability
l2_order <- order(
  sapply(bands_global_l2, function(x) x$mean[ceiling(length(x$mean) / 2)]),
  decreasing = TRUE
)

elastic_order <- order(
  sapply(bands_global_elastic, function(x) x$mean[ceiling(length(x$mean) / 2)]),
  decreasing = TRUE
)

plot_figure_15 <- function() {
  graphics::par(mfrow = c(1, 2))
  graphics::par(mar = c(5, 5, 4, 2) + 0.1)
  
  ylim_all <- range(
    unlist(lapply(bands_global_l2, function(x) c(x$lower, x$upper))),
    unlist(lapply(bands_global_elastic, function(x) c(x$lower, x$upper))),
    na.rm = TRUE
  )
  
  ########################################
  # Left panel, L2
  ########################################
  graphics::plot(
    x = time_days,
    y = bands_global_l2[[l2_order[1]]]$mean,
    type = "n",
    ylim = ylim_all,
    xaxt = "n",
    xlab = "Days",
    ylab = "f(t)",
    main = "L2 centroids with bootstrap bands"
  )
  
  graphics::axis(1, at = time_days, labels = x_labels)
  
  for (i in seq_along(l2_order)) {
    idx <- l2_order[i]
    
    graphics::polygon(
      x = c(time_days, rev(time_days)),
      y = c(bands_global_l2[[idx]]$upper, rev(bands_global_l2[[idx]]$lower)),
      col = safe_adjustcolor(cluster_palette[i], alpha = 0.22),
      border = NA
    )
    
    graphics::lines(
      x = time_days,
      y = bands_global_l2[[idx]]$mean,
      lwd = 2.5,
      lty = 1,
      col = cluster_palette[i]
    )
  }
  
  graphics::legend(
    "topright",
    legend = paste("Rank", seq_len(K_global)),
    col = cluster_palette[seq_len(K_global)],
    lty = 1,
    lwd = 2.5,
    bty = "o",
    cex = 0.85,
    title = "Order, high to low"
  )
  
  ########################################
  # Right panel, elastic
  ########################################
  graphics::plot(
    x = time_days,
    y = bands_global_elastic[[elastic_order[1]]]$mean,
    type = "n",
    ylim = ylim_all,
    xaxt = "n",
    xlab = "Days",
    ylab = "f(t)",
    main = "Elastic SRVF centroids with bootstrap bands"
  )
  
  graphics::axis(1, at = time_days, labels = x_labels)
  
  for (i in seq_along(elastic_order)) {
    idx <- elastic_order[i]
    
    graphics::polygon(
      x = c(time_days, rev(time_days)),
      y = c(bands_global_elastic[[idx]]$upper, rev(bands_global_elastic[[idx]]$lower)),
      col = safe_adjustcolor(cluster_palette[i], alpha = 0.22),
      border = NA
    )
    
    graphics::lines(
      x = time_days,
      y = bands_global_elastic[[idx]]$mean,
      lwd = 2.5,
      lty = 2,
      col = cluster_palette[i]
    )
  }
  
  graphics::legend(
    "topright",
    legend = paste("Rank", seq_len(K_global)),
    col = cluster_palette[seq_len(K_global)],
    lty = 2,
    lwd = 2.5,
    bty = "o",
    cex = 0.85,
    title = "Order, high to low"
  )
}

save_base_plot_dual(
  filename_no_ext = "Figure 15",
  script_dir = script_dir,
  plot_fun = plot_figure_15,
  width_png = 2600,
  height_png = 1300,
  width_pdf = 12,
  height_pdf = 6.5,
  res_png = 300
)

##############################
# 7. Agreement diagnostics
##############################
tab_global_l2_vs_elastic <- table(
  L2 = cl_global_l2,
  Elastic = cl_global_elastic
)

ari_global_l2_vs_elastic <- mclust::adjustedRandIndex(
  cl_global_l2,
  cl_global_elastic
)

cat("\nGlobal comparison between L2 and elastic clustering\n")
cat("\nContingency table:\n")
print(tab_global_l2_vs_elastic)

cat("\nAdjusted Rand Index:\n")
print(ari_global_l2_vs_elastic)

##############################
# 8. Store outputs for the manuscript and checks
##############################
figure_14_output <- figure_14_results

figure_15_output <- list(
  K_global = K_global,
  cl_global_l2 = cl_global_l2,
  cl_global_elastic = cl_global_elastic,
  centroids_global_l2 = centroids_global_l2,
  centroids_global_elastic = centroids_global_elastic,
  bands_global_l2 = bands_global_l2,
  bands_global_elastic = bands_global_elastic,
  contingency_table = tab_global_l2_vs_elastic,
  ari_global = ari_global_l2_vs_elastic
)

cat("\nElastic robustness figures exported successfully.\n")
cat("Files created:\n")
cat(" - Figure 14.png / Figure 14.pdf\n")
cat(" - Figure 15.png / Figure 15.pdf\n")




##############################
# 9. FINAL CHECK OF VARIABILITY
##############################

png("elastic_global_clusters_check.png", width = 2400, height = 1600, res = 300)

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

for (k in sort(unique(cl_global_elastic))) {
  fk <- extract_aligned_cluster_curves(elastic_global$fit, k)
  
  matplot(
    time_days,
    fk,
    type = "l",
    lty = 1,
    lwd = 1,
    col = grDevices::adjustcolor(cluster_palette[k], alpha.f = 0.35),
    xlab = "Days",
    ylab = "f(t)",
    main = paste("Elastic global cluster", k),
    xaxt = "n"
  )
  
  axis(1, at = time_days, labels = x_labels)
  
  lines(
    time_days,
    rowMeans(fk, na.rm = TRUE),
    lwd = 3,
    lty = 2,
    col = cluster_palette[k]
  )
}

dev.off()








###############################################################################
######################## GLOBAL ARI: L2 vs ELASTIC #############################
###############################################################################

##############################
# 1. Required package
##############################
if (!requireNamespace("mclust", quietly = TRUE)) {
  install.packages("mclust")
}

library(mclust)

##############################
# 2. Extract cluster labels
##############################
# ATTENZIONE: questi oggetti devono esistere nel tuo ambiente

# L2 clustering (Figure 15 left)
cl_l2_global <- cl_global_l2

# Elastic clustering (Figure 15 right)
cl_elastic_global <- cl_global_elastic

##############################
# 3. Sanity check
##############################
if (length(cl_l2_global) != length(cl_elastic_global)) {
  stop("Cluster vectors must have the same length.")
}

##############################
# 4. Adjusted Rand Index
##############################
ari_global <- adjustedRandIndex(cl_l2_global, cl_elastic_global)

##############################
# 5. Contingency table
##############################
contingency_global <- table(
  L2 = cl_l2_global,
  Elastic = cl_elastic_global
)

##############################
# 6. Console output
##############################
cat("\nGlobal comparison between L2 and elastic clustering\n")

cat("\nAdjusted Rand Index:\n")
print(ari_global)

cat("\nContingency table:\n")
print(contingency_global)

##############################
# 7. Store outputs
##############################
ari_unconditional <- ari_global
contingency_unconditional <- contingency_global




