#' @title Plot Method for MLDFM object
#'
#' @description Dispatches to specific plot functions for factors, loadings, or residuals.
#'
#' @param x An object of class \code{mldfm}.
#' @param which What to plot: one of \code{"factors"} (default), \code{"loadings"}, or \code{"residuals"}.
#' @param dates Optional vector of dates (as \code{Date} or \code{zoo::yearqtr}) to use for the x-axis. If not provided, a simple index (1:N) is used.
#' @param var_names Optional vector of variable names to label loadings and residual axis.
#' @param flip Optional vector of length equal to the number of factors. Set 1 to flip sign for a specific factor (and related loadings); 0 to leave unchanged.
#' @param fpr Logical. If \code{TRUE}, uses FPR Gamma (Fresoli, Poncela, Ruiz, 2024); otherwise, uses standard time-varying Gamma.
#' @param ... Additional arguments (ignored)
#' 
#' @return No return value. Called for plots generation.
#' 
#' @method plot mldfm
#' 
#' @export
plot.mldfm <- function(x, which = "factors", dates = NULL, var_names = NULL, flip = NULL, fpr = FALSE, ...) {
  
  stopifnot(inherits(x, "mldfm"))
  if (!is.logical(fpr) || length(fpr) != 1) stop("fpr must be a logical value (TRUE or FALSE).")
  
  
  which <- match.arg(tolower(which), c("factors", "loadings", "residuals"))
  
  switch(which,
         "factors"   = plot_factors.mldfm(x, dates = dates, flip = flip, fpr, ...),
         "loadings"  = plot_loadings.mldfm(x, var_names = var_names, flip = flip, ...),
         "residuals" = plot_residuals.mldfm(x, var_names = var_names, ...)
  )
}


#' @title Plot Method for \code{mldfm_subsample} Object
#'
#' @description Plots a histogram of the number of iterations used in each subsample estimation.
#'
#' @param x An object of class \code{mldfm_subsample}.
#' @param ... Additional arguments (ignored).
#'
#' @return A ggplot object (invisibly).
#'
#' @importFrom ggplot2 ggplot aes geom_histogram labs theme_minimal
#' @importFrom ggplot2 element_text
#' @importFrom rlang .data
#' @method plot mldfm_subsample
#' @export
plot.mldfm_subsample <- function(x, ...) {
  stopifnot(inherits(x, "mldfm_subsample"))
  
  iterations <- sapply(x$models, function(m) m$iterations)
  df <- data.frame(Iterations = iterations)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Iterations)) +
    ggplot2::geom_histogram(binwidth = 1, fill = "steelblue", color = "white", ...) +
    ggplot2::labs(
      title = "Sequential Least Squares Iterations",
      x = "Number of Iterations",
      y = "Frequency"
    ) +
    ggplot2::theme_minimal()
  
  print(p)
  invisible(p)
}


#' @title Plot Method for \code{fars} Object
#'
#' @description Generates line plots of estimated quantiles from a \code{fars} object. 
#' If a stressed scenario is available, it is plotted in a separate panel.
#'
#' @param x An object of class \code{fars}.
#' @param dates Optional vector of dates (as \code{Date} or \code{zoo::yearqtr}) to use for the x-axis. 
#' If not provided, a simple index is used.
#' @param ... Additional arguments (ignored).
#'
#' @return No return value. Called for plot generation.
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom rlang .data
#' @method plot fars
#' @export
plot.fars <- function(x, dates = NULL, ...) {
  stopifnot(inherits(x, "fars"))
  
  # Prepare data
  quantiles <- x$quantiles
  scenario <- x$stressed_quantiles
  levels <- x$levels
  
  if (is.null(dates)) {
    dates <- 1:nrow(quantiles)
  }
  
  # Compute global min and max
  y_min <- min(quantiles, na.rm = TRUE)
  y_max <- max(quantiles, na.rm = TRUE)
  if (!is.null(scenario)) {
    y_min <- min(y_min, min(scenario, na.rm = TRUE))
    y_max <- max(y_max, max(scenario, na.rm = TRUE))
  }
  y_range <- c(y_min, y_max)
  
  # --- Forecasted Quantiles ---
  df <- as.data.frame(quantiles)
  colnames(df) <- paste0("Q", levels)
  df$Time <- dates
  df_long <- reshape2::melt(df, id.vars = "Time", variable.name = "Quantile", value.name = "Value")
  
  p_main <- ggplot(df_long, aes(x = .data$Time, y = .data$Value, color = .data$Quantile)) +
    geom_line(linewidth = 1) +
    labs(title = "Quantiles",
         y = "Value", x = "Time") +
    scale_y_continuous(limits = y_range) +
    theme_minimal()
  
  print(p_main)
  
  # --- Stressed Quantiles (if available) ---
  if (!is.null(scenario)) {
    df_s <- as.data.frame(scenario)
    colnames(df_s) <- paste0("Q", levels)
    df_s$Time <- dates
    df_s_long <- reshape2::melt(df_s, id.vars = "Time", variable.name = "Quantile", value.name = "Value")
    
    p_stress <- ggplot(df_s_long, aes(x = .data$Time, y = .data$Value, color = .data$Quantile)) +
      geom_line(linewidth = 1) +
      labs(title = "Stressed Quantiles",
           y = "Value", x = "Time") +
      scale_y_continuous(limits = y_range) +
      theme_minimal()
    
    print(p_stress)
  }
}


#' Plot Method for \code{fars_scenario} Object
#'
#' @description Plots the hyperellipsoid for a given time observation (only for 1D or 2D cases). 
#'
#' @param x An object of class \code{fars_scenario}.
#' @param obs Integer. Time index to plot (default = 1).
#' @param ... Additional arguments (ignored).
#'
#' @method plot fars_scenario
#' @importFrom graphics segments points axis
#' @export
plot.fars_scenario <- function(x, obs = 1, ...) {
  stopifnot(inherits(x, "fars_scenario"))
  
  K <- ncol(x$center)
  T <- x$periods
  if (obs < 1 || obs > T) stop("Invalid observation index: out of bounds.")
  
  center <- x$center[obs, ]
  shape <- x$ellipsoids[[obs]]
  
  if (K == 1) {
    lower <- shape[1]
    upper <- shape[2]
    
    plot(NA,
         xlim = c(lower, upper),
         ylim = c(0.9, 1.1),
         xlab = "Factor",
         ylab = "",
         yaxt = "n",
         xaxt = "n",
         main = paste("1D Confidence Interval (t =", obs, ")"))
    
    segments(lower, 1, upper, 1, col = "lightblue", lwd = 3)
    points(center, 1, pch = 19, col = "darkblue")
    
    axis(1,
         at = round(c(lower, center, upper), 3),
         labels = round(c(lower, center, upper), 3))
  } else if (K == 2) {
    
    plot(shape, type = "l", col = "lightblue", lwd = 2,
         xlab = "Factor 1", ylab = "Factor 2", asp = 1,
         main = paste("2D Ellipsoid (t =", obs, ")"), ...)
    points(center[1], center[2], pch = 19, col = "darkblue")
  } else {
    warning("Plotting is only supported for 1D and 2D cases")
  }
}


#' @title Plot Method for \code{fars_density} Object
#'
#' @description Plots the evolution of the estimated density over time as a 3D surface.
#'
#' @param x An object of class \code{fars_density}.
#' @param time_index Optional vector for the time axis (default is 1:nrow).
#' @param ... Additional arguments (ignored).
#'
#' @importFrom plotly plot_ly layout
#' 
#' @method plot fars_density
#' @export
plot.fars_density <- function(x, time_index = NULL, ...) {
  stopifnot(inherits(x, "fars_density"))
  
  
  # Extract components
  density_matrix <- x$density
  x_vals <- x$eval_points
  n_time <- nrow(density_matrix)
  n_points <- ncol(density_matrix)
  
  # Time axis
  if (is.null(time_index)) {
    time_index <- seq_len(n_time)
  }
  
  # Create meshgrid
  z_matrix <- density_matrix
  x_axis <- x_vals
  y_axis <- time_index
  
  # Create plot
  plotly::plot_ly(
    x = ~x_axis,
    y = ~y_axis,
    z = ~z_matrix,
    type = "surface",
    colorscale = "Viridis"
  ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = "Evaluation points"),
        yaxis = list(title = "Time"),
        zaxis = list(title = "Density"),
        camera = list(eye = list(x = 1.25, y = -1.25, z = 1))
      ),
      title = "Density over Time"
    )
}


#' @title Plot Factors from \code{mldfm} Object
#'
#' @description Displays time series plots of the estimated factors with 95% confidence bands.
#'
#' @param x An object of class \code{mldfm}.
#' @param dates Optional vector of dates. If NULL, uses 1:n as default.
#' @param flip Optional vector of length equal to the number of factors. Set 1 to flip sign for a specific factor (and related loadings); 0 to leave unchanged.
#' @param fpr Logical. If \code{TRUE}, uses FPR Gamma (Fresoli, Poncela, Ruiz, 2024); otherwise, uses standard time-varying Gamma.
#' @param ... Additional arguments (ignored).
#' 
#' @importFrom dplyr mutate filter
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon ggtitle coord_cartesian theme_bw theme element_blank element_text scale_y_continuous
#' @importFrom MASS ginv
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @keywords internal
plot_factors.mldfm <- function(x, dates = NULL, flip = NULL, fpr = FALSE, ...) {
  stopifnot(inherits(x, "mldfm"))
  
  factors   <- get_factors(x)
  loadings  <- get_loadings(x)
  residuals <- get_residuals(x)
  
  T_obs  <- nrow(residuals)
  N_vars <- ncol(residuals)
  r      <- ncol(factors)
  
  
  # handle sign flips 
  if (!is.null(flip)) {
    if (length(flip) != r) {
      stop("`flip` must have length equal to the number of factors (", r, ").")
    }
    if (!all(flip %in% c(0,1,TRUE,FALSE))) {
      stop("`flip` must contain only 0 or 1.")
    }
    
    # build sign multipliers: -1 = flip, +1 = keep
    s <- ifelse(as.integer(flip) == 1L, -1, 1)
    
    factors  <- sweep(factors,  2, s, `*`)
    loadings <- sweep(loadings, 2, s, `*`)
    
  }
  
  # Compute standard deviation for confidence bands
  SD <- vector("list", T_obs)
  PP <- solve(crossprod(loadings) / N_vars)
  
  # Compute FPR gamma if needed
  if(fpr){
    gamma <- compute_fpr_gamma(residuals, loadings)
  }
  
  for (t in seq_len(T_obs)) {
    if(!fpr){
      d <- residuals[t, ]^2
      gamma <- crossprod(loadings, loadings * d) / N_vars
    }
    MSE <- PP %*% (gamma / N_vars) %*% PP
    SD[[t]] <- sqrt(diag(MSE))
  }
  
  # Convert to matrix form
  SD_mat <- do.call(rbind, SD)                  
  colnames(SD_mat) <- paste0("F", seq_len(ncol(SD_mat)))  
  
  # Factor names
  keys         <- names(x$factors_list)
  values       <- unlist(x$factors_list)
  factor_names <- unlist(mapply(function(key, val) {
    clean <- paste0("F", gsub("-", "", key))
    if (val > 1) paste0(clean, "n", seq_len(val)) else clean
  }, keys, values, SIMPLIFY = FALSE))
  if (is.null(factor_names) || length(factor_names) != ncol(factors)) {
    factor_names <- paste0("F", seq_len(ncol(factors)))
  }
  colnames(factors) <- factor_names
  colnames(SD_mat)  <- factor_names
  
  if (is.null(dates)) dates <- seq_len(nrow(factors))
  
  
  factors_long <- as.data.frame(factors) %>%
    mutate(Date = as.Date(dates)) %>%
    pivot_longer(cols = -.data$Date, names_to = "Factor", values_to = "Value")
  
  sd_long <- as.data.frame(SD_mat) %>%
    mutate(Date = as.Date(dates)) %>%
    pivot_longer(cols = -.data$Date, names_to = "Factor", values_to = "SD")
  
  # LB UB
  df_long <- factors_long %>%
    dplyr::left_join(sd_long, by = c("Date","Factor")) %>%
    mutate(LB = .data$Value - 2 * SD,
           UB = .data$Value + 2 * SD)
  
  # Same range
  y_min <- min(df_long$LB, na.rm = TRUE)
  y_max <- max(df_long$UB, na.rm = TRUE)
  
  # Plot
  for (factor_name in factor_names) {
    df_i <- df_long %>% filter(.data$Factor == factor_name)
    
    p <- ggplot(df_i, aes(x = .data$Date, y = .data$Value)) +
      geom_ribbon(aes(ymin = .data$LB, ymax = .data$UB), fill = "grey70", alpha = 0.3) +
      geom_line(color = "blue", alpha = 0.6) +
      geom_line(alpha = 0.8) +
      ggtitle(factor_name) +
      coord_cartesian(ylim = c(y_min, y_max)) +
      theme_bw() +
      theme(
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none"
      )
    
    print(p)
  }
  
  
  invisible(NULL)
}


#' @title Plot Loadings from \code{mldfm} Object
#'
#' @description Displays bar plots of the estimated factor loadings with 95% confidence intervals.
#'
#' @param x An object of class \code{mldfm}.
#' @param var_names Optional vector of variable names. If NULL, default names are used.
#' @param flip Optional vector of length equal to the number of factors. Set 1 to flip sign for a specific factor (and related loadings); 0 to leave unchanged.
#' @param ... Additional arguments (ignored).
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate filter
#' @importFrom ggplot2 ggplot geom_bar geom_errorbar geom_hline theme_bw theme element_blank element_text scale_y_continuous ggtitle coord_flip
#' @importFrom forcats fct_rev
#' @importFrom rlang .data
#'
#' @keywords internal
plot_loadings.mldfm <- function(x, var_names = NULL, flip = NULL, ...) {
  stopifnot(inherits(x, "mldfm"))
  
  factors   <- get_factors(x)
  loadings  <- get_loadings(x)
  residuals <- get_residuals(x)
  
  t <- nrow(residuals)
  N <- ncol(residuals)
  r      <- ncol(factors)
  
  
  # handle sign flips 
  if (!is.null(flip)) {
    if (length(flip) != r) {
      stop("`flip` must have length equal to the number of factors (", r, ").")
    }
    if (!all(flip %in% c(0,1,TRUE,FALSE))) {
      stop("`flip` must contain only 0 or 1.")
    }
    
    # build sign multipliers: -1 = flip, +1 = keep
    s <- ifelse(as.integer(flip) == 1L, -1, 1)
    
    factors  <- sweep(factors,  2, s, `*`)
    loadings <- sweep(loadings, 2, s, `*`)
    
  }
  
  loadings_df <- as.data.frame(loadings)
  
  # Factor names
  keys   <- names(x$factors_list)
  values <- unlist(x$factors_list)
  factor_names <- unlist(
    mapply(function(key, val) {
      clean <- paste0("F", gsub("-", "", key))
      if (val > 1) paste0(clean, "n", seq_len(val)) else clean
    }, keys, values, SIMPLIFY = FALSE)
  )
  colnames(loadings_df) <- factor_names
  
  # Variable names
  loadings_df$Variables <- if (is.null(var_names)) {
    paste0("Var", seq_len(nrow(loadings_df)))
  } else {
    var_names
  }
  
  loadings_df <- loadings_df[, c("Variables", setdiff(names(loadings_df), "Variables"))]
  
  # Long format
  loadings_long <- loadings_df %>%
    pivot_longer(cols = - .data$Variables, names_to = "Factor", values_to = "Loading")
  
  # Standard errors and CIs
  se_vector <- apply(residuals, 2, sd) / sqrt(t)
  
  loadings_long <- loadings_long %>%
    mutate(SE = rep(se_vector, times = length(unique(.data$Factor))),
           Loading_lower = .data$Loading - 1.96 * .data$SE,
           Loading_upper = .data$Loading + 1.96 * .data$SE)
  
  
  # Plot
  unique_factors <- unique(loadings_long$Factor)
  y_min <- -1
  y_max <- 1
  
  for (factor_name in unique_factors) {
    df_i <- loadings_long %>%
      filter(.data$Factor == factor_name & .data$Loading != 0) %>%
      mutate(Variables = factor(.data$Variables, levels = unique(.data$Variables)))
    
    p <- ggplot(df_i, aes(x = forcats::fct_rev(.data$Variables), y = .data$Loading)) +
      geom_bar(stat = "identity", fill = "grey", alpha = 0.7) +
      geom_hline(yintercept = 0, color = "red") +
      geom_errorbar(aes(ymin = .data$Loading_lower, ymax = .data$Loading_upper),
                    width = 0.5, color = "black", alpha = 1, linewidth = 0.2) +
      coord_flip() +
      theme_bw() +
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_y_continuous(limits = c(y_min, y_max)) +
      ggtitle(factor_name)
    
    
    print(p)
  }
  
  invisible(NULL)
}


#' @title Plot Residuals from \code{mldfm} Object
#'
#' @description Displays a correlation heatmap of the residuals.
#' 
#' @param x An object of class \code{mldfm}.
#' @param var_names Optional vector of variable names. If NULL, default names are used.
#' @param ... Additional arguments (ignored).
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_distiller labs theme_minimal theme element_text element_blank
#' @importFrom stats cor
#'
#' @keywords internal
plot_residuals.mldfm <- function(x, var_names = NULL, ...) {
  stopifnot(inherits(x, "mldfm"))
  
  residuals <- get_residuals(x)
  n_vars <- ncol(residuals)
  
  country_names <- if (is.null(var_names)) {
    paste0("Var", seq_len(n_vars))
  } else {
    var_names
  }
  
  corr_matrix <- cor(residuals)
  rownames(corr_matrix) <- country_names
  colnames(corr_matrix) <- country_names
  
  corr_df <- as.data.frame(as.table(corr_matrix))
  colnames(corr_df) <- c("Country1", "Country2", "Correlation")
  
  Country1 <- Country2 <- Correlation <- NULL
  
  g <- ggplot(corr_df, aes(x = Country1, y = Country2, fill = Correlation)) +
    geom_tile(color = "white", linewidth = 0.1) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1), name = "Correlation") +
    labs(title = "Residual Correlation Matrix",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
      axis.text.y = element_text(size = 8),
      panel.grid = element_blank(),
      axis.ticks = element_blank()
    )
  
  print(g)
  invisible(NULL)
}

