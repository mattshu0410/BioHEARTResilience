#' Plot CACS vs Average Normalized Risk Score with Resilience Classes
#'
#' Creates a scatter plot showing the relationship between CACS and average normalized
#' risk score, colored by resilience classification.
#'
#' @param data Data frame containing CACS, risk scores, and classifications
#' @param cacs_col Name of CACS column (default: "cacs")
#' @param risk_score_col Name of risk score column (default: "average_norm_score") 
#' @param class_col Name of classification column (default: "resilience_class")
#' @param log_transform Logical, whether to log-transform CACS (default: TRUE)
#' @param title Plot title
#' @param colors Named vector of colors for each class
#' @return ggplot object if ggplot2 is available, otherwise base R plot
#' @export
#' @examples
#' \dontrun{
#' # Basic plot
#' plot_cacs_vs_risk(combined_data)
#' 
#' # Custom colors and title
#' plot_cacs_vs_risk(combined_data, 
#'                   title = "Resilience Classification",
#'                   colors = c(resilient = "green", susceptible = "red"))
#' }
plot_cacs_vs_risk <- function(data,
                             cacs_col = "cacs",
                             risk_score_col = "average_norm_score",
                             class_col = "resilience_class",
                             log_transform = TRUE,
                             title = "CACS vs Average Normalized Risk Score",
                             colors = NULL) {
  
  # Validate inputs
  required_cols <- c(cacs_col, risk_score_col, class_col)
  validate_columns(data, required_cols)
  
  # Default colors
  if (is.null(colors)) {
    colors <- c(
      "resilient" = "#2E8B57",    # Sea green
      "reference" = "#4169E1",     # Royal blue  
      "susceptible" = "#DC143C",   # Crimson
      "other" = "#708090",         # Slate gray
      "missing" = "#D3D3D3"        # Light gray
    )
  }
  
  # Prepare data
  plot_data <- data.frame(
    cacs = data[[cacs_col]],
    risk_score = data[[risk_score_col]],
    class = data[[class_col]]
  )
  
  # Remove rows with missing essential data
  complete_cases <- complete.cases(plot_data$cacs, plot_data$risk_score)
  plot_data <- plot_data[complete_cases, ]
  
  if (nrow(plot_data) == 0) {
    stop("No complete cases available for plotting")
  }
  
  # Transform CACS if requested
  if (log_transform) {
    plot_data$cacs_plot <- log(plot_data$cacs + 1)
    y_label <- "log(CACS + 1)"
  } else {
    plot_data$cacs_plot <- plot_data$cacs
    y_label <- "CACS"
  }
  
  # Check if ggplot2 is available
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    # Create ggplot
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = risk_score, y = cacs_plot, color = class)) +
      ggplot2::geom_point(alpha = 0.7, size = 2) +
      ggplot2::scale_color_manual(values = colors, name = "Classification") +
      ggplot2::labs(
        title = title,
        x = "Average Normalized Risk Score",
        y = y_label
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
        legend.title = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12)
      )
    
    return(p)
    
  } else {
    # Fallback to base R plotting
    warning("ggplot2 not available, using base R plotting")
    
    # Set up colors for base R
    class_levels <- levels(plot_data$class)
    if (is.null(class_levels)) {
      class_levels <- unique(plot_data$class)
    }
    
    plot_colors <- colors[class_levels]
    plot_colors[is.na(plot_colors)] <- "black"  # Default for missing colors
    
    # Create base plot
    plot(plot_data$risk_score, plot_data$cacs_plot,
         col = plot_colors[plot_data$class],
         pch = 16,
         xlab = "Average Normalized Risk Score",
         ylab = y_label,
         main = title)
    
    # Add legend
    legend("topright", 
           legend = class_levels,
           col = plot_colors,
           pch = 16,
           title = "Classification")
    
    return(invisible(NULL))
  }
}

#' Plot risk score distributions by resilience class
#'
#' Creates density or histogram plots showing the distribution of risk scores
#' for each resilience class.
#'
#' @param data Data frame containing risk scores and classifications
#' @param risk_score_col Name of risk score column (default: "average_norm_score")
#' @param class_col Name of classification column (default: "resilience_class")
#' @param plot_type Type of plot ("density" or "histogram")
#' @param title Plot title
#' @param colors Named vector of colors for each class
#' @return ggplot object if ggplot2 is available, otherwise base R plot
#' @export
plot_risk_distribution <- function(data,
                                  risk_score_col = "average_norm_score",
                                  class_col = "resilience_class", 
                                  plot_type = c("density", "histogram"),
                                  title = "Risk Score Distribution by Resilience Class",
                                  colors = NULL) {
  
  plot_type <- match.arg(plot_type)
  
  # Validate inputs
  required_cols <- c(risk_score_col, class_col)
  validate_columns(data, required_cols)
  
  # Default colors
  if (is.null(colors)) {
    colors <- c(
      "resilient" = "#2E8B57",
      "reference" = "#4169E1", 
      "susceptible" = "#DC143C",
      "other" = "#708090",
      "missing" = "#D3D3D3"
    )
  }
  
  # Prepare data
  plot_data <- data.frame(
    risk_score = data[[risk_score_col]],
    class = data[[class_col]]
  )
  
  # Remove missing risk scores
  plot_data <- plot_data[!is.na(plot_data$risk_score), ]
  
  if (nrow(plot_data) == 0) {
    stop("No valid risk scores available for plotting")
  }
  
  # Check if ggplot2 is available
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = risk_score, fill = class))
    
    if (plot_type == "density") {
      p <- p + ggplot2::geom_density(alpha = 0.7)
    } else {
      p <- p + ggplot2::geom_histogram(alpha = 0.7, position = "identity", bins = 30)
    }
    
    p <- p +
      ggplot2::scale_fill_manual(values = colors, name = "Classification") +
      ggplot2::labs(
        title = title,
        x = "Average Normalized Risk Score",
        y = if (plot_type == "density") "Density" else "Count"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
        legend.title = ggplot2::element_text(size = 12)
      )
    
    return(p)
    
  } else {
    # Base R fallback
    warning("ggplot2 not available, using base R plotting")
    
    classes <- unique(plot_data$class)
    
    if (plot_type == "density") {
      # Calculate density for each class
      xlim_range <- range(plot_data$risk_score)
      ylim_max <- 0
      
      densities <- list()
      for (class in classes) {
        class_data <- plot_data$risk_score[plot_data$class == class]
        if (length(class_data) > 1) {
          densities[[class]] <- stats::density(class_data)
          ylim_max <- max(ylim_max, max(densities[[class]]$y))
        }
      }
      
      # Create plot
      plot(1, type = "n", 
           xlim = xlim_range, 
           ylim = c(0, ylim_max),
           xlab = "Average Normalized Risk Score",
           ylab = "Density",
           main = title)
      
      # Add density lines
      for (i in seq_along(classes)) {
        if (classes[i] %in% names(densities)) {
          lines(densities[[classes[i]]], 
                col = colors[classes[i]] %||% i,
                lwd = 2)
        }
      }
      
      # Add legend
      legend("topright",
             legend = classes,
             col = colors[classes],
             lty = 1,
             lwd = 2,
             title = "Classification")
      
    } else {
      # Histogram
      hist(plot_data$risk_score,
           main = title,
           xlab = "Average Normalized Risk Score",
           ylab = "Count",
           col = "lightgray",
           border = "white")
    }
    
    return(invisible(NULL))
  }
}

#' Plot CACS percentile distribution
#'
#' Shows the distribution of calculated CACS percentiles, which should be
#' approximately uniform if the model fits well.
#'
#' @param data Data frame containing CACS percentiles
#' @param percentile_col Name of percentile column (default: "cacs_percentile")
#' @param title Plot title
#' @return ggplot object if ggplot2 is available, otherwise base R plot
#' @export
plot_percentile_distribution <- function(data,
                                        percentile_col = "cacs_percentile",
                                        title = "CACS Percentile Distribution") {
  
  # Validate inputs
  validate_columns(data, percentile_col)
  
  percentiles <- data[[percentile_col]]
  percentiles <- percentiles[!is.na(percentiles)]
  
  if (length(percentiles) == 0) {
    stop("No valid percentiles available for plotting")
  }
  
  # Convert to 0-100 scale if needed
  if (max(percentiles) <= 1) {
    percentiles <- percentiles * 100
    x_label <- "CACS Percentile (%)"
  } else {
    x_label <- "CACS Percentile"
  }
  
  # Check if ggplot2 is available
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    
    plot_data <- data.frame(percentiles = percentiles)
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = percentiles)) +
      ggplot2::geom_histogram(bins = 20, fill = "skyblue", color = "white", alpha = 0.7) +
      ggplot2::geom_hline(yintercept = length(percentiles) / 20, 
                         linetype = "dashed", color = "red", 
                         alpha = 0.7, size = 1) +
      ggplot2::labs(
        title = title,
        subtitle = "Dashed line shows expected frequency for uniform distribution",
        x = x_label,
        y = "Count"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10)
      )
    
    return(p)
    
  } else {
    # Base R fallback
    warning("ggplot2 not available, using base R plotting")
    
    hist(percentiles,
         breaks = 20,
         main = title,
         xlab = x_label,
         ylab = "Count",
         col = "skyblue",
         border = "white")
    
    # Add expected uniform line
    expected_freq <- length(percentiles) / 20
    abline(h = expected_freq, col = "red", lty = 2, lwd = 2)
    
    return(invisible(NULL))
  }
}

#' Plot model diagnostics
#'
#' Creates diagnostic plots for the zero-inflated model including
#' predicted vs observed and residual plots.
#'
#' @param model Fitted CACS model
#' @param percentile_data Optional percentile data for additional plots
#' @param title_prefix Prefix for plot titles
#' @return List of ggplot objects or base R plots
#' @export
plot_model_diagnostics <- function(model, 
                                  percentile_data = NULL,
                                  title_prefix = "Model Diagnostics") {
  
  if (!inherits(model, "zeroinfl")) {
    stop("model must be a zeroinfl object")
  }
  
  # Get fitted values and observed
  fitted_vals <- stats::fitted(model)
  observed_vals <- model$y
  residuals_vals <- stats::residuals(model)
  
  # Scale factor for interpretation
  scale_factor <- attr(model, "scale_factor") %||% 100
  
  plots <- list()
  
  # 1. Predicted vs Observed
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    
    plot_data <- data.frame(
      fitted = fitted_vals / scale_factor,
      observed = observed_vals / scale_factor
    )
    
    plots$predicted_observed <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted, y = observed)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      ggplot2::labs(
        title = paste(title_prefix, "- Predicted vs Observed"),
        x = "Predicted CACS",
        y = "Observed CACS"
      ) +
      ggplot2::theme_minimal()
    
    # 2. Residuals vs Fitted
    plot_data$residuals <- residuals_vals
    
    plots$residuals_fitted <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted, y = residuals)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      ggplot2::labs(
        title = paste(title_prefix, "- Residuals vs Fitted"),
        x = "Fitted Values",
        y = "Residuals"
      ) +
      ggplot2::theme_minimal()
    
  } else {
    # Base R plots
    warning("ggplot2 not available, using base R plotting")
    
    # Predicted vs Observed
    plot(fitted_vals / scale_factor, observed_vals / scale_factor,
         main = paste(title_prefix, "- Predicted vs Observed"),
         xlab = "Predicted CACS",
         ylab = "Observed CACS",
         pch = 16, alpha = 0.6)
    abline(0, 1, col = "red", lty = 2)
    
    # Residuals vs Fitted  
    plot(fitted_vals / scale_factor, residuals_vals,
         main = paste(title_prefix, "- Residuals vs Fitted"),
         xlab = "Fitted Values", 
         ylab = "Residuals",
         pch = 16, alpha = 0.6)
    abline(h = 0, col = "red", lty = 2)
    
    plots <- NULL
  }
  
  # 3. Percentile distribution if provided
  if (!is.null(percentile_data) && "cacs_percentile" %in% names(percentile_data)) {
    plots$percentile_dist <- plot_percentile_distribution(
      percentile_data, 
      title = paste(title_prefix, "- Percentile Distribution")
    )
  }
  
  return(plots)
}

#' Create a comprehensive resilience analysis plot
#'
#' Combines multiple visualization components into a single comprehensive view
#'
#' @param data Data frame with all analysis results
#' @param cacs_col Name of CACS column
#' @param risk_score_col Name of risk score column
#' @param class_col Name of classification column
#' @param percentile_col Name of percentile column
#' @return ggplot object or message about base R plots
#' @export
plot_resilience_summary <- function(data,
                                   cacs_col = "cacs",
                                   risk_score_col = "average_norm_score", 
                                   class_col = "resilience_class",
                                   percentile_col = "cacs_percentile") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("ggplot2 not available. Use individual plot functions for base R plots.")
    return(invisible(NULL))
  }
  
  # Create individual plots
  p1 <- plot_cacs_vs_risk(data, cacs_col, risk_score_col, class_col,
                         title = "CACS vs Risk Score")
  
  p2 <- plot_risk_distribution(data, risk_score_col, class_col,
                              title = "Risk Score Distribution")
  
  p3 <- plot_percentile_distribution(data, percentile_col,
                                    title = "Percentile Distribution")
  
  # Try to arrange plots if gridExtra or patchwork is available
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    combined_plot <- gridExtra::grid.arrange(p1, p2, p3, ncol = 2)
    return(combined_plot)
  } else {
    message("Install gridExtra package for combined plots. Returning individual plots.")
    return(list(cacs_vs_risk = p1, risk_distribution = p2, percentile_dist = p3))
  }
}