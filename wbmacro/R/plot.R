#' Quick Trend Plot from World Bank Data
#'
#' @description
#' Creates a ggplot2 line chart of time series from wb_fetch() output.
#' Supports coloring by country or indicator, faceting, and basic themes.
#'
#' @param data A tibble from wb_fetch() (long format).
#' @param color_by Character: "country" (default) or "indicator" to color lines.
#' @param facet_by Character: "country", "indicator", or NULL (no facet).
#' @param title Character: optional plot title.
#' @param ... Passed to ggplot2::geom_line() etc.
#'
#' @return A ggplot object.
#'
#' @examples
#' dat <- wb_fetch(
#'   indicators = c("NY.GDP.PCAP.KD", "SP.POP.GROW"),
#'   countries = c("CAN", "USA", "CHN"),
#'   start_year = 2010,
#'   end_year = 2023
#' )
#' wb_plot_trends(dat, color_by = "country", facet_by = "indicator")
#'
#' @export
wb_plot_trends <- function(data,
                           color_by = c("country", "indicator"),
                           facet_by = NULL,
                           title = NULL,
                           ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting. Install it with install.packages('ggplot2').")
  }

  color_by <- match.arg(color_by)

  # Ensure key columns exist
  required_cols <- c("country_name", "indicator_name", "year", "value")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("Data missing required columns: ", paste(missing, collapse = ", "))
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = year, y = value))

  if (color_by == "country") {
    p <- p + ggplot2::aes(color = country_name)
  } else {
    p <- p + ggplot2::aes(color = indicator_name)
  }

  p <- p +
    ggplot2::geom_line(linewidth = 1, ...) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      x = "Year",
      y = "Value",
      color = stringr::str_to_title(color_by),
      title = title %||% "World Bank Indicator Trends",
      caption = "Data: World Bank Indicators API"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    )

  # Faceting if requested
  if (!is.null(facet_by)) {
    if (facet_by == "country") {
      p <- p + ggplot2::facet_wrap(~ country_name, scales = "free_y")
    } else if (facet_by == "indicator") {
      p <- p + ggplot2::facet_wrap(~ indicator_name, scales = "free_y")
    }
  }

  p
}
