#' Search World Bank Indicators
#'
#' @description
#' Search for World Bank development indicators by keyword or phrase.
#' Returns a tidy tibble with indicator code, name, description, etc.
#'
#' @param keyword Character string to search for (case-insensitive partial match).
#'   If NULL or empty, returns empty tibble (avoid fetching all indicators).
#' @param page_size Number of results per page (max 1000, default 50).
#' @param ... Additional query parameters passed to the API (e.g. source=2 for specific data sources).
#'
#' @return A tibble with columns: indicator_id, name, unit, source_note,
#'   source_organization, topics, etc.
#'
#' @examples
#' wb_search_indicators("gdp per capita")
#' wb_search_indicators("inflation", page_size = 20)
#'
#' @export
wb_search_indicators <- function(keyword = NULL, page_size = 50, ...) {
  # Early return for invalid/empty input – prevents unnecessary API calls
  if (is.null(keyword) || nchar(trimws(keyword)) < 1) {
    return(tibble::tibble())
  }

  base_url <- "https://api.worldbank.org/v2/indicator"

  # Build request with httr2
  resp <- httr2::request(base_url) |>
    httr2::req_url_query(
      format   = "json",
      per_page = min(page_size, 1000),
      q        = keyword,                # search term (partial match)
      ...
    ) |>
    httr2::req_perform()

  # Throw meaningful error if HTTP status is not 200
  httr2::resp_check_status(resp)

  # Parse JSON
  content <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  # Early return if no data array found
  if (length(content) < 2 || !is.data.frame(content[[2]])) {
    return(tibble::tibble())
  }

  # Convert to tibble and clean up column names/types
  indicators <- content[[2]] |>
    tibble::as_tibble() |>
    dplyr::rename(
      indicator_id       = id,
      name               = name,
      unit               = unit,
      source_note        = sourceNote,
      source_organization = sourceOrganization
    ) |>
    dplyr::mutate(
      indicator_id = as.character(indicator_id),
      # Replace empty strings with NA for cleaner downstream analysis
      across(where(is.character), ~ dplyr::na_if(.x, ""))
    ) |>
    dplyr::select(
      indicator_id,
      name,
      unit,
      source_note,
      source_organization,
      dplyr::everything()   # keep any other useful columns (topics, etc.)
    )

  indicators
}
