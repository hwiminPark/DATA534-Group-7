#' List World Bank Countries and Regions
#'
#' @description
#' Retrieve metadata for countries, economies, and aggregates from the World Bank API.
#' Handles pagination automatically and returns a tidy tibble.
#'
#' @param exclude_aggregates Logical. If TRUE (default FALSE), remove regional/aggregate entries.
#' @param income_groups Character vector of income levels to include (e.g. "High income").
#' @param per_page Max items per API page (default 100).
#'
#' @return A tibble with columns: iso3c, iso2c, name, region, income_level, etc.
#'
#' @examples
#' wb_countries() |> nrow()
#' wb_countries(exclude_aggregates = TRUE) |> head()
#'
#' @export
wb_countries <- function(exclude_aggregates = FALSE,
                         income_groups = NULL,
                         per_page = 100) {

  base_url <- "https://api.worldbank.org/v2/country"

  countries <- list()
  page <- 1

  repeat {
    resp <- httr2::request(base_url) |>
      httr2::req_url_query(
        format   = "json",
        per_page = per_page,
        page     = page
      ) |>
      httr2::req_perform()

    httr2::resp_check_status(resp)

    content <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    if (length(content) < 2 || !is.data.frame(content[[2]])) {
      break
    }

    page_data <- content[[2]] |>
      tibble::as_tibble() |>
      # Extract .value from nested list-columns
      dplyr::mutate(
        region       = region$value,
        income_level = incomeLevel$value,
        lending_type = lendingType$value
      ) |>
      dplyr::rename(
        iso3c        = id,
        iso2c        = iso2Code,
        name         = name
      ) |>
      dplyr::mutate(
        iso3c = as.character(iso3c),
        iso2c = as.character(iso2c),
        across(where(is.character), ~ dplyr::na_if(.x, ""))
      ) |>
      dplyr::select(
        iso3c, iso2c, name, region, income_level, lending_type,
        capitalCity, longitude, latitude, dplyr::everything()
      )

    countries[[page]] <- page_data

    meta <- content[[1]]
    if (page >= meta$pages) break
    page <- page + 1
  }

  if (length(countries) == 0) {
    return(tibble::tibble())
  }

  result <- dplyr::bind_rows(countries)

  # Apply filters
  if (exclude_aggregates) {
    result <- result |>
      dplyr::filter(
        !grepl("aggregate|world|income|dividend|ida|not classified",
               region, ignore.case = TRUE) &
          !grepl("aggregate|world|income|dividend|ida|not classified",
                 name, ignore.case = TRUE)
      )
  }

  if (!is.null(income_groups)) {
    result <- result |>
      dplyr::filter(income_level %in% income_groups)
  }

  result
}
