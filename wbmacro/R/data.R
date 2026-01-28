#' Fetch World Bank Indicator Time Series Data
#'
#' @description
#' Retrieve time-series data for specified indicators and countries over a date range.
#' Handles pagination and returns tidy long-format data.
#'
#' @param indicators Character vector of indicator codes (e.g. "NY.GDP.PCAP.KD").
#'   Use `wb_search_indicators()` to find codes.
#' @param countries Character vector of ISO3 codes (e.g. c("CAN", "USA", "CHN")).
#'   Use `wb_countries()` to get valid codes. "all" fetches everything (slow!).
#' @param start_year,end_year Integer years (e.g. 2000, 2024).
#' @param per_page Items per API page (default 100).
#'
#' @return A tibble with columns: country_iso3c, country_name, indicator_id,
#'   indicator_name, year, value, unit, etc.
#'
#' @examples
#' wb_fetch(
#'   indicators = "NY.GDP.PCAP.KD",
#'   countries  = c("CAN", "USA"),
#'   start_year = 2010,
#'   end_year   = 2023
#' )
#'
#' @export
wb_fetch <- function(indicators,
                     countries,
                     start_year,
                     end_year,
                     per_page = 100) {

  if (length(indicators) == 0 || anyNA(indicators)) {
    stop("Provide at least one valid indicator code.")
  }

  if (length(countries) == 0 || anyNA(countries)) {
    stop("Provide at least one valid country code.")
  }

  if (!is.numeric(start_year) || !is.numeric(end_year) ||
      start_year > end_year || start_year < 1960 || end_year > 2100) {
    stop("Invalid year range (use integers, e.g. 2000 to 2024).")
  }

  # Special case: "all" countries
  if (identical(countries, "all")) {
    countries <- "all"
  } else {
    countries <- paste(countries, collapse = ";")
  }

  ind_str <- paste(indicators, collapse = ";")

  base_url <- paste0("https://api.worldbank.org/v2/country/",
                     countries,
                     "/indicator/",
                     ind_str)

  data_list <- list()
  page <- 1

  repeat {
    resp <- httr2::request(base_url) |>
      httr2::req_url_query(
        format   = "json",
        date     = paste(start_year, end_year, sep = ":"),
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
      dplyr::mutate(
        year         = as.integer(date),
        value        = as.numeric(value),
        country_iso3c = country$id,
        country_name = country$value,
        indicator_id = indicator$id,
        indicator_name = indicator$value,
        unit         = unit
      ) |>
      dplyr::select(
        country_iso3c, country_name,
        indicator_id, indicator_name,
        year, value, unit,
        dplyr::everything()
      ) |>
      dplyr::arrange(country_iso3c, indicator_id, year)

    data_list[[page]] <- page_data

    meta <- content[[1]]
    if (page >= meta$pages) break
    page <- page + 1
  }

  if (length(data_list) == 0) {
    message(
      "API returned no data for the combined query.\n",
      "This can happen with multi-indicator requests if one series has gaps.\n",
      "Falling back to fetching each indicator separately..."
    )

    all_data <- list()

    for (ind in indicators) {
      single_resp <- tryCatch(
        wb_fetch(
          indicators = ind,
          countries  = countries,
          start_year = start_year,
          end_year   = end_year,
          per_page   = per_page
        ),
        error = function(e) {
          message("Failed for indicator '", ind, "': ", e$message)
          tibble::tibble()
        }
      )

      if (nrow(single_resp) > 0) {
        all_data[[ind]] <- single_resp
      }
    }

    if (length(all_data) > 0) {
      return(dplyr::bind_rows(all_data))
    } else {
      message("Fallback also returned no data. Check indicator availability, years, or try later.")
      return(tibble::tibble())
    }
  }

  dplyr::bind_rows(data_list)
}
