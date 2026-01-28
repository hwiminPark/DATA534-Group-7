#' Fetch World Bank Indicator Time Series Data
#'
#' @description
#' Retrieve time-series data for specified indicators and countries over a date range.
#' Handles pagination and returns tidy long-format data.
#'
#' @param indicators Character vector of indicator codes (e.g. "NY.GDP.PCAP.KD").
#' @param countries Character vector of ISO3 codes or "all".
#' @param start_year,end_year Integer years.
#' @param per_page Items per API page (default 100).
#'
#' @return A tibble with columns: country_iso3c, country_name, indicator_id,
#'   indicator_name, year, value, unit, etc.
#'
#' @export
wb_fetch <- function(indicators,
                     countries,
                     start_year,
                     end_year,
                     per_page = 100) {

  # Input validation
  if (length(indicators) == 0 || anyNA(indicators)) stop("Provide at least one valid indicator.")
  if (length(countries) == 0 || anyNA(countries)) stop("Provide at least one country code.")
  if (!is.numeric(start_year) || !is.numeric(end_year) ||
      start_year > end_year || start_year < 1960 || end_year > 2100) {
    stop("Invalid year range (integers, e.g. 2000 to 2024).")
  }

  # Build country string
  countries_str <- if (identical(countries, "all")) "all" else paste(countries, collapse = ";")
  ind_str <- paste(indicators, collapse = ";")

  base_url <- paste0("https://api.worldbank.org/v2/country/", countries_str, "/indicator/", ind_str)

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

    if (length(content) < 2 || !is.data.frame(content[[2]])) break

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

  # Handle empty combined result → fallback to per-indicator fetch
  if (length(data_list) == 0) {
    message(
      "No data from combined query (common API quirk with multi-indicator requests).\n",
      "Falling back to single-indicator fetches..."
    )

    all_data <- list()

    for (ind in indicators) {
      single <- tryCatch(
        wb_fetch(indicators = ind, countries = countries,
                 start_year = start_year, end_year = end_year,
                 per_page = per_page),
        error = function(e) {
          message("  Failed for ", ind, ": ", e$message)
          tibble::tibble()
        }
      )
      if (nrow(single) > 0) all_data[[ind]] <- single
    }

    if (length(all_data) > 0) {
      return(dplyr::bind_rows(all_data))
    }

    message("No data after fallback. Check availability at data.worldbank.org.")
    return(tibble::tibble())
  }

  dplyr::bind_rows(data_list)
}
