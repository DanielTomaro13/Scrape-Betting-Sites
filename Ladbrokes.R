#' Fetch Ladbrokes Event Summary and Assign to Global Environment
#'
#' @param event_id Character. Ladbrokes event UUID.
#' @return Invisibly returns a list with `ladbrokes_market_summary` and `ladbrokes_selections`.
#' @export
fetch_ladbrokes_summary <- function(event_id) {
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(stringr)

  url <- "https://api.ladbrokes.com.au/v2/sport/event-card"
  res <- GET(
    url,
    query = list(id = event_id),
    add_headers(
      `User-Agent` = "Mozilla/5.0",
      `Accept` = "application/json"
    )
  )

  parsed <- content(res, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE)

  markets <- parsed$markets
  entrants <- parsed$entrants
  prices <- parsed$prices

  if (is.null(markets) || length(markets) == 0) {
    warning("No markets found for event ", event_id)
    assign("ladbrokes_market_summary", tibble(), envir = .GlobalEnv)
    assign("ladbrokes_selections", tibble(), envir = .GlobalEnv)
    return(invisible(list(market_summary = tibble(), selections = tibble())))
  }

  # Build selections with updated_at
  selections <- map_dfr(markets, function(mkt) {
    if (is.null(mkt$entrant_ids)) return(NULL)

    map_dfr(mkt$entrant_ids, function(ent_id) {
      entrant <- entrants[[ent_id]]
      if (is.null(entrant)) return(NULL)

      price_key_pattern <- paste0("^", ent_id, ":[^:]*:$")
      price_key <- names(prices)[str_detect(names(prices), price_key_pattern)][1]
      price_info <- prices[[price_key]]

      price <- NA_real_
      updated_at <- NA_character_

      if (!is.null(price_info)) {
        if (!is.null(price_info$odds$numerator) && !is.null(price_info$odds$denominator) && price_info$odds$denominator != 0) {
          price <- (price_info$odds$numerator / price_info$odds$denominator) + 1
        }
        if (!is.null(price_info$updated_at)) {
          updated_at <- price_info$updated_at
        }
      }

      tibble(
        market_id = mkt$id,
        market_name = mkt$name,
        selection_id = ent_id,
        selection_name = entrant$name,
        price = price,
        status = mkt$market_status_id,
        updated_at = updated_at
      )
    })
  })

  # Build market summary and include the most recent updated_at per market
  market_summary <- selections %>%
    group_by(market_id, market_name) %>%
    summarise(
      num_selections = n(),
      updated_at = max(updated_at, na.rm = TRUE),
      .groups = "drop"
    )

  assign("ladbrokes_market_summary", market_summary, envir = .GlobalEnv)
  assign("ladbrokes_selections", selections, envir = .GlobalEnv)

  invisible(list(market_summary = market_summary, selections = selections))
}
