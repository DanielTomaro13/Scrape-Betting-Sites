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
  res <- GET(url, query = list(id = event_id))
  stop_for_status(res)

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

  # Build market summary
  market_summary <- map_dfr(markets, function(mkt) {
    tibble(
      market_id = mkt$id,
      market_name = mkt$name,
      num_selections = if (!is.null(mkt$entrant_ids)) length(mkt$entrant_ids) else 0
    )
  })

  # Build selections
  selections <- map_dfr(markets, function(mkt) {
    if (is.null(mkt$entrant_ids)) return(NULL)

    map_dfr(mkt$entrant_ids, function(ent_id) {
      entrant <- entrants[[ent_id]]
      if (is.null(entrant)) return(NULL)

      # Find the matching price key
      price_key_pattern <- paste0("^", ent_id, ":[^:]*:$")
      price_key <- names(prices)[str_detect(names(prices), price_key_pattern)][1]
      price_info <- prices[[price_key]]

      price <- NA_real_
      if (!is.null(price_info) && !is.null(price_info$odds$numerator) && !is.null(price_info$odds$denominator)) {
        denom <- price_info$odds$denominator
        if (denom != 0) {
          price <- (price_info$odds$numerator / denom) + 1
        }
      }

      tibble(
        market_id = mkt$id,
        market_name = mkt$name,
        selection_id = ent_id,
        selection_name = entrant$name,
        price = price,
        status = mkt$market_status_id
      )
    })
  })

  assign("ladbrokes_market_summary", market_summary, envir = .GlobalEnv)
  assign("ladbrokes_selections", selections, envir = .GlobalEnv)

  invisible(list(market_summary = market_summary, selections = selections))
}
