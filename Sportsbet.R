#' Fetch Sportsbet Event Summary and assign to global environment
#'
#' @param event_id Character or numeric. Sportsbet event ID.
#' @return Invisibly returns list with market_summary and selections
#' @export
fetch_sportsbet_summary <- function(event_id) {
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(purrr)

  url <- sprintf("http://xmlfeeds.sportsbet.com.au/oxipub/getEventDetails?event=%s&output=JSON", event_id)
  response <- GET(url)

  if (status_code(response) != 200) {
    warning("Request failed: ", status_code(response))
    return(invisible(list(market_summary = tibble(), selections = tibble())))
  }

  parsed <- content(response, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE)
  event <- parsed$oxip$response$event
  if (is.null(event) || is.null(event$market)) {
    warning("No markets found for event ID: ", event_id)
    return(invisible(list(market_summary = tibble(), selections = tibble())))
  }

  markets <- event$market

  market_summary <- map_dfr(seq_len(nrow(markets)), function(i) {
    market <- markets[i, ]
    outcomes <- tryCatch(market$outcome[[1]], error = function(e) NULL)
    tibble(
      market_id      = market$`@id`,
      market_name    = market$`@name`,
      date_released = market$`@lastUpdateDate`,
      time_released = market$`@lastUpdateTime`,
      num_selections = if (is.data.frame(outcomes)) nrow(outcomes) else 0
    )
  })

  selections <- map_dfr(seq_len(nrow(markets)), function(i) {
    market <- markets[i, ]
    outcomes <- tryCatch(market$outcome[[1]], error = function(e) NULL)

    if (is.null(outcomes) || !is.data.frame(outcomes) || nrow(outcomes) == 0) return(NULL)

    tibble(
      event_id     = event$`@id`,
      event_name   = event$`@name`,
      market_id    = market$`@id`,
      market_name  = market$`@name`,
      date_released = market$`@lastUpdateDate`,
      time_released = market$`@lastUpdateTime`,
      outcome_id   = outcomes$`@id`,
      outcome_name = outcomes$`@name`,
      win_odds     = suppressWarnings(as.numeric(outcomes$`@oddsDecimal`)),
      status       = outcomes$`@status`,
      last_update  = paste(outcomes$`@lastUpdateDate`, outcomes$`@lastUpdateTime`)
    )
  })

  assign("market_summary", market_summary, envir = .GlobalEnv)
  assign("selections", selections, envir = .GlobalEnv)

  invisible(list(market_summary = market_summary, selections = selections))
}
