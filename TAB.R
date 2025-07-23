#' Fetch TAB Event Summary and assign to global environment
#'
#' @param url Character. Base URL to the TAB API match endpoint (no query string).
#' @param bearer_token Character. Bearer token for authorization header.
#' @return Invisibly returns a list with tab_market_summary and tab_selections
#' @export
fetch_tab_summary <- function(url, bearer_token) {
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(purrr)

  response <- GET(
    url = url,
    query = list(jurisdiction = "NSW"),
    add_headers(Authorization = paste("Bearer", bearer_token))
  )

  if (status_code(response) != 200) {
    warning("Request failed: ", status_code(response))
    return(invisible(list(tab_market_summary = tibble(), tab_selections = tibble())))
  }

  parsed <- fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)

  if (is.null(parsed$match$markets)) {
    warning("No markets found in the response.")
    return(invisible(list(tab_market_summary = tibble(), tab_selections = tibble())))
  }

  markets <- parsed$match$markets

  tab_market_summary <- map_dfr(seq_len(nrow(markets)), function(i) {
    market <- markets[i, ]
    propositions <- tryCatch(market$propositions, error = function(e) NULL)

    tibble(
      market_id = market$id,
      market_name = market$name,
      num_selections = if (is.data.frame(propositions)) nrow(propositions) else 0
    )
  })

  tab_selections <- map_dfr(seq_len(nrow(markets)), function(i) {
    market <- markets[i, ]
    props <- tryCatch(market$propositions, error = function(e) NULL)

    if (is.null(props) || !is.data.frame(props) || nrow(props) == 0) return(NULL)

    tibble(
      market_id      = market$id,
      market_name    = market$name,
      selection_id   = props$id,
      selection_name = props$name,
      price          = suppressWarnings(as.numeric(props$returnWin)),
      status         = props$bettingStatus,
      is_open        = props$isOpen
    )
  })

  assign("tab_market_summary", tab_market_summary, envir = .GlobalEnv)
  assign("tab_selections", tab_selections, envir = .GlobalEnv)

  invisible(list(tab_market_summary = tab_market_summary, tab_selections = tab_selections))
}
