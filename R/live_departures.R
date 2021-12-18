#' Get TfE live departures
#'
#' @param stop_code SMS code for bus stop or vector of SMS codes
#' @param api TfE API endpoint
#' @param version TfE API version
#'
#' @return A tibble with next departures from selected stops
#' @export
#'
#' @examples live_departures(c(36235979, 36253636))
live_departures = function(stop_code,
                           api = "https://tfe-opendata.com/api/",
                           version = 1) {

  stop_code <- as.character(stop_code)
  version   <- as.character(version)

  stop_url <- paste0(api, "v", version, "/live_bus_times/")

  all_stops_departures <- tibble::tibble()

  for (i in 1:length(stop_code)) {

    this_stop_departures <-
      httr::GET(url = paste0(stop_url, stop_code[i])) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      magrittr::extract2(2) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble() %>%
      janitor::clean_names()

    all_stops_departures <- all_stops_departures %>%
      dplyr::bind_rows(this_stop_departures)
  }

  return(all_stops_departures)
}
