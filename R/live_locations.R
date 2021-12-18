#' Live Lothian bus and Edinburgh tram locations
#'
#' @param api TfE API endpoint
#' @param version TfE API version
#'
#' @return A tibble with live locations of all buses and trams
#' @export
#'
#' @examples live_locations()
live_locations <- function(api = "https://tfe-opendata.com/api/",
                           version = 1) {

  version <- as.character(version)

  live_locations_url <- paste0(api, "v", version, "/vehicle_locations")

  httr::GET(url = live_locations_url) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    magrittr::extract2(2) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()

}
