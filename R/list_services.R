#' List all TFE services
#'
#' Lists all Lothian Buses, Airlink, Skylink,
#' Lothian Country, East Coast Buses, Nightbus,
#' Nighthawk, and Edinburgh Trams services
#'
#' @return A tibble
#' @export
#'
#' @examples list_services()
list_services <- function(){
  httr::GET(url = "https://tfe-opendata.com/api/v1/services") %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    magrittr::extract2(2) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::select(name, description, service_type)
}
