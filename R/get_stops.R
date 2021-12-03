#' Get a list of TFE stops, with locations and services calling
#'
#' @return A tibble
#' @export
#'
#' @examples
#' get_stops()
get_stops <- function(){

  int_stops <- httr::GET(url = "https://tfe-opendata.com/api/v1/stops") %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    magrittr::extract2(2) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()

  int_destinations <- int_stops %>%
    tidyr::unnest(destinations) %>%
    dplyr::select(stop_id, destinations) %>%
    dplyr::group_by(stop_id) %>%
    dplyr::mutate(order = match(destinations, unique(destinations)),
                  order = paste0("destination_", order)) %>%
    tidyr::pivot_wider(id_cols = stop_id,
                       names_from = order,
                       values_from = destinations)

  int_services <- int_stops %>%
    tidyr::unnest(services) %>%
    dplyr::select(stop_id, services) %>%
    dplyr::group_by(stop_id) %>%
    dplyr::mutate(order = match(services, unique(services)),
                  order = paste0("service_", order)) %>%
    tidyr::pivot_wider(id_cols = stop_id,
                       names_from = order,
                       values_from = services)

  int_stops <- int_stops %>%
    dplyr::select(-c(services, destinations)) %>%
    dplyr::left_join(int_destinations, by = "stop_id") %>%
    dplyr::left_join(int_services, by = "stop_id")

  return(int_stops)

}
