#' Get a set of headways by time period
#'
#' @param feed A GTFS object.
#' @param route A character string with the route ID.
#' @param service A character string with the service ID
#' @return A tibble of time periods and headways.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' hdwys <- gtfs_get_hdwys(feed = nfta_gtfs, route = "19", service = "1")
#' @export

gtfs_get_hdwys <- function(feed,
                          route,
                          service) {

  trips_0 <- feed$trips[feed$trips$route_id == route &
                          feed$trips$service_id == service &
                          feed$trips$direction_id == 0,]

  trips_1 <- feed$trips[feed$trips$route_id == route &
                          feed$trips$service_id == service &
                          feed$trips$direction_id == 1,]

  stop_times_0 <- feed$stop_times[feed$stop_times$trip_id %in% trips_0$trip_id &
                                    feed$stop_times$stop_sequence == 1,]

  stop_times_1 <- feed$stop_times[feed$stop_times$trip_id %in% trips_1$trip_id &
                                    feed$stop_times$stop_sequence == 1,]

  dir_0 <- tibble::tibble(
      from = stop_times_0$arrival_time[1:length(stop_times_0$arrival_time) - 1],
      until = stop_times_0$arrival_time[2:length(stop_times_0$arrival_time)],
      headway = as.numeric(diff(stop_times_0$arrival_time)) / 60,
      period = cumsum(c(1, diff(headway) != 0))) %>%
      dplyr::group_by(period) %>%
      dplyr::summarise(from = dplyr::first(from),
                until = dplyr::last(until),
                headway = round(mean(headway))) %>%
    dplyr::mutate(direction = 0)

  dir_1 <- tibble::tibble(
    from = stop_times_1$arrival_time[1:length(stop_times_1$arrival_time) - 1],
    until = stop_times_1$arrival_time[2:length(stop_times_1$arrival_time)],
    headway = as.numeric(diff(stop_times_1$arrival_time)) / 60,
    period = cumsum(c(1, diff(headway) != 0))) %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(from = dplyr::first(from),
              until = dplyr::last(until),
              headway = round(mean(headway))) %>%
    dplyr::mutate(direction = 1)

  rbind(dir_0, dir_1)
}
