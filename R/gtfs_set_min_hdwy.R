#' Update trips and stop_times with a new set of headways
#'
#' @param feed A GTFS object.
#' @param route A character string with the route ID.
#' @param service A character string with the service ID
#' @param new_hdwy A number with the new minimum headway
#' @return An updated GTFS object.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' new_feed <- gtfs_set_min_hdwy(feed = nfta_gtfs,
#'                              route = "19",
#'                              service = "1",
#'                              new_hdwy = 5)
#' @export
gtfs_set_min_hdwy <- function(feed,
                             route,
                             service,
                             new_hdwy) {

  # Calculate a new set of headways for each period by scaling down to the
  # user-specified new minimum
  hdwys <- gtfs_get_hdwys(feed, route, service)
  factor <- new_hdwy / min(hdwys$headway)

  hdwys <- hdwys %>%
    dplyr::mutate(new_hdwy = round(headway * factor)) %>%
    dplyr::mutate(n_trips = round((as.numeric(until - from) / 60) / new_hdwy))

  # The trips we'll be replacing
  old_trips <- feed$trips[feed$trips$route_id == route &
                          feed$trips$service_id == service,]

  # The trips we won't be replacing
  keep_trips <- feed$trips[
    !(feed$trips$trip_id %in% old_trips$trip_id),]

  # The stop times we'll be replacing
  old_stop_times <- feed$stop_times[
    feed$stop_times$trip_id %in% old_trips$trip_id,]

  # The stop times we won't be replacing
  keep_stop_times <- feed$stop_times[
    !(feed$stop_times$trip_id %in% old_trips$trip_id),]

  # Get the trip ID of the first trip in each direction
  first_trip_0 <- dplyr::first(old_trips$trip_id[old_trips$direction_id == 0])
  first_trip_1 <- dplyr::first(old_trips$trip_id[old_trips$direction_id == 1])

  # Create a new set of trips
  new_trips <- tibble::tibble(route_id = route,
                      service_id = service,
                      trip_id = paste(
                        seq(1, sum(hdwys$n_trips)),
                        "edt", sep=""),
                      trip_headsign = c(
                        rep(
                          dplyr::first(
                            old_trips$trip_headsign[
                              old_trips$direction_id == 0]),
                          times = sum(hdwys$n_trips[hdwys$direction == 0])),
                        rep(
                          dplyr::first(
                            old_trips$trip_headsign[
                              old_trips$direction_id == 1]),
                          times = sum(hdwys$n_trips[hdwys$direction == 1]))),
                      direction_id =
                        c(rep(0, times =
                                sum(hdwys$n_trips[hdwys$direction == 0])),
                          rep(1, times =
                                sum(hdwys$n_trips[hdwys$direction == 1]))),
                      block_id = "new",
                      shape_id = c(
                        rep(
                          dplyr::first(
                            old_trips$shape_id[
                              old_trips$direction_id == 0]),
                          times = sum(hdwys$n_trips[hdwys$direction == 0])),
                        rep(
                          dplyr::first(
                            old_trips$shape_id[
                              old_trips$direction_id == 1]),
                          times = sum(hdwys$n_trips[hdwys$direction == 1]))))

  # Set things up to loop through to make new stop times
  new_stop_times_0 <- last_stop_times_0 <-
    old_stop_times[old_stop_times$trip_id == first_trip_0,] %>%
    dplyr::mutate(trip_id = new_trips$trip_id[1])

  new_stop_times_1 <- last_stop_times_1 <-
    old_stop_times[old_stop_times$trip_id == first_trip_1,] %>%
    dplyr::mutate(trip_id = new_trips$trip_id[
      1 + sum(hdwys$n_trips[hdwys$direction == 0])])

  next_hdwy_0 <- hdwys$new_hdwy[1]
  next_hdwy_1 <- dplyr::first(hdwys$new_hdwy[hdwys$direction == 1])

  for (i in 2:sum(hdwys$n_trips[hdwys$direction == 0])) {
    next_stop_times_0 <- last_stop_times_0 %>%
      dplyr::mutate(trip_id = new_trips$trip_id[i]) %>%
      dplyr::mutate(arrival_time =
               hms::as_hms(arrival_time +
                             lubridate::dminutes(next_hdwy_0))) %>%
      dplyr::mutate(departure_time =
               hms::as_hms(departure_time +
                             lubridate::dminutes(next_hdwy_0)))

    for (j in 1:sum(hdwys$direction == 0) ){
      if (next_stop_times_0$arrival_time[1] > hdwys$from[j] &
          next_stop_times_0$arrival_time[1] < hdwys$until[j]) {
        next_hdwy_0 <- hdwys$new_hdwy[j]
      }
    }
    last_stop_times_0 <- next_stop_times_0
    new_stop_times_0 <- rbind(new_stop_times_0, next_stop_times_0)
  }

  for (i in 2:sum(hdwys$n_trips[hdwys$direction == 1])) {
    next_stop_times_1 <- last_stop_times_1 %>%
      dplyr::mutate(trip_id = new_trips$trip_id[
        i + sum(hdwys$n_trips[hdwys$direction == 0])]) %>%
      dplyr::mutate(arrival_time =
               hms::as_hms(arrival_time +
                             lubridate::dminutes(next_hdwy_1))) %>%
      dplyr::mutate(departure_time =
               hms::as_hms(departure_time +
                             lubridate::dminutes(next_hdwy_1)))

    for (j in 1:sum(hdwys$direction == 1) ){
      if (next_stop_times_1$arrival_time[1] >
          hdwys$from[j + sum(hdwys$direction == 0)] &
          next_stop_times_1$arrival_time[1] <
          hdwys$until[j + sum(hdwys$direction == 0)]) {
        next_hdwy_1 <- hdwys$new_hdwy[j]
      }
    }
    last_stop_times_1 <- next_stop_times_1
    new_stop_times_1 <- rbind(new_stop_times_1, next_stop_times_1)
  }

  new_stop_times <- rbind(new_stop_times_0, new_stop_times_1)

  all_trips <- rbind(keep_trips, new_trips)
  all_stop_times <- rbind(keep_stop_times, new_stop_times)

  new_feed <- feed

  new_feed$trips <- all_trips
  new_feed$stop_times <- all_stop_times

  new_feed
}
