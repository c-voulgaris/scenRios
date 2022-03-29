#' Calculate balancing factors for gravity model
#'
#' @param od_zones A data frame with origins and destinations
#' @param friction A data frame with friction factors for each O-D pair
#' @param zone_id Name of ID column in od_zones
#' @param zone_o Name of origins column in od_zones
#' @param zone_d Name of destinations column in od_zones
#' @param friction_o_id Name of column with origin ID in friction
#' @param friction_d_id Name of column with destination ID in friction
#' @param friction_factor Name of column with friction factor in friction
#' @param tolerance Acceptable error (number of trips)
#' @param max_iter Maximum number of iterations
#' @return A list of two data frames, one with the flows and one with convergence data
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' result <- grvty_balancing(od_zones = salt_lake_zones,
#'                           friction = salt_lake_friction,
#'                           zone_id = "GEOID",
#'                           zone_o = "hbo_prod",
#'                           zone_d = "hbo_attr_bal",
#'                           friction_o_id = "fromId",
#'                           friction_d_id = "toId",
#'                           friction_factor = "F_HBO",
#'                           tolerance = 0.01,
#'                           max_iter = 100000)
#'
#'

grvty_balancing <- function(od_zones,
                            friction,
                            zone_id,
                            zone_o,
                            zone_d,
                            friction_o_id,
                            friction_d_id,
                            friction_factor,
                            tolerance,
                            max_iter) {

od_zones <- denver_zones
friction <- denver_friction
zone_id <- "GEOID"
zone_o <- "nhb_prod"
zone_d <- "nhb_attr_bal"
friction_o_id <- "fromId"
friction_d_id <- "toId"
friction_factor <- "F_NHB"
tolerance <- 0.01
max_iter <- 100

  # rename and select columns
  wip_friction <- friction %>%
    dplyr::rename(o_id = tidyselect::all_of(friction_o_id),
                  d_id = tidyselect::all_of(friction_d_id),
                  factor = tidyselect::all_of(friction_factor)) %>%
    dplyr::select(o_id, d_id, factor)

  wip_zones <- od_zones %>%
    dplyr::rename(id = tidyselect::all_of(zone_id),
                  origin = tidyselect::all_of(zone_o),
                  destin = tidyselect::all_of(zone_d)) %>%
    dplyr::mutate(origin = round(origin),
                  destin = round(destin)) %>%
    dplyr::select(id, origin, destin)

  # get minimum non-zero value for friction factor
  min_factor <- min(wip_friction$factor[wip_friction$factor != 0])

  # remove friction rows where the origin and destination are the same
  wip_friction <- wip_friction %>%
    dplyr::filter(o_id != d_id) %>%
    # set all zero friction values equal to the smallest non-zero value
    dplyr::mutate(factor = ifelse(factor == 0, min_factor, factor))

  # scale up so all values are greater thean 10^-100
  if (min_factor < 10^-100) {
    wip_friction <- wip_friction %>%
      dplyr:: mutate(factor = factor * (10^-100 / min_factor))
  }

  # Add productions and attractions to trip matrix
  origins <- wip_zones %>%
    dplyr::select(id, origin)

  destinations <- wip_zones %>%
    dplyr::select(id, destin)

  flows <- wip_friction %>%
    dplyr::left_join(origins, by = c("o_id" = "id")) %>%
    dplyr::left_join(destinations, by = c("d_id" = "id")) %>%
    dplyr::rename(friction = factor)

  # first iteration
  flows <- flows %>%
    dplyr::mutate(B_factor = 1)

  flows <- flows %>%
    dplyr::group_by(o_id) %>%
    dplyr::mutate(A_factor = 1/sum(B_factor * destin * friction)) %>%
    dplyr::mutate(flow = A_factor * origin * B_factor * destin * friction) %>%
    dplyr::ungroup()

  balance_check_o <- flows %>%
    dplyr::group_by(o_id) %>%
    dplyr::summarize(target = mean(origin),
              value = sum(flow)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(diff = (value - target) / target) %>%
    tidyr::replace_na(list(diff = 0)) %>%
    dplyr::summarize(max_o_diff = max(abs(diff)))

  balance_check_d <- flows %>%
    dplyr::group_by(d_id) %>%
    dplyr::summarize(target = mean(destin),
              value = sum(flow)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(diff = (value - target) / target) %>%
    tidyr::replace_na(list(diff = 0)) %>%
    dplyr::summarize(max_d_diff = max(abs(diff)))

  balance_check <- tibble::tibble(iteration = 1,
                     max_o_diff = round(balance_check_o$max_o_diff[1],4),
                     max_d_diff = round(balance_check_d$max_d_diff[1],4))

  # Loop for the rest of the iterations
  done <- FALSE
  i <- 2
  while (!done) {
    flows <- flows %>%
      dplyr::group_by(d_id) %>%
      dplyr::mutate(B_factor = 1 / sum(A_factor * origin * friction)) %>%
      dplyr::mutate(flow = A_factor * origin * B_factor * destin * friction) %>%
      dplyr::ungroup()

    balance_check_o <- flows %>%
      dplyr::group_by(o_id) %>%
      dplyr::summarize(target = mean(origin),
                       value = sum(flow)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(diff = (value - target) / target) %>%
      tidyr::replace_na(list(diff = 0)) %>%
      dplyr::summarize(max_o_diff = max(abs(diff)))

    balance_check_d <- flows %>%
      dplyr::group_by(d_id) %>%
      dplyr::summarize(target = mean(destin),
                       value = sum(flow)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(diff = (value - target) / target) %>%
      tidyr::replace_na(list(diff = 0)) %>%
      dplyr::summarize(max_d_diff = max(abs(diff)))

    next_balance_check <- tibble::tibble(iteration = i,
                                         max_o_diff =
                                           round(balance_check_o$max_o_diff[1],4),
                                         max_d_diff =
                                           round(balance_check_d$max_d_diff[1],4))

    balance_check <- rbind(balance_check, next_balance_check)

    i <- i + 1

    flows <- flows %>%
      dplyr::group_by(o_id) %>%
      dplyr::mutate(A_factor = 1 / sum(B_factor * destin * friction)) %>%
      dplyr::mutate(flow = A_factor * origin * B_factor * destin * friction) %>%
      dplyr::ungroup()

    balance_check_o <- flows %>%
      dplyr::group_by(o_id) %>%
      dplyr::summarize(target = mean(origin),
                       value = sum(flow)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(diff = (value - target) / target) %>%
      tidyr::replace_na(list(diff = 0)) %>%
      dplyr::summarize(max_o_diff = max(abs(diff)))

    balance_check_d <- flows %>%
      dplyr::group_by(d_id) %>%
      dplyr::summarize(target = mean(destin),
                       value = sum(flow)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(diff = (value - target) / target) %>%
      tidyr::replace_na(list(diff = 0)) %>%
      dplyr::summarize(max_d_diff = max(abs(diff)))

    next_balance_check <- tibble::tibble(iteration = i,
                                         max_o_diff =
                                           round(balance_check_o$max_o_diff[1],4),
                                         max_d_diff =
                                           round(balance_check_d$max_d_diff[1],4))

    balance_check <- rbind(balance_check, next_balance_check)

    i <- i + 1
    done = (next_balance_check$max_o_diff < tolerance &
      next_balance_check$max_d_diff < tolerance) |
      i > max_iter

  }

  flows <- flows %>%
    dplyr::mutate(flow = round(flow)) %>%
    dplyr::select(o_id, d_id, flow)

  list(flows = flows, convergence = balance_check)
}
