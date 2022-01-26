#' Title
#'
#' @param old_file
#' @param new_file
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' new_osm <- osm_prep_network(old_osm = buffalo_osm)

osm_prep_network <- function(old_osm) {

  new_lines <- stringr::str_replace(old_osm,
                           "(?<=lon=\".{6,12}\")/",
                           " version = \"1\"/")

  new_lines <- stringr::str_replace(new_lines,
                           "(?<=lon=\".{6,12}\")>",
                           " version = \"1\">")

  new_lines <- stringr::str_replace(new_lines,
                           '(?<=way id=\".{6,12}\")>',
                           " version = \"1\">")

  new_lines <- stringr::str_replace(new_lines,
                           '(?<=relation id=\".{6,12}\")>',
                           " version = \"1\">")
  new_lines
}
