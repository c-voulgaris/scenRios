#' Title
#'
#' @param old_file
#' @param new_file
#'
#' @return
#' @export
#'
#' @examples
#' osm_prep_network(old_file = "existing.osm", new_file = "proposed.osm")

osm_prep_network <- function(old_file, new_file) {
  lines <- read_lines(old_file)

  new_lines <- str_replace(lines,
                           "(?<=lon=\".{6,12}\")/",
                           " version = \"1\"/")

  new_lines <- str_replace(new_lines,
                           "(?<=lon=\".{6,12}\")>",
                           " version = \"1\">")

  new_lines <- str_replace(new_lines,
                           '(?<=way id=\".{6,12}\")>',
                           " version = \"1\">")

  new_lines <- str_replace(new_lines,
                           '(?<=relation id=\".{6,12}\")>',
                           " version = \"1\">")

  write_lines(new_lines,
              file = new_file,
              append = FALSE)

}
