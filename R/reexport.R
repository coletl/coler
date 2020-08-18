# Functions to import from other packages

# Programming ----
#' @importFrom purrr safely
#' @export
purrr::safely

#' @importFrom pryr object_size
#' @export
pryr::object_size

#' @importFrom waldo compare
#' @export
waldo::compare

#' @importFrom purrr map map_at
#' @export
purrr::map
purrr::map_at

# Strings ----
#' @importFrom stringr str_extract str_split str_split_fixed
#' @export
#' @export
stringr::str_extract
stringr::str_split
stringr::str_split_fixed

# Other ----
#' @importFrom skimr skim
#' @export
skimr::skim

#' @importFrom janitor tabyl
#' @export
janitor::tabyl

#' @importFrom broom tidy
#' @export
broom::tidy

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
