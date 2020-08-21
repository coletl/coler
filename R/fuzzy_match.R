#' Link data.tables by fuzzy string matching
#'
#' @name fuzzy_match
#' @aliases fuzzy_check
#'
#' @description Finds the closest string match.
#' The default method computes Jaro-Winkler string distances using the \code{stringdist} package.
#' For strings with multiple closest matches, only the first is reported.
#' @param a a source vector of strings
#' @param b a target vector
#' @param method method for \code{\link[stringdist]{stringdistmatrix}}
#' @param cutoff numeric indicating the maximum distance threshold for a match (\code{fuzzy_match} only).
#' String distances equal to or below the cutoff are counted as matches.
#' @param ... further arguments for \code{\link[stringdist]{stringdistmatrix}}
#'
#' @return For \code{fuzzy_match}, a vector of nearest string matches.
#' For strings with multiple closest matches, only the first is returned.
#'
#' @seealso
#' \code{\link[stringdist]{stringdistmatrix}}
#'
#' @examples
#' library(data.table)
#'
#' set.seed(575)
#' fruit <- sample(stringr::fruit, 30)
#'
#' DTA <- data.table(block1 = sample(LETTERS[1:4], 20, TRUE),
#'                   block2 = sample(LETTERS[1:4], 20, TRUE),
#'                   fruit   = sample(fruit, 20))
#'
#' DTB <- data.table(block1 = sample(LETTERS[1:4], 20, TRUE),
#'                   block2 = sample(LETTERS[1:4], 20, TRUE),
#'                   fruit   = sample(fruit, 20))
#'
#' fuzzy_check(DTA$fruit, DTB$fruit)
#' fuzzy_match(DTA$fruit, DTB$fruit)
#'
#' setkey(DTB, block1, block2)
#'
#' DTA[ , fuzzy_check(fruit, b = DTB[.BY, fruit]),
#'      by = .(block1, block2)]
#' DTA[ , .(fruit,
#'          B_fruit = fuzzy_match(fruit, b = DTB[.BY, fruit])),
#'      by = .(block1, block2)]
#'
#' @export

fuzzy_match <- function(a, b, method = "jw", cutoff = 0.5, ...) {

  distmat <- stringdist::stringdistmatrix(a, b, useNames = TRUE, method = method, ...)
  simmat <- 1 - distmat

  best_match <- colnames(simmat)[max.col(simmat)]
  min_dist   <- matrixStats::rowMins(distmat)

  # Guarantee character-type output
  out <- dplyr::if_else(min_dist <= cutoff, best_match, NA_character_)

  return(out)
}


#' @rdname fuzzy_match
#' @return For \code{fuzzy_check}, a data.frame containing the source strings,
#' their closest matches, and the string distance for each match.
#' @export

fuzzy_check <- function(a, b, method = "jw", ...) {

  distmat <- stringdist::stringdistmatrix(a, b, useNames = TRUE, method = method, ...)
  simmat <- 1 - distmat

  best_match <- colnames(simmat)[max.col(simmat)]
  min_dist   <- matrixStats::rowMins(distmat)

  out <- data.frame(source = rownames(distmat), best_match, min_dist)

  return(out)
}
