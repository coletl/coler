#' @name detect_duped
#' @title Test for duplicates
#'
#' @description Test for duplicates in one or a combination of vectors.
#'
#' @details
#' This function \strong{\emph{should not}} be used in places of \code{duplicated()} and \strong{\emph{does not return duplicates}}.
#' Instead, \code{detect_duped()} executes the common pattern
#'
#' \code{vector \%in\% vector[duplicated(vector)]}.
#'
#' Providing multiple vector arguments to \code{...} concatenates the vectors element-wise
#' before testing for duplicates in the new vector.
#'
#' @param ... vectors to concatenate.
#' @param sep a string used to when concatenating the vectors. See \code{\link[base]{interaction}}.
#' @param incomparables FALSE or a vector of incomparable---i.e., never-duplicate---values. See \code{\link[base]{duplicated}}.
#' @param named when TRUE, the output is named with the
#'
#' @return A logical vector of the same length as input vectors. TRUE values mark elements that are duplicates \emph{or are duplicated}.
#'
#' @seealso \code{\link[base]{duplicated}} and \code{\link[base]{interaction}}.
#'
#' @examples
#' state <- c("CA", "CA", "FL", "CA", "FL", "FL")
#' cd    <- c(22, 11, 22, 22, NA, NA)
#'
#' data.frame(state, cd,
#'            dup = detect_duped(state, cd),
#'            dup2 = detect_duped(state, cd, incomparables = NA))
#'
#' @export


detect_duped <-
  function(..., sep = "-^-", incomparables = FALSE,
           named = FALSE, message = FALSE) {

    if(length(list(...)) > 1) combs <- paste(..., sep = sep)
    else combs <- as.vector(...)

    dups <- unique(combs[duplicated(combs, incomparables = incomparables)])

    if(require(fastmatch)) out <- combs %fin% dups
    else                   out <- combs %in% dups


    if(message) message(
      sprintf("%d instances of %d duplicated elements",
              sum(out), length(dups)
      )
    )

    if(named) names(out) <- combs

    return(out)
  }
