#' @name mcheck
#' @title Check match rates
#' @aliases merge_check
#'
#' @description Check match rates between two character vectors
#'
#' @param x a character vector
#' @param y a character vector
#' @param incomparables a vector of incomparable values, where FALSE means no values.
#' See \code{\link[base]{unique}}
#'
#' @return A numeric matrix of match statistics.
#'
#' @seealso \code{\link[data.table]{`data.table::`%chin%``}} and \code{\link[base]{interaction}}.
#'
#' @examples
#' A <- sample(state.abb, 50, replace = TRUE)
#' B <- sample(state.abb, 100, replace = TRUE)
#' B[sample(1:length(B), 10)] <- NA_character_
#'
#' mcheck(A, B)
#'
#' @export


mcheck <- function(x, y, incomparables = FALSE){

    xdupes <- x[duplicated(x)]
    ydupes <- y[duplicated(y)]


    x_in_y    <- mean(data.table::`%chin%`(x, y), na.rm = TRUE)
    y_in_x    <- mean(data.table::`%chin%`(y, x), na.rm = TRUE)

    in_other  <- c(x = x_in_y,                y = y_in_x)
    missing   <- c(x = mean(is.na(x)),        y = mean(is.na(y)))
    duplicate <- c(x = mean(data.table::`%chin%`(x, xdupes)),
                   y = mean(data.table::`%chin%`(y, ydupes)))

    unique    <- c(x = length(unique(x, incomparables = incomparables)),
                   y = length(unique(y, incomparables = incomparables)))

    mstats <- rbind(in_other, missing, duplicate, unique)

    return(mstats)
}

