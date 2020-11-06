#' @name read_rda
#'
#' @title Read an .rda file and return its contents
#'
#' @description Reads the contents of an .RData file into a separate environment
#' (to avoid overwriting existing variables) and returns the contents for assignment
#' to your own variable names.
#'
#' @param file a string with the file name and path.
#'
#' @return An object or list of objects stored in \code{file}.
#' @export


read_rda <- function(file){

    env <- new.env()
    new_objnms <- load(file, env)

    # If file had multiple objects, build a list to return
    out <-
        if(length(new_objnms) == 1) get(new_objnms, envir = tenv)
    else mget(new_objnm, envir = tenv)

    return(out)
}
