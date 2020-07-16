#' Write list elements to disk as separate objects
#' @name save_list
#'
#'
#' @param x a list of objects to save.
#' @param dir a directory in which to save the list elements.
#' @param ext a single extension for all files.
#' @param names a vector of file names without extensions. Defaults to the names of \code{x}.
#' @param cores number of cores to use in \code{\link{mcparallel}}. The default (1) runs in series. Note that several file-writing functions already implement multi-threading.
#' @param ... further arguments to pass to the file-writing function:
#'            \code{\link{saveRDS}},
#'            \code{\link[fst]{write_fst}}, \code{\link[qs]{qsave}},
#'            \code{\link[data.table]{fwrite}},
#'            or \code{\link[ggplot2]{ggsave}}.
#' @seealso \code{\link{saveRDS}}, \code{\link[data.table]{fwrite}}, or \code{\link[ggplot2]{ggsave}}.
#' @examples
#'
#' tmp_dir <- tempdir()
#' df <- data.frame(char = LETTERS[1:10], num = 1:10)
#' ldf <- replicate(3, df, simplify = FALSE)
#'
#' save_list(ldf, dir = tmp_dir, ext = ".csv")
#'
#' list.files(tmp_dir)
#' unlink(tmp_dir, recursive = TRUE)
#'
#' @export

save_list <-
  function(x, dir, ext = c(".fst", ".qs", ".rds", ".csv", ".pdf"), names = NULL,
           cores = 1, compress = 100, ...){
    if(!file.exists(dir)) dir.create(dir)

    if(is.null(names)) names <- if(is.null(names(x))) 1:length(x) else names(x)

    ext <- match.arg(ext)

    invisible(

      switch(ext,

             .fst = parallel::mclapply(names,
                                       function(nm) fst::write_fst(x[[nm]], file.path(dir, paste0(nm, ext)),
                                                                   compress = compress, ...),
                                       mc.cores = cores),

             .qs  = parallel::mclapply(names,
                                       function(nm) qs::qsave(x[[nm]], file.path(dir, paste0(nm, ext)),
                                                              ...),
                                       mc.cores = cores),

             .rds = parallel::mclapply(names,
                                       function(nm) saveRDS(x[[nm]],
                                                            file.path(dir, paste0(nm, ext)),
                                                            ...),
                                       mc.cores = cores),

             .csv = parallel::mclapply(names,
                                       function(nm) data.table::fwrite(x[[nm]],
                                                                       file = file.path(dir, paste0(nm, ext)),
                                                                       ...),
                                       mc.cores = cores),

             .pdf = parallel::mclapply(names,
                                       function(nm) ggplot2::ggsave(plot = x[[nm]],
                                                                    filename = file.path(dir, paste0(nm, ext)),
                                                                    ...),
                                       mc.cores = cores)
      )

    )
  }
