#' Read in a directory of OpenFace files and do stuff.
#'
#' This will load all OpenFace `.csv` files as a `list`.
#' Make sure to save it to an object or it won't work.
#'
#' @param path Path to your directory of `.csv` files after running OpenFace.
#'
#' @export
load_openface <- function(path) {

  filelist <- list.files(path = path,
                         pattern =" *.csv",
                         full.names = TRUE)

  lst <- lapply(filelist,
                read.csv,
                header = TRUE,
                stringsAsFactors = FALSE)

  names(lst) <- filelist

  lst
}
