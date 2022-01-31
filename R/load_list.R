#' Read in a directory of OpenFace files and do stuff.
#'
#' This will load all OpenFace `.csv` files as a `list`.
#' Make sure to save it to an object or it won't work.
#'
#' @param path Path to your directory of `.csv` files after running OpenFace.
#'
#' @export
load_list <- function(path) {

  filelist <- list.files(path = path,
                         pattern =" *.csv",
                         full.names = TRUE)

  lst <- lapply(filelist,
                read.csv,
                header = TRUE,
                stringsAsFactors = FALSE)

  names(lst) <- filelist

  namelist <- fs::path_file(filelist)
  namelist <- unlist(lapply(namelist, sub, pattern = ".csv", replacement = ""),
                     use.names = FALSE)

  lst <- mapply(cbind, lst, "clipID" = namelist, SIMPLIFY = FALSE)

  lst <- lapply(lst, FUN = function(x){x[, c("clipID", names(x), "clipID")]})

  lst
}
