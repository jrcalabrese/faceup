#' Read in a directory of OpenFace files as a list
#'
#' This will read in all OpenFace `.csv` files as a `list` from a specific directory.
#' Make sure to save it to an object or it won't work.
#'
#' @param path Character, the path to the directory of `.csv` files after running OpenFace
#'
#' @importFrom fs path_file
#' @importFrom utils read.csv
#' @importFrom purrr map
#' @importFrom dplyr %>% select everything

#' @export
#'
load_list <- function(path) {

  filelist <- list.files(path = path,
                         pattern =" *.csv",
                         full.names = TRUE)

  lst <- lapply(filelist,
                utils::read.csv,
                header = TRUE,
                stringsAsFactors = FALSE)

  names(lst) <- filelist

  namelist <- fs::path_file(filelist)
  namelist <- unlist(lapply(namelist,
                            sub,
                            pattern = ".csv",
                            replacement = ""),
                     use.names = FALSE)

  lst <- mapply(cbind, lst, "clipID" = namelist, SIMPLIFY = FALSE) # fine!

  #column_names <- faceup::column_names

  # Reorder column names
  lst <- purrr::map(lst, ~ .x %>% dplyr::select("clipID", "frame", "face_id", "timestamp", "confidence", "success",
                                  dplyr::everything()))

  return(lst)
}
