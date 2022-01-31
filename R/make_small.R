#' Make OpenFace `.csv` files narrower
#'
#' We only need the first few columns and the Action Unit columns any way.
#'
#' @param bigpath Path to where all your big files currently live.
#'
#' @param newpath Path to where you want to write out the new smaller files.
#'
#' @export
make_small <- function(bigpath, newpath) {

  filelist <- list.files(path = bigpath,
                         pattern = "*.csv",
                         full.names = TRUE)

  lst <- lapply(filelist,
                read.csv,
                header = TRUE,
                stringsAsFactors = FALSE)

  lst <- map(lst, ~ (.x %>% select(
    frame, face_id, timestamp, confidence, success,
    starts_with("AU")
  )))

  namelist <- filelist
  namelist <- fs::path_file(filelist)
  names(lst) <- namelist

  lapply(1:length(lst), function(i) write.csv(lst[[i]],
                                              file = file.path(newpath, names(lst[i])),
                                              row.names = FALSE))
}
