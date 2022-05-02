#' Make OpenFace `.csv` files narrower
#'
#' Subset only relevant columns from OpenFace data: the first five columns and the Action Units.
#'
#' @param input_dir Character, path to where all your big files currently live.
#' @param output_dir Character, path to where you want to write out the new smaller files.
#'
#' @importFrom dplyr %>% select starts_with
#' @importFrom fs path_file
#' @importFrom utils write.csv read.csv
#' @importFrom purrr map
#' @export
make_small <- function(input_dir, output_dir) {

  filelist <- list.files(path = input_dir,
                         pattern = "*.csv",
                         full.names = TRUE)

  lst <- lapply(filelist,
                utils::read.csv,
                header = TRUE,
                stringsAsFactors = FALSE)

  lst <- purrr::map(lst, ~ (.x %>% dplyr::select(
    frame, face_id, timestamp, confidence, success,
    dplyr::starts_with("AU")
  )))

  namelist <- filelist
  namelist <- fs::path_file(filelist)
  names(lst) <- namelist

  lapply(1:length(lst), function(i) utils::write.csv(lst[[i]],
                                              file = file.path(output_dir, names(lst[i])),
                                              row.names = FALSE))
}
