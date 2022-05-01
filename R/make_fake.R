#' Create fake OpenFace data.
#'
#' Each `.csv` file will have a random 15-digit ID number with the prefix "fake_".
#' Excluding the informational (e.g., `faceid`, `timestamp`) and the Action Unit data, the values don't make sense.
#'
#' @param path Character, path to where you want the files to go.
#'
#' @param n Numeric, number of rows for each generated `.csv`
#'
#' @param loop Numeric, total number of individual `.csv` files to generate.
#'
#' @param seed Numeric, random seed.
#'
#' @importFrom dplyr mutate %>% across everything ends_with
#' @importFrom stats runif
#' @importFrom stringr str_sub str_remove_all
#'
#' @export
make_fake <- function(path, n, loop, seed) {

  if (missing(seed))
    seed <- 1227
  else
    seed <- seed

  set.seed(seed)

  timestamp <- gaze_0_x <- p_33 <- NULL

  for(i in 1:loop) {

  dat <- matrix(0, ncol = 329, nrow = n) %>% as.data.frame()

  #colnames(dat) <- faceup::column_names
  colnames(dat) <- column_names

  dat <- dat %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
    dplyr::mutate(frame = 1:n) %>%
    dplyr::mutate(face_id = 1) %>%
    dplyr::mutate(timestamp = seq(from = 1, to = n, by = 1)) %>%
    dplyr::mutate(timestamp = (timestamp/3)) %>%
    dplyr::mutate(confidence = stats::runif(n,0.35, 0.97)) %>%
    dplyr::mutate(success = sample(c(0, 1), replace = TRUE, size = n)) %>%
    dplyr::mutate(dplyr::across(gaze_0_x:p_33, ~ rnorm(n = n, mean = 0, sd = 5))) %>% # eh
    dplyr::mutate(dplyr::across(dplyr::ends_with("_r"), ~ sample((seq(from = 0, to = 5, by = .05)), size = n, replace = TRUE))) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_c"), ~ sample(c(0, 1), replace = TRUE, size = n)))

  # make new file path, generate random ID numbers
  x <- stringr::str_sub( (as.numeric(Sys.time())*47), -6, -1)
  x <- stringr::str_remove_all(x, "[.]")
  x <- paste("FakeOpenFace_", x, sep="")

  newpath <- file.path(path, paste(x, ".csv", sep = ""))

  write.csv(dat, file = newpath, row.names = FALSE)

  rm(newpath, x)

  }
}
