#' Create fake dyadic OpenFace data.
#'
#' Each `.csv` file will have a random 5-digit ID number with the prefix "FakeOpenFace_".
#' Excluding the informational (e.g., `faceid`, `timestamp`) and the Action Unit data,
#' since the values don't make sense in this situation.
#' Each generated `.csv` file will end with a suffix indicating dyad member.
#'
#' @param path Character, path to where you want the files to go.
#' @param n Numeric, number of rows for each generated `.csv`
#' @param loop Numeric, total number of individual `.csv` files to generate.
#' @param dyad Two dyad members as a character vector. E.g., `c("Mother", "Daughter)`.
#' @param seed Numeric, random seed. Optional.
#'
#' @importFrom dplyr mutate %>% across everything ends_with
#' @importFrom stats runif
#' @importFrom stringr str_sub str_remove_all
#'
#' @export
make_fake2 <- function(path, n, loop, dyad, seed) {

  if (missing(seed))
    seed <- 1227
  else
    seed <- seed

  timestamp <- gaze_0_x <- p_33 <- NULL

  for(i in 1:loop) {

    set.seed(seed)

    dat <- matrix(0, ncol = 329, nrow = n) %>% as.data.frame()

    #colnames(dat) <- faceup::column_names
    colnames(dat) <- column_names

    # For the first dyad member
    dat1 <- dat %>%
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

    # For the second dyad member
    dat2 <- dat %>%
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

    # For first dyad member
    x1 <- paste("FakeOpenFace_", x, sep = "")
    y1 <- dyad[1]
    x1 <- paste(x1, "_", y1, sep="")
    newpath1 <- file.path(path, paste(x1, ".csv", sep = ""))
    write.csv(dat1, file = newpath1, row.names = FALSE)

    # For the second dyad member
    x2 <- paste("FakeOpenFace_", x, sep = "")
    y2 <- dyad[2]
    x2 <- paste(x2, "_", y2, sep="")
    newpath2 <- file.path(path, paste(x2, ".csv", sep = ""))
    write.csv(dat2, file = newpath2, row.names = FALSE)

    rm(newpath1, newpath2,
       dat1, dat2,
       x1, x2,
       y1, y2)

  }

}
