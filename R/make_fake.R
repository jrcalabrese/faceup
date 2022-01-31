#' Create fake OpenFace data.
#'
#' Each `.csv` file will have a random 15-digit ID number with the prefix "fake_".
#' Excluding the informational (e.g., `faceid`, `timestamp`) and the Action Unit data, the values don't make sense.
#'
#' @param path Path to where you want the files to go.
#'
#' @param n Number of rows for each generated `.csv`
#'
#' @param loop Total number of individual `.csv` files to generate.
#'
#' @param seed Random seed.
#'
#' @export
#'

make_fake <- function(path, n, loop, seed) {

  for(i in 1:loop) {

  set.seed(seed)

  dat <- matrix(0, ncol = 329, nrow = n) %>% as.data.frame()

  column_names <- faceup::column_names
  colnames(dat) <- column_names

  dat <- dat %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(frame = 1:n) %>%
    mutate(face_id = 1) %>% # let's not get too complicated
    mutate(timestamp = seq(from = 1, to = n, by=1)) %>%
    mutate(timestamp = (timestamp/3)) %>%
    mutate(confidence = runif(n,0.85,0.97)) %>%
    mutate(success = sample(c(0,1), replace=TRUE, size = n)) %>%
    mutate(across(gaze_0_x:p_33, ~ rnorm(n = n, mean = 0, sd = 5))) %>% # eh
    mutate(across(ends_with("_r"), ~ sample((seq(from=0, to=5, by=.05)), size=n, replace=TRUE))) %>%
    mutate(across(ends_with("_c"), ~ sample(c(0,1), replace=TRUE, size = n)))

  # make new file path
  x <- as.numeric(Sys.time())*100000
  x <- paste("fake_", x, sep="")

  newpath <- file.path(path, paste(x, ".csv", sep=""))

  write.csv(dat, file = newpath, row.names = FALSE)

  rm(newpath, x)

  }
}
