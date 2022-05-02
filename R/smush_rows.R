#' Average rows by a specified amount of time.
#'
#' Average rows by a specified amount of time, e.g., 5 frames, 10 frame
#' Be aware that this will remove the following variables: frame, timestamp, confidence, and success.
#'
#' @param dat Dataframe.
#' @param id_num Your participant identifier. Defaults to first column in the dataframe.
#' @param smush To what extent do you want to average rows together?
#' For example, if you specify `10`, then each chunk of 10 rows will be averaged together.
#' Keep in mind that each row is 1/3 of a second. Defaults to 10.
#'
#' @importFrom dplyr %>% group_by summarize across starts_with
#' @export
smush_rows <- function(dat, id_num, smush){

  if (missing(id_num))
    id_num <- dat[, 1]
  else
    do_nothing <- "nothing"

  if (missing(smush))
    smush <- 30
  else
    smush <- smush

  x <- dat %>%
    group_by({{id_num}},
             new_timestamp = as.integer(gl(n(), smush, n()))) %>%
    summarize(across(starts_with("AU"), mean),
              .groups = "drop")

  return(x)

}
