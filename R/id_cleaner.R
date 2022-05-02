#' Clean your participant identifier.
#'
#' Removes prefix from your participant identifier.
#' This assumes your participant ID number are all the digits following the last underscore (e.g., "OpenFace_01234").
#' Do not apply to raw dyadic data (e.g., "OpenFace_01234_Mother").
#'
#' @param dat Dataframe
#' @param id_num Participant identifier.
#'
#' @importFrom dplyr %>% mutate across
#' @export
id_cleaner <- function(dat, id_num){

  dat <- dat %>%
    mutate(across(id_num, ~ gsub(".*_", "", .)))

}
