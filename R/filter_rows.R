#' Filter rows based on your exclusion criteria.
#'
#' Filter rows and/or remove entire participants with poor confidence and/or low success.
#' Note that `conf_removal` and `succ_removal` will occur before row filtering in that order.
#' Then rows will be filtered first by confidence, then success.
#'
#' @param dat Your long-form dataframe.
#' @param conf_removal Numeric. Do you want to remove entire participants based on average confidence score?
#' Specify threshold here. Will keep all participants above this average. Defaults to 0.
#' @param succ_removal Do you want to remove entire participants based on average number of unsuccessful rows?
#' Specify threshold here. Will keep all participants above this threshold. Defaults to 0.
#' @param conf_thres Numeric. Your confidence threshold. Will keep all rows above the specified threshold. Makes NA, does not remove.
#' @param succ_thres Binary, TRUE/FALSE. Do you want to remove individual rows where success is zero? Defaults to FALSE. Makes NA, does not remove.
#' @param output_dir A `.txt` file will generated on who was removed and what the exclusion criteria was.
#' Where should this output file be saved?
#'
#' @importFrom stats aggregate
#' @importFrom dplyr %>% group_by ungroup filter mutate select summarize if_else distinct n rowwise
#'
#' @export
filter_rows <- function(dat, conf_removal, succ_removal, conf_thres, succ_thres, output_dir) {

  clipID <- confidence <- avg_confidence <- keep_conf <- success <- avg_success <- keep_succ <- keep_conf_row <- keep_succ_row <- AU01_r <- AU45_c <-

  # Address the presence or absence of conf_removal and succ_removal
  if (missing(conf_removal))
    conf_removal <- 0
  else
    conf_removal <- conf_removal

  if (missing(succ_removal))
    succ_removal <- 0
  else
    succ_removal <- succ_removal

  if (missing(succ_thres))
    succ_thres <- FALSE
  else
    succ_thres <- succ_thres

  # Remove entire "bad" participants
  ## Confidence
  dat <- dat %>%
    group_by(clipID) %>%
    mutate(avg_confidence = mean(confidence)) %>%
    ungroup() %>%
    mutate(keep_conf = if_else(avg_confidence > conf_removal, "KEEP", "TRASH"))

  removed_conf_list <- dat %>%
    filter(keep_conf %in% "TRASH") %>%
    select(clipID, avg_confidence) %>% distinct()

  path <- paste0(output_dir, "/participant_removed_low_conf.csv")
  write.csv(removed_conf_list, path, row.names = FALSE)
  rm(removed_conf_list, path)

  dat <- dat %>%
    filter(keep_conf %in% "KEEP") %>%
    select(-c(avg_confidence, keep_conf))

  ## Success
  dat <- dat %>%
    group_by(clipID) %>%
    mutate(avg_success = mean(success)) %>%
    ungroup() %>%
    mutate(keep_succ = if_else(avg_success > succ_removal, "KEEP", "TRASH"))

  removed_succ_list <- dat %>%
    filter(keep_succ %in% "TRASH") %>%
    select(clipID, avg_success) %>% distinct()

  path <- paste0(output_dir, "/participant_removed_low_success.csv")
  write.csv(removed_succ_list, path, row.names = FALSE)
  rm(removed_succ_list, path)

  dat <- dat %>%
    filter(keep_succ %in% "KEEP") %>%
    select(-c(avg_success, keep_succ))

  # Remove select rows due to badness
  ## Confidence
  dat <- dat %>%
    mutate(keep_conf_row = if_else(confidence > conf_thres, "KEEP", "TRASH"))

  filtered_conf_list <- dat %>%
    filter(keep_conf_row %in% "TRASH") %>%
    select(clipID, confidence) %>%
    group_by(clipID) %>%
    summarize(Number_of_Rows_Removed_LowConf = n())

  path <- paste0(output_dir, "/rows_removed_low_conf.csv")
  write.csv(filtered_conf_list, path, row.names = FALSE)
  rm(filtered_conf_list, path)

  dat <- dat %>%
    #filter(keep_conf_row %in% "KEEP") %>%
    #select(-c(keep_conf_row)) #%>%
    #mutate(across(everything(), ~ as.numeric(.x))) %>%
    rowwise() %>%
    #mutate(across(everything(), .fns = ~ ifelse(keep_conf_row == "TRASH", yes = NA, .x))) %>%
    mutate(across(AU01_r:AU45_c, .fns = ~ ifelse(keep_conf_row == "TRASH", yes = NA, .x))) %>%
    select(-keep_conf_row) %>%
    ungroup()

  ## Success
  if (succ_thres == TRUE) {

    dat <- dat %>%
      mutate(keep_succ_row = if_else(success == 1, "KEEP", "TRASH"))

    filtered_succ_list <- dat %>%
      filter(keep_succ_row %in% "TRASH") %>%
      select(clipID, success) %>%
      group_by(clipID) %>%
      summarize(Number_of_Rows_Removed_0Success = n())

    path <- paste0(output_dir, "/rows_removed_zero_success.csv")
    write.csv(filtered_succ_list, path, row.names = FALSE)
    rm(filtered_succ_list, path)

    dat <- dat %>%
      #filter(keep_succ_row %in% "KEEP") %>%
      #select(-c(keep_succ_row)) #%>%
      #mutate(across(everything(), ~ as.numeric(.x))) %>%
      rowwise() %>%
      #mutate(across(everything(), .fns = ~ ifelse(keep_succ_row == "TRASH", yes = NA, .x))) %>%
      mutate(across(AU01_r:AU45_c, .fns = ~ ifelse(keep_succ_row == "TRASH", yes = NA, .x))) %>%
      select(-keep_succ_row) %>%
      ungroup()

  }

    else {
      dat <- dat

    }

}


