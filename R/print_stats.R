#' Calculate statistics on confidence and succcess for a dataframe.
#'
#' Saves statistics on confidence and data for each participant to .csv files.
#'
#' @param dat Dataframe
#' @param id_num Participant identifier variables. Defaults to the first column in your dataframe.
#' @param output_dir Where output should be saved to.
#'
#' @importFrom dplyr %>% group_by mutate select distinct
#' @export
print_stats <- function(dat, id_num, output_dir){

  confidence <- sd <- avg_confidence <- sd_confidence <- success <- avg_success <- sd_success <- enquo <- NULL

  id_num <- enquo(id_num)

  if (missing(id_num))
    id_num <- dat[, 1]
  else
    id_num <- id_num

  conf_by_id <- dat %>%
    group_by({{id_num}}) %>%
    mutate(avg_confidence = mean(confidence), sd_confidence = sd(confidence)) %>%
    ungroup() %>%
    select({{id_num}}, avg_confidence, sd_confidence) %>%
    distinct() %>%
    as.data.frame()

  path1 <- paste0(output_dir, "/openface_conf_rawstats.csv")
  write.csv(conf_by_id, path1, row.names = FALSE)
  rm(conf_by_id, path1)

  succ_by_id <- dat %>%
    group_by({{id_num}}) %>%
    mutate(avg_success = mean(success), sd_success = sd(success)) %>%
    ungroup() %>%
    select({{id_num}}, avg_success, sd_success) %>%
    distinct() %>%
    as.data.frame()

  path2 <- paste0(output_dir, "/openface_succ_rawstats.csv")
  write.csv(succ_by_id, path2, row.names = FALSE)
  rm(succ_by_id, path2)

}
