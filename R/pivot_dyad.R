#' Widen data by dyad member.
#'
#' Widen dataframe by dyad member.
#' At this point, you should have merged your data using `load_list` and `long stack`,
#' filtered using relevant criteria using `filter_rows`,
#' removed orphans with `remove_orphans`, and smushed with `smush_rows`.
#'
#' For dyadic data, this is the last step in the workflow.
#'
#' This will output two dataframes: 1) an extra long dataframe by dyad member, and 2)
#' a slightly wider dataframe where each dyad members has their own Action Unit column.
#'
#' @param dat Dataframe.
#' @param id_num Participant identifier. Defaults to first column in dataframe.
#' @param smushed Binary, TRUE/FALSE. Has your data been smushed with `smush_rows()`? Required.
#' @param output_dir Where you want your dataframes to be saved to.
#'
#' @importFrom dplyr %>% select starts_with all_of
#' @importFrom tidyr pivot_wider separate
#' @export
pivot_dyad <- function(dat, id_num, smushed, output_dir) {

  # I am not 100% sure this will work,
  # I need to test it on some real/non-simulated data,
  # or ideally improve the make_fake functions...

  DyadMember <- NULL

  # First, address missing
  if (missing(id_num))
    id_num <- dat[, 1]
  else
    do_nothing <- "nothing"

  # Separate identifier into number and dyad member
  long <- dat %>%
    separate(col = {{id_num}},
             into = c("id_num", "DyadMember"),
             sep = "_(?=[^_]+$)")

  # Write out very long dataframe
  path1 <- paste0(output_dir, "/openface_longer_df.csv", sep = "")
  write.csv(x = long, file = path1, row.names = FALSE)
  rm(path1)

  # ID variable
  if (smushed == FALSE) {
    idvars <- c("id_num", "frame", "timestamp")
  }
  else {
    idvars <- c("id_num", "new_timestamp")
  }

  # Action Units
  aus <- long %>%
    select(starts_with("AU")) %>%
    names()

  wide <- pivot_wider(
    data = long,
    id_cols = all_of(idvars),
    names_from = DyadMember,
    values_from = all_of(aus),
    values_fn = function(x) paste(x, collapse=""))

  path2 <- paste0(output_dir, "/openface_wider_df.csv", sep = "")
  write.csv(x = wide, file = path2, row.names = FALSE)
  rm(path2)

}
