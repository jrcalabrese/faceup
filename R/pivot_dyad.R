#' Widen data by dyad member.
#'
#' Widen dataframe by dyad member.
#' At this point, you should have merged your data using `load_list` and `long stack`
#' and filtered using relevant criteria using `filter_rows`.
#' This will output two dataframes: 1) an extra long dataframe by dyad member, and 2)
#' a slightly wider dataframe where each dyad members has their own Action Unit column.
#'
#' @param dat Dataframe.
#' @param output_dir Where you want your dataframes to be saved to.
#'
#' @importFrom dplyr %>% select starts_with all_of
#' @importFrom tidyr pivot_wider separate
#' @export
pivot_dyad <- function(dat, output_dir) {

  face_id <- clipID <- DyadMember <- NULL

  # Separate identifier into number and dyad member
  long <- dat %>%
    select(-face_id) %>% # Get rid, it'll only confuse things
    separate(clipID, into = c("clipID", "DyadMember"), sep="_(?=[^_]+$)")

  # Write out very long dataframe
  path1 <- paste0(output_dir, "/OpenFace_LongerDF.csv", sep = "")
  write.csv(x = long, file = path1, row.names = FALSE)
  rm(path1)

  # ID variable
  idvars <- c("clipID", "frame", "timestamp")

  # Action Units
  aus <- long %>%
    select(starts_with("AU")) %>%
    names()

  wide <- pivot_wider(
    data = long,
    id_cols = idvars,
    names_from = DyadMember,
    values_from = all_of(aus))

  path2 <- paste0(output_dir, "/OpenFace_WiderDF.csv", sep = "")
  write.csv(x = wide, file = path2, row.names = FALSE)
  rm(path2)

}
