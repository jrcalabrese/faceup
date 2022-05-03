#' Clean up your filtered dataset.
#'
#' Try not to leave any orphans behind. Specific to dyadic data.
#'
#' @param dat Dataframe.
#' @param id_num Participant identifier. Must be stored like so, e.g., "OpenFace_01234_Mother".
#' Defaults to the first column in a dataframe.
#' @param output_dir Where you want the output to be saved to.
#'
#' @importFrom dplyr %>% coalesce select distinct mutate
#' @importFrom tidyr pivot_wider separate
#' @importFrom stats sd complete.cases
#'
#' @export
remove_orphans <- function(dat, id_num, output_dir){

  newID <- DyadMember <- Daughter <- Mother <- Member <- RemoveThese <- NULL

  # First, address missing
  if (missing(id_num))
    id_num <- dat[, 1]
  else
    do_nothing <- "nothing"

  # Output the IDs that you are removing
  orphans <- dat %>%
    select({{id_num}}) %>%
    separate({{id_num}}, into = c("newID", "DyadMember"), sep="_(?=[^_]+$)") %>%
    distinct(.keep_all = TRUE) %>%
    pivot_wider(id_cols = newID,
                names_from = DyadMember,
                values_from = DyadMember)

  orphans <- orphans[!complete.cases(orphans), ]
  orphans[is.na(orphans)] <- "Removed during filter_rows() due to low/success"
  orphans$Daughter[orphans$Daughter == "Daughter"] <- "Adequate confidence/success"
  orphans$Mother[orphans$Mother == "Mother"] <- "Adequate confidence/success"

  path1 <- paste0(output_dir, "/orphaned_dyadmembers.csv")
  write.csv(x = orphans,
            file = path1,
            row.names = FALSE)
  rm(orphans)

  # Now you can remove them
  orphans <- dat %>%
    select({{id_num}}) %>%
    separate({{id_num}}, into = c("newID", "DyadMember"), sep="_(?=[^_]+$)") %>%
    distinct(.keep_all = TRUE) %>%
    pivot_wider(id_cols = newID,
                names_from = DyadMember,
                values_from = DyadMember)

  orphans <- orphans[!complete.cases(orphans), ]

  orphans <- orphans %>%
    mutate(Member = coalesce(Daughter, Mother)) %>%
    mutate(RemoveThese = paste0(newID, "_", Member)) %>%
    select(RemoveThese)

  final <- filter(dat, !({{id_num}} %in% orphans$RemoveThese))
  rm(dat, orphans)

  return(final)

}

