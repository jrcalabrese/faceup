#' Filter rows based on your exclusion criteria.
#'
#' Filter rows and/or remove entire participants with poor confidence and/or low success.
#' To use this function, you must already have one single dataset in long-form,
#' like after using `long_stack()`.
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


## Remove entire bad dyads
# How many IDs are there?
length(unique(data$clipID))
# Remove low confidence for right side
unconfidenceright <- aggregate( confidence_right ~ clipID + GROUP, data, mean)
unconfidencerightlist <- data.frame(unconfidenceright$clipID[unconfidenceright$confidence_right < .75])
colnames(unconfidencerightlist) <- "unconfidence"
#data <- data[ ! data$clipID %in% unconfidencerightlist$unconfidence, ]
# Remove low confidence for left side
unconfidenceleft <- aggregate( confidence_left ~ clipID + GROUP, data, mean)
unconfidenceleftlist <- data.frame(unconfidenceleft$clipID[unconfidenceleft$confidence_left < .75])
colnames(unconfidenceleftlist) <- "unconfidence"
#data <- data[ ! data$clipID %in% unconfidenceleftlist$unconfidence, ]
# Remove low success for right side
unsuccessright <- aggregate( success_right ~ clipID + GROUP, data, mean)
unsuccessrightlist <- data.frame(unsuccessright$clipID[unsuccessright$success_right < .90])
colnames(unsuccessrightlist) <- "unsuccess"
#data <- data[ ! data$clipID %in% unsuccessrightlist$unsuccess, ]
# Remove low success for left side
unsuccessleft <- aggregate( success_left ~ clipID + GROUP, data, mean)
unsuccessleftlist <- data.frame(unsuccessleft$clipID[unsuccessleft$success_left < .90])
colnames(unsuccessleftlist) <- "unsuccess"
#data <- data[ ! data$clipID %in% unsuccessleftlist$unsuccess, ]
# Do the big removal
data <- data[ ! data$clipID %in% unconfidencerightlist$unconfidence, ]
data <- data[ ! data$clipID %in% unconfidenceleftlist$unconfidence, ]
data <- data[ ! data$clipID %in% unsuccessrightlist$unsuccess, ]
data <- data[ ! data$clipID %in% unsuccessleftlist$unsuccess, ]
# How many IDs are there?
length(unique(data$clipID))
# Is right side still good?
aggregate( success_right ~ clipID, data, mean) # All should be above .90
aggregate( confidence_right ~ clipID, data, mean) # All should be above .75
# Is the left side still good?
aggregate( success_left ~ clipID, data, mean) # All should be above .90
aggregate( confidence_left ~ clipID, data, mean) # All should be above .75



## Remove individual frames/rows that are bad
# This is removing individual rows from dyads that are success=0 or confidence<.75
# That weren't included dyads that were just straight up removed
# This is kind of iffy because I'm really tearing apart the videos
# Removing rows where success = 0 and confidence < .75
data <- filter(data,
               success_left != 0 & success_right != 0 & # where success == 0
                 confidence_left > .75 & confidence_right > .75) # where confidence < .75
# Is right side still good?
aggregate( success_right ~ clipID, data, mean) # All should be above .90
aggregate( confidence_right ~ clipID, data, mean) # All should be above .75
# Is the left side still good?
aggregate( success_left ~ clipID, data, mean) # All should be above .90
aggregate( confidence_left ~ clipID, data, mean) # All should be above .75
# How many IDs are there?
length(unique(data$clipID))

## Identify the bad dyads that we just removed
# Put all the bad dyads in a list and rename columns in those lists
colnames(unconfidencerightlist) <- "clipID"
colnames(unconfidenceleftlist) <- "clipID"
colnames(unsuccessrightlist) <- "clipID"
colnames(unsuccessleftlist) <- "clipID"
# Merge the bad stuff so we have clipID and GROUP as columns in the same dataframe
# But here it's still split up by side and confidence/success
unconfidencerightlist <- merge(unconfidencerightlist, unconfidenceright, by="clipID")
unconfidenceleftlist <- merge(unconfidenceleftlist, unconfidenceleft, by="clipID")
unsuccessrightlist <- merge(unsuccessrightlist, unsuccessright, by="clipID")
unsuccessleftlist <- merge(unsuccessleftlist, unsuccessleft, by="clipID")
# Remove these because we don't need them anymore
rm(unconfidenceright)
rm(unconfidenceleft)
rm(unsuccessright)
rm(unsuccessleft)
# Merge those four dataframes vertically
bad_dyads <- dplyr::bind_rows(unconfidencerightlist,
                              unconfidenceleftlist,
                              unsuccessrightlist,
                              unsuccessleftlist,
                              .id = "index")
bad_dyads <- melt(bad_dyads, na.rm=TRUE)
# Write out this list of bad_dyads to server
write.csv(bad_dyads, "~/firstyearproject/bad_dyads.csv", row.names = FALSE)
# One more check
mean(data$success_left)
mean(data$success_right)
mean(data$confidence_left)
mean(data$confidence_right)
# Clean up
rm(unsuccessleftlist)
rm(unsuccessrightlist)
rm(unconfidenceleftlist)
rm(unconfidencerightlist)
rm(bad_dyads)
# Are there any NA values?
colSums(is.na(data)) # Good!

}
