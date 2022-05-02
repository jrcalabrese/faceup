#' Merge dataframes from a list into one long dataframe
#'
#' Merge all OpenFace dataframes from a list into a single long-form dataframe.
#' This will take a very long time, so it comes with a progress bar.
#' Your list of dataframes must already be read into R.
#' Be aware that this function will remove the `face_id` column as it is not needed for the rest of the workflow.
#'
#' @param yourlist List, a list of dataframes.
#'
#' @importFrom dplyr %>% bind_rows
#' @importFrom utils txtProgressBar
#' @export
long_stack <- function(yourlist){

  face_id <- NULL

  # first the actual function
  newlst <- yourlist %>%
    dplyr::bind_rows()

  out = c()
  print("Stacking in progress...")

  progress_bar = utils::txtProgressBar(min = 0,
                                max = length(yourlist),
                                style = 1,
                                char = "=")

  for(i in 1:length(yourlist)){
    out = c(out , (yourlist[[i]]))
    utils::setTxtProgressBar(progress_bar, value = i)
  }

  close(progress_bar)

  newlst <- newlst %>%
    select(-face_id)

  return(newlst)
}
