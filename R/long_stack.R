#' Merge dataframes from a list into one long dataframe
#'
#' Merge all OpenFace dataframes from a list into a single long-form dataframe.
#' This will take a very long time, so it comes with a progress bar.
#' Your list of dataframes must already be read into R.
#'
#' @param yourlist List, a list of dataframes.
#'
#' @importFrom dplyr %>% bind_rows
#' @importFrom utils txtProgressBar
#' @export
long_stack <- function(yourlist){

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

  return(newlst)
}
