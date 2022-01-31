#' Merge/stack all dataframes in your list into one long dataframe.
#'
#' This will take a ridiculously long time.
#'
#' @param yourlist List of dataframes, assuming it's already been loaded into R.
#'
#'
#' @export
long_stack <- function(yourlist){

  # first the actual function
  newlst <- yourlist %>% bind_rows()

  out= c()
  print("Stacking in progress...")

  progress_bar = txtProgressBar(min=0, max=length(yourlist), style = 1, char="=")

  for(i in 1:length(yourlist)){
    out = c(out , (yourlist[[i]]))
    setTxtProgressBar(progress_bar, value = i)
  }

  close(progress_bar)

  return(newlst)
}

