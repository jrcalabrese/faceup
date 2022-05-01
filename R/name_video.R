#' Generate one line of text for FeatureExtraction.exe
#'
#' Generate one line of text to run in the Command Line for FeatureExtraction.exe
#' OpenFace does not allow you to run all the videos in a single directory,
#' but does allow multiple videos on the same line if each video is individually specified.
#' This may be able to be done with a Python tool, but this may be easier,
#' since I don't have to rely on a third party tool or download Python.
#'
#' @param input_dir Directory of where your files are stored.
#' @param output_dir Directory of where you want the processed OpenFace videos to be saved.
#' @param of_dir Directory of where your command is stored. Must include `FeatureExtraction.exe` at the end.
#' @param save_txt Directory of where you want the .txt file to be saved.
#' @param specify Optional. Adds `-aus` to specify that only Action Units should be computed Defaults to `FALSE`.
#'
#' @export
name_video <- function(input_dir, output_dir, of_dir, save_txt, specify){

  # If specify is missing
  if (missing(specify))
    specify <- FALSE
  else
    specify <- specify

  # Read in list of videos
  filelist <- list.files(path = input_dir,
                         pattern =" *.mp4",
                         full.names = TRUE)

  # Surround FeatureExtraction.exe location in double quotes
  of_dir <- paste0('"', of_dir, '"')

  # Surround output directory in double quotes
  output_dir <- paste0('"', output_dir, '"')

  # Add name of file to save_text
  save_txt <- paste0(save_txt, "/openface_code.txt")

  # Put `-out_dir ` in front of output dir
  output_dir <- paste0("-out_dir ", output_dir)

  # Put each video in double quotes, add `-f ` to front of each video to call on it
  filelist <- paste0('-f "', filelist, '"')

  # Collapse into one line, separated by spaces
  filelist <- paste(filelist, collapse = ' ')

  # Put of_dir in front of filelist
  filelist <- paste0(of_dir, " ", filelist)

  # Put out_dir behind filelist
  filelist <- paste0(filelist, " ", output_dir)

  # Add `-aus` if added
  if (specify == TRUE) {
    filelist <- paste0(filelist, " -aus")
  }
  else {
      filelist <- filelist
      }

  writeLines(filelist, save_txt)
}

