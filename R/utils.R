# Get the filename of the current file, or
# the file being rendered

this_filename <- function() {
  if (interactive()) {
    filename <- rstudioapi::getSourceEditorContext()$path
  } else {
    filename <- knitr::current_input()
  }
  return(fs::path(filename))
}
