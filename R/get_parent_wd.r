####################################################
#' Get the parent working directory of the current file
#' @param
#' @return a string of the parent directory
#' @export
#' @keywords directory
#' @examples
#' get_parent_wd()

get_parent_wd = function(){
  cwd = getwd()
  setwd("..")
  pwd = getwd()
  setwd(cwd)
  return(pwd)
}

get_x_directory = function(pwd, subdir, ...){
  # Can be achived with file.path; redundant
  # the use of ..., 1) can put in as many arguments as possible, 2) need to be passed to a function
  # Sys.info() gives os name, .Platform$file.sep gives the file seperator
  # osname = Sys.info()['sysname']
  subsubdir = c(...)
  path_sep = .Platform$file.sep
  file_path = paste(c(pwd, subdir, subsubdir), collapse=path_sep)
  return(file_path)
}
