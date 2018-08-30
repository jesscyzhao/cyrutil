####################################################
#' Find the global arguments not given by the argument list
#' @param fn: a function to be checked
#' @return a vector of string
#' @export
#' @keywords coding
#' @examples
#' find_global_arg(fn)

find_global_arg = function(fn){
  require(codetools, quietly=TRUE)
  return(codetools::findGlobals(fn))
}
