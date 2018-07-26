#' Summarize the mcmc sample that lives in a dataframe, return the summary of each column as a row of present table.
#' @param sample_df: the dataframe whose columns are the mcmc chains of parameters.
#' @return a dataframe that summarizes the posterior samples
#' @export
#' @keywords posterior summary
#' @examples
#' x = rnorm(1000)
#' y = rnorm(1000, 10, 1)
#' output=cbind(x=x, y=y)
#' sum_samples(output)

sum_samples = function(sample_df){
  sum_stats = function(x){c(mean(x), sd(x), quantile(x, c(.025,.25,.5,.75,.975)))}

  present_table = c()
  for (i in 1:dim(sample_df)[2]){
    present_table = rbind(present_table, sum_stats(sample_df[, i]))
    }
  colnames(present_table) = c("Mean", "Sd", "2.5%", "25%", "50%", "75%", "97.5%")
  rownames(present_table) = colnames(sample_df)
  return(present_table)
}
