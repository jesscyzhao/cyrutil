#' plot the trace plot of a mcmc chain of a parameter
#' @param param_sample: a mcmc chain of one parameter
#' @param bquote_param_name: a bquote object
#' @param sample_mean: boolean, if plot sample mean
#' @param sample_median: boolean, if plot sample median
#' @param plot_value: boolean, if plot the ind_value
#' @param ind_value: a float, the special value
#' @return plot of traceplot
#' @export
#' @keywords traceplot
#' @examples
#' x = rnorm(1000)
#' plot_traceplot(x, 'x', sample_mean=T, sample_median=T, plot_value=T, ind_value=-1)

plot_traceplot = function(param_sample, bquote_param_name, sample_mean = F, sample_median = F, plot_value=F, ind_value=NULL){
    # param_sample: a mcmc chain of one parameter
    # bquote_param_name: a bquote object
    # sample_mean: boolean, if plot sample mean
    # sample_median: boolean, if plot sample median
    # plot_value: boolean, if plot the ind_value
    # ind_value: a float, the special value
    n = length(param_sample)
    plot(param_sample, type="l", main=bquote_param_name, xlab="",  ylab="")
    if(sample_mean){
      abline(h=mean(param_sample), col="red", cex=1.5)
      }
    if(sample_median){
      abline(h=median(param_sample), col="blue", cex=1.5)
      }
      if(plot_value){
      abline(h=ind_value, col="pink", cex=1.5)
      }
  }
