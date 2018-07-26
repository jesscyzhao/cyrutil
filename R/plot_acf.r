#' plot the acf plot of a mcmc chain of a parameter
#' @param param_sample: a mcmc chain of one parameter
#' @param bquote_param_name: a bquote object
#' @return: a plot of acf plot
#' @export
#' @keywords acfplot
#' @examples
#' x = rnorm(1000)
#' plot_acf(x, 'x')

plot_acf = function(param_sample, bquote_param_name){
    sample_acf = acf(param_sample, plot = F)
    plot(sample_acf, main=bquote_param_name)
}
