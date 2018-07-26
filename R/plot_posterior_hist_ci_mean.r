#' Plot the histogram of posterior sample with verticle blue lines indicating the 95% confidence interval and median, red line indicating the true value.
#' @param param_sample: a mcmc chain of one parameter
#' @param bquote_param_name: a bquote object
#' @param true_value: the true value of parameter
#' @return a plot of histogram 
#' @export
#' @keywords posterior histogram
#' @examples
#' x = rnorm(1000)
#' plot_posterior_hist_ci_mean(x, 'x', 0)

plot_posterior_hist_ci_mean = function(param_sample, bquote_param_name, true_value = NULL){
    # param_sample: a mcmc chain of one parameter
    # bquote_param_name: a bquote object
    # true_value: the benchmark to be compared to
    mean = mean(param_sample)
    ci = quantile(param_sample, c(0.025, 0.975))
    par(mfrow=c(1,1))
    hist(param_sample, main = bquote_param_name)
    abline(v = mean, col="blue")
    abline(v = ci[1], col="blue")
    abline(v = ci[2], col="blue")

    if(!is.null(true_value)){
        abline(v=true_value, col="red")
    }
}
