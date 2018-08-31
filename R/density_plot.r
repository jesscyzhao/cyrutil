#' Plot the density of raw data, point estimates and confidence interval.
#' @param predictive_sample: the point estimates
#' @param dat: raw data
#' @param plot_name: name of the plot
#' @param plot_ci: boolean, default false, when true, check for additonal arguments to plot the confidence interval
#' @param plot_hist: boolean, default true, when true, plot the histogram of raw data.
#' @param legend_pos: string, default 'topright', can be changed to 'topleft'.
#' @param lb: vector of lowerbound values of CI
#' @param ub: vector of upperbound values of CI
#' @param grid: vector of grid based on which lb and ub are calculated.
#' @param level: str, the confidence level.
#' @export
#' @keywords plot density
#' sim_data = rnorm(1000)
#' lb = dnorm(seq(-2, 2, by=0.01), mean=0, sd=1.1)
#' ub = dnorm(seq(-2, 2, by=0.01), mean=0, sd=0.9)
#' plot_posterior_density(sim_data, sim_data+0.01, 'Test plot', plot_ci=T, lb=lb, ub=ub, grid=seq(-2, 2, by=0.01), level='0.9')
plot_posterior_density = function(predictive_sample, dat, plot_name, plot_ci=FALSE, plot_hist=TRUE, breaks="Sturges", legend_pos='topright', ...){
    data_density = density(dat)
    point_estimate_density = density(predictive_sample)
    hist_range = c(0, max(c(data_density$y, point_estimate_density$y)))
    x_range= NULL
    if(plot_ci){
      ci_input = list(...)
      ci_input_names = names(ci_input)
      if(!("lb"%in% ci_input_names)){
        warning("lower bound not provided")
      }
      if(!("ub"%in% ci_input_names)){
        warning("upper bound not provided")
      }
      if(!("level" %in% ci_input_names)){
        warning("confidence level not provided")
      }
      if(!("grid" %in% ci_input_names)){
        warning("grid not provided")
      }
      grid = ci_input[['grid']]
      lb = ci_input[['lb']]
      ub = ci_input[['ub']]
      mean = ci_input[['mean']]
      level = ci_input[['level']]
      hist_range = c(0, max(ub))
      x_range = c(min(grid), max(grid))
      }

    if(plot_hist){
      hist(dat, main=plot_name, prob=T, ylim = hist_range, xlim = x_range, xlab='', breaks=breaks)
      lines(data_density, lwd=2, lend=0)
    }else{
      plot(data_density, type='l', lwd=2, lend=0, ylim=hist_range, xlim=x_range, xlab='', main=plot_name)
    }

    lines(point_estimate_density, lty='dashed', col='red', lwd=2)
    if(plot_ci){
      lines(grid, lb, lty='dotted', lwd=2,  col='blue')
      lines(grid, ub, lty='dotted', lwd=2, col='blue')
      lines(grid, mean, lty='dotted', lwd=2, col='green')
      legend_names = c('data', 'y_0', paste0(level,'% interval'), 'mean marginal')
      cols =  c('black', 'red', 'blue', 'green')
      ltys = c('solid', 'dashed', 'dotted', 'dotted')
    }
    else{
      legend_names = c('data', 'y_0')
      cols = c('black', 'red')
      ltys = c('solid', 'dashed')
    }
    legend(legend_pos, legend=legend_names, col=cols, lty=ltys, bty='n')
}
