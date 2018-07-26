#' Update the mean and sd of nromal kernel with normal prior on mean.
#' @param x_bar: the mean of data sample
#' @param sigma: the sd of sampling distribution, should be known
#' @param n: number of data points
#' @param mu_0: the mean of the normal prior
#' @param sigma_0: the sd of the normal prior
#' @return: a random draw from the posterior normal distribution.
#' @export
#' @keywords conjugate update
#' @examples
#' normal_standard_update(1, 1, n, 1, 1)

normal_standard_update = function(x_bar, sigma, n, mu_0, sigma_0){
  normal_mean = (x_bar*n*sigma_0^2 + mu_0*sigma^2)/(n*sigma_0^2 + sigma^2)
  normal_sd = sigma*sigma_0/sqrt(n*sigma_0^2 + sigma^2)
  return(rnorm(1, normal_mean, normal_sd))
}
