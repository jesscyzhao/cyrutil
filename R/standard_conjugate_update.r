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

normal_standard_update_mean = function(x_bar, sigma, n, mu_0, sigma_0){
  normal_mean = (x_bar*n*sigma_0^2 + mu_0*sigma^2)/(n*sigma_0^2 + sigma^2)
  normal_sd = sigma*sigma_0/sqrt(n*sigma_0^2 + sigma^2)
  return(rnorm(1, normal_mean, normal_sd))
}

#' Update the sd of nromal kernel with inverse gamma prior
#' @param mu: the mean of sampling distribution, known
#' @param dat: the observed normal draws
#' @param n: number of data points
#' @param sigma_a: the IG prior shape
#' @param sigma_b: the IG prior rate
#' @param re_sd: boulean, true if return sd, o.w. return variance.
#' @return: a random draw from IG, or the sqrt of that, depending on re_sd.
#' @export
#' @keywords conjugate update
#' @examples
#' normal_standard_update_var(1, 1, n, 1, 1)

normal_standard_update_var = function(mu, dat, n, sigma_a, sigma_b, re_sd=T){
  sigma_shape = n/2 + sigma_a
  sigma_rate = sum((dat-mu)^2)/2 + sigma_b
  var_or_sd = ifelse(re_sd, 1/sqrt(rgamma(1, sigma_shape, sigma_rate)), 1/(rgamma(1, sigma_shape, sigma_rate)))
  return(var_or_sd)
}
