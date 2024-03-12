#' Curve
#'
#' @param mu mu
#' @param sigma sigma
#' @param a a
#'
#' @return none
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a) {
  curve(dnorm(x, mean=mu, sd=sigma), xlim=c(mu - 3*sigma, mu + 3*sigma), ylab="Density", xlab="x")
  xvals = seq(mu - 3*sigma, a, length.out=1000)
  yvals = dnorm(xvals, mean=mu, sd=sigma)
  polygon(c(mu - 3*sigma, xvals, a), c(0, yvals, 0), col="lightgray")
  prob = pnorm(a, mean=mu, sd=sigma)
  text(20, 0.05, paste("P(X<=", a, ")"))
  list(mu = mu, sigma = sigma, probability = prob)
}
