#' Title
#'
#' @param mu
#' @param sigma
#' @param a
#'
#' @return
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a) {
  curve(dnorm(x, mean=mu, sd=sigma), xlim=c(mu - 3*sigma, mu + 3*sigma), ylab="Density", xlab="x")
  xvals = seq(mu - 3*sigma, a, length.out=1000)
  yvals = dnorm(xvals, mean=mu, sd=sigma)
  polygon(c(mu - 3*sigma, xvals, a), c(0, yvals, 0), col="lightgray")
  prob = pnorm(a, mean=mu, sd=sigma)
  abline(v = a, col = "red", lwd = 2)
  text(a, dnorm(a, mean=mu, sd=sigma), paste("P(X<=", a, ")=", round(prob, 4)), pos=4)
  list(mu = mu, sigma = sigma, probability = prob)
}