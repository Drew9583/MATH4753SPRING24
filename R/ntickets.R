#' ntickets
#'
#' @param N Number of seats
#' @param gamma Probability a plane will be truly overbooked
#' @param p Probability of a "show"
#'
#' @return A list containing nd, nc, N, p and gamma
#' @export
#'
#' @import stats
#' @import graphics
#'
#' @examples
#' \dontrun{ntickets(N=400,gamma = 0.02, p = 0.95)}
ntickets = function(N, gamma, p) {
  nd = N

  nc = qnorm(1 - gamma, mean = N * p, sd = sqrt(N * p * (1 - p)))

  results = list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)

  n_values = seq(from = N, to = max(nd, nc) + 10, by = 1)
  probabilities = pbinom(N, size = n_values, prob = p, lower.tail = FALSE)
  plot(n_values, probabilities, type = 'o', xlab = 'n', ylab = 'Objective',
       main = 'Objective Vs n to find optimal tickets sold (Discrete)')
  abline(h = gamma, col = 'red')
  points(results$nd, gamma, col = 'blue', pch = 19)

  n_values = seq(from = N, to = max(nd, nc) + 10, by = 0.1)
  probabilities = 1 - pnorm(N, mean = n_values * p, sd = sqrt(n_values * p * (1 - p)))
  plot(n_values, probabilities, type = 'l', xlab = 'n', ylab = 'Objective',
       main = 'Objective Vs n to find optimal tickets sold (Continuous)')
  abline(h = gamma, col = 'red')
  points(results$nc, gamma, col = 'blue', pch = 19)

  return(results)
}
