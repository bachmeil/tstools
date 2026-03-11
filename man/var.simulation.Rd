\name{var.simulation}
\alias{var.simulation}
\title{Simulate a VAR using an augmented companion-form transition matrix}
\description{
[This is AI-generated documentation.] Simulate forward from a companion-form state transition matrix, optionally including an augmented intercept state. Shocks can be generated from a multivariate normal distribution or by bootstrapping supplied residuals. 
}
\usage{
var.simulation(fmat, start, Sigma, h, K,
                             seed = NULL,
                             shocks = c("normal", "bootstrap"),
                             residmat = NULL,
                             cumulate = c("rows", "cols", "none"),
                             intercept = c("use", "ignore"))
}
\arguments{
  \item{fmat}{Numeric \eqn{nstate \times nstate}{nstate x nstate} state transition
  matrix (companion form).}

  \item{start}{Numeric vector of length \code{nstate}. Initial state vector.}

  \item{Sigma}{Numeric \eqn{K \times K}{K x K} covariance matrix for the innovations
  when \code{shocks = "normal"}.}

  \item{h}{Integer simulation horizon (number of steps).}

  \item{K}{Integer number of observed/endogenous variables. Innovations are injected
  into the first \code{K} elements of the state vector.}

  \item{seed}{Optional integer seed for reproducible randomness (passed to
  \code{set.seed()}).}

  \item{shocks}{Character string specifying the shock generation method:
  \describe{
    \item{\code{"normal"}}{Draw i.i.d. Gaussian innovations with covariance \code{Sigma}.}
    \item{\code{"bootstrap"}}{Sample rows from \code{residmat} with replacement.}
  }}

  \item{residmat}{Optional numeric matrix of residuals/innovations with dimension
  \eqn{T \times K}{T x K}. Required when \code{shocks = "bootstrap"}.}

  \item{cumulate}{Character string controlling what is returned:
  \describe{
    \item{\code{"none"}}{Return only the final \eqn{K}{K}-vector \eqn{y_h}{y_h}.}
    \item{\code{"rows"}}{Return a \eqn{(h+1) \times K}{(h+1) x K} matrix with time in rows.}
    \item{\code{"cols"}}{Return a \eqn{K \times (h+1)}{K x (h+1)} matrix with time in columns.}
  }}

  \item{intercept}{Character string controlling the augmented intercept state:
  \describe{
    \item{\code{"use"}}{Force the last state element to equal 1 at each step.}
    \item{\code{"ignore"}}{Force the last state element to equal 0 at each step
    (useful for impulse responses / deviations from intercept).}
  }}
}
\details{
Let \eqn{x_t}{x_t} be the companion state vector of length \code{nstate}.
At each step the function draws (or bootstraps) a \eqn{K}{K}-vector of innovations
\eqn{u_t}{u_t} and injects it into the first \eqn{K}{K} state elements, so the update is
\deqn{x_{t+1} = F x_t + e_{t+1},}{x[t+1] = F x[t] + e[t+1],}
where \eqn{e_{t+1}}{e[t+1]} is length \code{nstate} with \eqn{u_{t+1}}{u[t+1]} in positions
\code{1:K} and zeros elsewhere.

If the state is augmented with a constant intercept element as the last component,
that element is pinned to 1 (\code{intercept = "use"}) or to 0 (\code{intercept = "ignore"})
after each update.
}
\value{
If \code{cumulate = "none"}, a numeric vector of length \code{K} containing the first
\code{K} elements of the final state (the final \eqn{y}{y}).

If \code{cumulate = "rows"}, a numeric matrix with dimension \eqn{(h+1) \times K}{(h+1) x K}
containing \eqn{y_0, y_1, \ldots, y_h}{y0, y1, ..., yh} in rows.

If \code{cumulate = "cols"}, a numeric matrix with dimension \eqn{K \times (h+1)}{K x (h+1)}
containing \eqn{y_0, y_1, \ldots, y_h}{y0, y1, ..., yh} in columns.
}
\examples{
# --- Example 1: Simple AR(1) in 1D (K = 1), no intercept augmentation ---
K <- 1
phi <- 0.9
fmat <- matrix(phi, 1, 1)
start <- 0
Sigma <- matrix(1, 1, 1)

# Full simulated path (time in rows)
y_path <- var.simulation(
  fmat = fmat, start = start, Sigma = Sigma, h = 12, K = K,
  seed = 123, shocks = "normal", cumulate = "rows", intercept = "ignore"
)
y_path

# Final value only
y_final <- var.simulation(
  fmat = fmat, start = start, Sigma = Sigma, h = 12, K = K,
  seed = 123, shocks = "normal", cumulate = "none", intercept = "ignore")
y_final

# --- Example 2: Bivariate VAR(1) (K = 2) in companion form ---
K <- 2

# VAR(1): y_t = A1 y_{t-1} + u_t
A1 <- matrix(c( 0.60, 0.10,
                0.05, 0.70), nrow = K, byrow = TRUE)

# Companion state is just y_t when p = 1 (no extra lags), and no intercept augmentation here:
fmat <- A1

# Initial state y_0 (length K == nstate)
start <- c(0, 0)

# Innovation covariance
Sigma <- matrix(c(1.0, 0.3,
                  0.3, 2.0), nrow = K, byrow = TRUE)

# Simulate 20 steps, return (h+1) x K with y_0 included
y_path2 <- var.simulation(
  fmat = fmat, start = start, Sigma = Sigma, h = 20, K = K,
  seed = 42, shocks = "normal", cumulate = "rows", intercept = "ignore")

# y_path2[t + 1, ] is y_t, for t = 0..h
head(y_path2)
}
\seealso{
\code{\link[base]{chol}}, \code{\link[stats]{rnorm}}, \code{\link[base]{sample.int}}
}
\keyword{simulation}
\keyword{ts}
