# Define Maxwell models
maxwell_model_one <- function(t, tau, A) {
  1 - A * (1 - exp(-t / tau))
}

maxwell_model_two <- function(t, tau1, tau2, A1, A2) {
  1 - A1 * (1 - exp(-t / tau1)) - A2 * (1 - exp(-t / tau2))
}
