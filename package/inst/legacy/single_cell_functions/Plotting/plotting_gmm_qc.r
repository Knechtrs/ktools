plotting_gmm_qc <- function(score, gmm_model) {
  # Empirical density
  total_density <- density(score)
  df_total <- data.frame(x = total_density$x, y = total_density$y)

  # GMM component curves
  means <- gmm_model$parameters$mean
  sds <- sqrt(gmm_model$parameters$variance$sigmasq)
  props <- gmm_model$parameters$pro
  x_vals <- seq(min(score), max(score), length.out = 1000)

    sds <- sqrt(gmm_model$parameters$variance$sigmasq)

df_components <- do.call(rbind, lapply(1:length(means), function(k) {
  data.frame(
    x = x_vals,
    y = props[k] * dnorm(x_vals, mean = means[k], sd = sds[k]),
    component = paste0("Population ", k) 
  )
}))

  # Cutoff: where posterior probabilities are ~0.5
  posteriors <- gmm_model$z
  cutoff_idx <- which.min(abs(posteriors[,1] - 0.5))
  cutoff <- score[cutoff_idx]

  # Plot
  ggplot() +
    geom_line(data = df_total, aes(x = x, y = y), color = "black", size = 1.2) +
    geom_line(data = df_components, aes(x = x, y = y, color = component), size = 1.1) +
    geom_vline(xintercept = cutoff, linetype = "dashed", color = "grey", size = 1) +
    # annotate("text", x = cutoff, y = Inf, label = "GMM cutoff", angle = 90, vjust = -1, color = "red") +
    labs(x = "Score", y = "Density", color = "") +
    theme_layout +
    theme_fontsize() +
    scale_y_continuous(expand= expansion(mult=c(0,0.1)), limits= c(0,NA))
}
