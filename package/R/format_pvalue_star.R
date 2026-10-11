format_pvalue_star <- function(p) {
  dplyr::case_when(
    is.na(p)        ~ NA_character_,
    p < 0.0001      ~ "****",
    p < 0.001       ~ "***",
    p < 0.01        ~ "**",
    p < 0.05        ~ "*",
    p < 0.08        ~ paste0(sprintf("%.2f", p)),
    TRUE            ~ "ns"
  )
}
