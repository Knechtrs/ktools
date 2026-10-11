format_pvalue <- function(p) {
  dplyr::case_when(
    is.na(p)        ~ NA_character_,
    p < 0.0001      ~ "p < 0.0001",
    p < 0.001       ~ "p < 0.001",
    p < 0.01        ~ "p < 0.01",
    p < 0.05        ~ "p < 0.05",
    TRUE            ~ paste("p =", round(p, 3))
  )
}