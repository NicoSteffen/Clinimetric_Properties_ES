# Required packages
library(dplyr)

# Function to calculate repeated-measures Cohen's d (drm) and Hedges' grm
compute_drm <- function(pre, post) {
  n <- length(pre)
  mean_diff <- mean(post - pre, na.rm = TRUE)
  sd_pre <- sd(pre, na.rm = TRUE)
  sd_post <- sd(post, na.rm = TRUE)
  r <- cor(pre, post, use = "complete.obs")
  
  # within-subject standardizer
  S_within <- sqrt((sd_pre^2 + sd_post^2 - 2 * r * sd_pre * sd_post) / (2 * (1 - r)))
  d_rm <- mean_diff / S_within
  
  # Hedges correction
  J <- 1 - (3 / (4 * (n - 1) - 1))
  g_rm <- d_rm * J
  
  return(data.frame(
    r = round(r, 3),
    d_rm = round(d_rm, 3),
    g_rm = round(g_rm, 3)
  ))
}

# Apply to each variable pair
drm_results <- list(
  BDI_total        = compute_drm(long$t0_BDI_total,        long$t1_BDI_total),
  ES_total         = compute_drm(long$t0_ES_total,         long$t1_ES_total),
  ES_likert_total  = compute_drm(long$t0_ES_likert_total,  long$t1_ES_likert_total),
  WHO5_total       = compute_drm(long$t0_WHO5_total,       long$t1_WHO5_total)
)

# Combine into one data frame
drm_table <- bind_rows(drm_results, .id = "Variable")
print(drm_table)