library(dplyr)
library(tidyr)
library(stringr)
library(rlang)

make_ae_table <- function(data, trt_var, soc_var, pt_var, subject_var) {
  trt_var <- enquo(trt_var)
  soc_var <- enquo(soc_var)
  pt_var  <- enquo(pt_var)
  subject_var <- enquo(subject_var)
  
  # Convert all vars to character if needed
  data <- data %>%
    mutate(
      !!as_name(soc_var) := as.character(!!soc_var),
      !!as_name(pt_var) := as.character(!!pt_var),
      !!as_name(trt_var) := as.character(!!trt_var),
      !!as_name(subject_var) := as.character(!!subject_var)
    )
  
  # Get N per treatment
  trt_n <- data %>%
    distinct(!!subject_var, !!trt_var) %>%
    count(!!trt_var, name = "N")
  
  # AE counts by SOC/PT
  ae_counts <- data %>%
    distinct(!!subject_var, !!trt_var, !!soc_var, !!pt_var) %>%
    count(!!soc_var, !!pt_var, !!trt_var, name = "n") %>%
    left_join(trt_n, by = rlang::as_name(trt_var)) %>%
    mutate(pct = sprintf("%d (%.1f%%)", n, 100 * n / N)) %>%
    select(!!soc_var, !!pt_var, !!trt_var, pct)
  
  # Wide format
  ae_wide <- ae_counts %>%
    pivot_wider(names_from = !!trt_var, values_from = pct, values_fill = "0 (0.0%)")
  
  # Total column
  trt_cols <- setdiff(names(ae_wide), c(as_name(soc_var), as_name(pt_var)))
  ae_wide <- ae_wide %>%
    mutate(
      Total = apply(across(all_of(trt_cols)), 1, function(row) {
        counts <- as.numeric(str_extract(row, "^\\d+"))
        sprintf("%d", sum(counts, na.rm = TRUE))
      })
    )
  
  # SOC-only rows
  soc_rows <- ae_wide %>%
    group_by(!!soc_var) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      label = !!soc_var,
      !!as_name(pt_var) := !!soc_var
    )
  
  # Indented PT rows
  pt_rows <- ae_wide %>%
    mutate(label = paste0("  ", !!pt_var))
  
  # Combine and order
  final <- bind_rows(soc_rows, pt_rows) %>%
    arrange(!!soc_var, desc(label != paste0("  ", !!pt_var))) %>%
    select(label, all_of(trt_cols), Total) %>% as.data.frame()
  
  return(final)
}


data_ae <- make_ae_table(adae, trta, aebodsys, aedecod,subject_var = usubjid)

