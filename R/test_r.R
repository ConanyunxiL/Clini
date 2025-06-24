#' Hello World
#'
#' This is just a test function.
#'
#' @return A string
#' @export
create_demo_table <- function(data, treatment, vars) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(rlang)

  treatment_sym <- sym(treatment)

  # Ensure treatment column is character to avoid type mismatch errors
  data <- data %>%
    mutate(across(all_of(treatment), as.character))

  # Create a total group
  data_total <- data %>%
    mutate(!!treatment_sym := "Total")

  # Combine original and total data
  data_all <- bind_rows(data, data_total)

  results <- lapply(vars, function(var) {
    var_sym <- sym(var)

    if (is.numeric(data[[var]])) {
      stats <- data_all %>%
        group_by(!!treatment_sym) %>%
        summarise(
          N = sum(!is.na(!!var_sym)),
          Mean = round(mean(!!var_sym, na.rm = TRUE), 2),
          Median = round(median(!!var_sym, na.rm = TRUE), 2),
          SD = round(sd(!!var_sym, na.rm = TRUE), 2),
          Min = round(min(!!var_sym, na.rm = TRUE), 2),
          Max = round(max(!!var_sym, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = -!!treatment_sym, names_to = "Stat", values_to = "Value") %>%
        mutate(
          Variable = var,
          Value = as.character(Value)
        ) %>%
        select(Variable, Stat, !!treatment_sym, Value)

    } else {
      stats <- data_all %>%
        group_by(!!treatment_sym, !!var_sym) %>%
        summarise(N = n(), .groups = "drop") %>%
        group_by(!!treatment_sym) %>%
        mutate(
          Pct = round(100 * N / sum(N), 1),
          Value = paste0(N, " (", Pct, "%)")
        ) %>%
        mutate(
          Variable = var,
          Stat = as.character(!!var_sym)
        ) %>%
        select(Variable, Stat, !!treatment_sym, Value)
    }
  })

  # Combine and pivot to wide format
  final <- bind_rows(results) %>%
    pivot_wider(
      names_from = !!treatment_sym,
      values_from = Value
    ) %>%
    arrange(Variable, Stat) %>% as.data.frame()

  return(final)
}
