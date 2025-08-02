#Generate AE Summary Table
library(dplyr)
library(tidyr)
library(stringr)
library(rlang)
# load("D:/adae.rda")
# load("D:/adsl.rda")

# Calculate big N per treatment from ADSL
adsl <- mutate(adsl, trt01a = as.character(trt01a))
bign <- bind_rows(filter(adsl, saffl == "Y"),
                  filter(mutate(adsl, trt01a="Total"), saffl == "Y")) %>%
  distinct(usubjid, trt01a) %>%
  dplyr::count(trt01a, name = "N") %>% 
  dplyr::rename(trta = trt01a)

ae_summary <- function(data, trt_var = 'trta', soc_var = 'aebodsys', pt_var = "aedecod") {
  # Convert all vars to character if needed
  data <- data %>%
    mutate(
      !!sym(trt_var) := as.character(!!sym(trt_var)),
      !!sym(soc_var) := as.character(!!sym(soc_var)),
      !!sym(pt_var) := as.character(!!sym(pt_var))
    )
  # Create a total in ADAE
  adae_saffl <- filter(data, saffl == "Y")
  adae_total <- adae_saffl %>% mutate(!!sym(trt_var) := "Total")
  adae1 <- bind_rows(adae_saffl, adae_total)
  
  # Calculate Any TEAE
  teae_any <- adae1 %>% 
    group_by(!!sym(trt_var)) %>% 
    summarise(n = n_distinct(usubjid), .groups = "drop") %>% 
    mutate(ord = 0, ord_soc = 0, !!sym(soc_var) := "Any TEAE")
  
  # Calculate the freq by SOC 
  teae_soc <- adae1 %>% 
    group_by(!!sym(trt_var), !!sym(soc_var)) %>% 
    summarise(n = n_distinct(usubjid), .groups = "drop")
  
  teae_soc_ord <- teae_soc %>% 
    filter(!!sym(trt_var) == 'Total') %>% 
    arrange(desc(n), !!sym(soc_var)) %>% 
    mutate(ord_soc = row_number())
  
  teae_soc1 <- left_join(teae_soc, 
                         select(teae_soc_ord, !!sym(soc_var), ord_soc),
                         by = c(soc_var)) %>% 
    mutate(ord = 1)
  
  # Calculate the freq by SOC and PT 
  teae_pt <- adae1 %>% 
    group_by(!!sym(trt_var), !!sym(soc_var), !!sym(pt_var)) %>% 
    summarise(n = n_distinct(usubjid), .groups = "drop") 
  
  teae_pt_ord <- teae_pt %>% 
    filter(!!sym(trt_var) == 'Total') %>% 
    left_join(select(teae_soc_ord,!!sym(soc_var), ord_soc),
              by = c(soc_var)) %>% 
    arrange(ord_soc, desc(n), !!sym(pt_var)) %>% 
    mutate(ord_pt = row_number())
  
  teae_pt1 <- left_join(teae_pt,
                        select(teae_pt_ord, !!sym(soc_var), !!sym(pt_var), ord_soc, ord_pt),
                        by = c(soc_var, pt_var)) %>% 
    mutate(ord = 2) %>% 
    arrange(ord_soc, ord_pt)
  
  # Combine SOC and PT
  final <- bind_rows(teae_any, teae_soc1, teae_pt1) %>% 
    left_join(bign, by = trt_var) %>%  
    
    mutate(percent = round(100*n/N, 1),
           col = str_c(as.character(n),
                 " (", as.character(percent), ")"),
           SOC_PT = case_when(is.na(!!sym(pt_var)) ~ as.character(!!sym(soc_var)),
                              TRUE ~ str_c("  ", as.character(!!sym(pt_var))))) %>% 
    
    pivot_wider(id_cols = c(SOC_PT, ord, ord_soc, ord_pt),
                names_from = !!sym(trt_var),
                values_from = col,
                values_fill = "0",
                names_prefix = "") %>% 
    arrange(ord_soc, ord, ord_pt) %>% 
    select(-'ord_soc', -'ord', -'ord_pt')
    names(final)[1] <- "System Organ Class/\nPreferred Term"
  return(final)
}

ae_table <- ae_summary(data = adae, trt_var = "trta", soc_var = 'aebodsys', pt_var = 'aedecod')
