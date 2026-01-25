libs <-  c(
  "dplyr",
  "ggplot2",
  "survival",
  "randomForest",
  "shiny",
  "DT",
  "tidyr",
  "stringr",
  "bslib"
)

for (pkg in libs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
# -------- UI --------
ui <- page_fluid(
  #theme = bs_theme(version = 5, bootswatch = "flatly"),
  # tags$head(
  #   tags$style(HTML("
  #     /* Compact DT tables */
  #     table.dataTable th, table.dataTable td { padding: 4px 8px; }
  #     /* Left nav width */
  #     .navlist-panel .well { padding: 0; border: none; background: transparent; }
  #   "))
  # ),
  titlePanel("Clinical Review Dashboard"),
  navlistPanel(
    widths = c(3, 9),   # left tabs width, right content width
    # --- Demography ---
    tabPanel("Demography",
             fluidRow(
               column(12,
                      fluidRow(
                        column(3, selectInput("sex", "Sex", choices = c("All", 'M','F'), selected = "ALL")),
                      ),
                      DTOutput("tbl_demog")
               )
             )
    ),
    # --- Adverse Events ---
    tabPanel("Adverse",
             fluidRow(
               column(12,
                      fluidRow(
                        column(3, selectInput("ae_ser", "Serious (AESER)", choices = c("All", "Y","N"), selected = "All")),
                        column(3, selectInput("ae_sev", "Severity (AESEV)", choices = c("All", "Mild","Moderate","Severe"), selected = "All")),
                        column(3, selectInput("ae_arm", "Arm", choices = c("All", sort(unique(adsl$arm))), selected = "All"))
                      ),
                      DTOutput("tbl_adae")
               )
             )
    ),
    # --- Labs ---
    tabPanel("Lab",
             fluidRow(
               column(12,
                      fluidRow(
                        column(3, selectInput("lab_param", "Parameter", choices = sort(unique(adlbc$param)), selected = "ALT")),
                        column(3, selectInput("lab_visit", "Visit", choices = sort(unique(adlbc$VISIT)), selected = "Visit 1")),
                        column(3, selectInput("lab_arm", "Arm", choices = c("All", sort(unique(adsl$arm))), selected = "All"))
                      ),
                      DTOutput("tbl_adlb")
               )
             )
    ),
    # --- Other Analysis (placeholder) ---
    tabPanel("Other Analysis",
             fluidRow(
               column(12,
                      div(style = "width: 100%",
                      tags$p("Put additional analyses here (e.g., exposure, efficacy summaries, listings, figures)."),
                      dataTableOutput("tbl_other")
               )
               )
             )
    )
  )
)

# ---- Server ----
### Demography table
create_demo_table <- function(data, treatment, vars) {
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
          `Mean (SD)` = paste0(round(mean(!!var_sym, na.rm = TRUE), 2),' (', round(sd(!!var_sym, na.rm = TRUE), 3),')'),
          Median = round(median(!!var_sym, na.rm = TRUE), 2),
          Min = round(min(!!var_sym, na.rm = TRUE), 2),
          Max = round(max(!!var_sym, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>% mutate(across(everything(), as.character)) %>%
        pivot_longer(cols = -!!treatment_sym, names_to = "Stat", values_to = "Value") %>%
        mutate(
          Variable = var,
          Value = as.character(Value)
        ) %>%
        select(Variable, Stat, !!treatment_sym, Value) %>%  pivot_wider(
          names_from = !!treatment_sym,
          values_from = Value
        )
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
        select(Variable, Stat, !!treatment_sym, Value) %>% 
        pivot_wider(
          names_from = !!treatment_sym,
          values_from = Value
        ) 
        
    }
  })

  # Combine and pivot to wide format
  final <- bind_rows(results) %>%  select(c( 'Variable' ,'Stat', 'Placebo',  `Xanomeline High Dose`, `Xanomeline Low Dose`, 'Total') )%>% as.data.frame()
  
  return(final)
}

### Adverse events table
create_ae_table <- function(data, adsl_data = adsl, trt_var_adsl= 'trt01a', 
                            soc_var = 'aebodsys', pt_var = "aedecod", trt_var = 'trta') {
  
  # Calculate total Subject per treatment from ADSL
  trt_var_adsl <- sym(trt_var_adsl)
  adsl_data <- mutate(adsl_data, trt_adsl = as.character(!!trt_var_adsl))
  adsl_data <- adsl_data %>% filter(saffl == "Y")
  
  bign <- bind_rows(filter(adsl_data, saffl == "Y"),
                    filter(mutate(adsl_data, trt_adsl="Total"), saffl == "Y")) %>%
    distinct(usubjid, trt_adsl) %>%
    dplyr::count(trt_adsl, name = "N") %>% 
    dplyr::rename(trta = trt_adsl)
  
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

# Labs table
create_lb_table <- function(data, trt_var = 'trta', adsl_data = adsl, trt_var_adsl= 'trt01a',  
                            par_var = 'param', ind_var = "lbnrind") {
  
  # Calculate total Subject per treatment from ADSL
  trt_var_adsl <- sym(trt_var_adsl)
  adsl_data <- mutate(adsl_data, trt_adsl = as.character(!!trt_var_adsl))
  adsl_data <- adsl_data %>% filter(saffl == "Y")
  bign <- bind_rows(filter(adsl_data, saffl == "Y"),
                    filter(mutate(adsl_data, trt_adsl="Total"), saffl == "Y")) %>%
    distinct(usubjid, trt_adsl) %>%
    count(trt_adsl, name = "N") %>% 
    rename(trta = trt_adsl)
  
  # Convert all vars to character if needed
  data <- data %>%
    mutate(
      !!sym(trt_var) := as.character(!!sym(trt_var)),
      !!sym(par_var) := as.character(!!sym(par_var)),
      !!sym(ind_var) := as.character(!!sym(ind_var))
    )
  
  # Create a total in ADLB
  adlb_saffl <- filter(data, saffl == "Y")
  adlb_total <- adlb_saffl %>% mutate(!!sym(trt_var) := "Total")
  adlb1 <- bind_rows(adlb_saffl, adlb_total)%>%
    mutate(lbnrindn = recode(!!sym(ind_var),
                             "LOW" = 1,
                             "NORMAL" = 2,
                             "HIGH" = 3,
                             .default = NA_real_))
  
  # Calculate the freq for "Any shift" of each PARAM
  #adlb_any <- adlb1 %>% 
  #group_by(!!sym(trt_var), !!sym(par_var)) %>% 
  #summarise(n = n_distinct(usubjid), .groups = "drop") %>% 
  #mutate(ord = 0, ord_soc = 0, !!sym(par_var) := "Any Shift")
  
  # Calculate the freq by PARAM
  adlb_param <- adlb1 %>% 
    group_by(!!sym(trt_var), !!sym(par_var)) %>% 
    summarise(n = n_distinct(usubjid), .groups = "drop") 
  
  adlb_par_ord <- adlb_param %>% 
    filter(!!sym(trt_var) == 'Total') %>% 
    arrange(desc(n), !!sym(trt_var), !!sym(par_var)) %>% 
    mutate(ord_par = row_number())
  
  adlb_param1 <- left_join(adlb_param, adlb_par_ord%>% 
                             select(!!sym(par_var), ord_par),
                           by = c(par_var)) %>% 
    mutate(ord = 1,ord_gra = 0)
  
  # Calculate the freq by grade of each PARAM
  df_first <- adlb1 %>% 
    arrange(usubjid, !!sym(trt_var), !!sym(par_var),!!sym(ind_var), lbnrindn) %>% 
    group_by(usubjid, !!sym(trt_var), !!sym(par_var)) %>% 
    slice_head(n =1) %>% 
    ungroup
  
  adlb_grade <- df_first %>% 
    group_by(!!sym(trt_var), !!sym(par_var),!!sym(ind_var), lbnrindn) %>% 
    summarise(n = n_distinct(usubjid), .groups = "drop")
  
  adlb_gra_ord <- adlb_grade %>% 
    filter(!!sym(trt_var) == 'Total') %>% 
    left_join(adlb_par_ord %>% select(!!sym(par_var), ord_par), by = par_var) %>% 
    arrange(ord_par, !!sym(ind_var)) %>% 
    mutate(ord_gra = row_number())
  
  adlb_grade1 <- adlb_grade %>% 
    left_join(adlb_gra_ord %>% select(!!sym(par_var), !!sym(ind_var), ord_par, ord_gra),
              by = c(par_var, ind_var)) %>% 
    mutate(ord = 2) %>% 
    arrange(ord_par, ord_gra, lbnrindn)
  
  # Combine 
  final_df <- bind_rows(adlb_param1, adlb_grade1)%>%
    left_join(bign, by = c(trt_var))%>% 
    mutate(percent = round(100 * n / N, 1),
           col = paste0(n, " (", percent, ")")) %>% 
    select(ord, ord_par, ord_gra, !!sym(par_var), !!sym(ind_var), lbnrindn, !!sym(trt_var), col) %>% 
    pivot_wider(
      names_from = !!sym(trt_var),
      values_from = col,
      names_prefix = "",
      values_fill = list(col = "0")) %>% 
    arrange(!!sym(par_var), ord, ord_gra, lbnrindn)
  
  final1 <- final_df %>%
    arrange(!!sym(par_var), ord_gra) %>%
    mutate(
      !!sym(par_var) := as.character(!!sym(par_var)),
      !!sym(ind_var) := as.character(!!sym(ind_var)),
      !!sym(par_var) := ifelse(!is.na(!!sym(ind_var)), !!sym(ind_var), !!sym(par_var)))
  
  final <- final1 %>% filter(!is.na(!!sym(par_var)) & !!sym(par_var) != "") %>% 
    select(-'ord_par', -'ord', -'ord_gra', -!!sym(ind_var), -'lbnrindn')
  names(final)[1] <- " "
  return(final)
}

# ---- Server ----
server <- function(input, output, session) {
  # Demography table
  output$tbl_demog <- renderDT({
    datatable(
      create_demo_table(adsl %>% filter(
        sex %in% if (input$sex == "All") c("M", "F") else input$sex
      ), 'trt01p', c('age','sex','race')),
      rownames = FALSE,
     # options = list(pageLength = 18, scrollX = TRUE, autoWidth = TRUE),
    #  filter = "top"
    options = list(pageLength = 100)
    )
  })
  # Adverse events table
  output$tbl_adae <- renderDT({
    datatable(
    create_ae_table(adae, adsl_data = adsl, trt_var_adsl= 'trt01a',
                    soc_var = 'aebodsys', pt_var = 'aedecod', trt_var = "trta"),
    options = list(pageLength = 50)
    
  )
    })
  
  # Lab
  output$tbl_adlb <- renderDT({
    create_lb_table(adlbc, trt_var = "trta", adsl_data = adsl, trt_var_adsl= 'trt01a',
                    par_var = 'param', ind_var = 'lbnrind')
  })
  # Other analysis placeholder (e.g., subject counts by arm)
  output$tbl_other <- renderDT({
    data.frame(Message = "Additional analyses will go here.")
  })
}

shinyApp(ui, server)
