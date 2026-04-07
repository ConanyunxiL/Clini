mod_home_ui <- function(id) {

  ns <- NS(id)
  
  tagList(
    tabsetPanel(
      tabPanel( "Demo",
                selectInput(ns("sex"), "Sex", choices = c("All", 'M','F'), selected = "ALL"),
                DTOutput(ns("tbl_demog"))
      ),
      tabPanel( "Adverse events",
                fluidRow(
                  column(
                    width = 6,
                    selectInput(ns("ae_rel"), "Causality", choices = c( "All", "NONE", "POSSIBLE", "PROBABLE","REMOTE"),
                                selected = "All"),
                  ),
                  column(
                    width = 6,
                    selectInput(ns("ae_sev"), "Severity",
                                choices = c("All", "Mild"='MILD',"Moderate"='MODERATE',"Severe"='SEVERE'), selected = "All"),
                  ),
                  DTOutput(ns("tbl_adae"))
                )
      ),
      tabPanel( "Laboratory",
                fluidRow(
                  column(
                    width = 12,
                    selectInput(ns("lab_param"), "Parameter", choices = sort(unique(adlbc$param)), selected = unique(adlbc$param)[1:5],multiple = T),
                  ),
                  DTOutput(ns("tbl_adlb"))
                )

      )
      
    )
  )
}
