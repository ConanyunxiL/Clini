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

load("~/Clini/adae.rda")
load("~/Clini/adlbc.rda")
load("~/Clini/adsl.rda")
source("R/mod_home_ui.R")
source("R/mod_home_server.R")
source("R/mod_plot_ui.R")
source("R/mod_plot_server.R")
source("R/mod_table_ui.R")
source("R/mod_table_server1.R")

ui <- fluidPage(
  titlePanel("Clinical Review Dashboard"),
  
  navlistPanel(
    id = "main_nav",
    widths = c(3, 9),
    
    tabPanel(title = "Demography",
    mod_home_ui("table_0")
),
    
    tabPanel(
      title = "Plot",
      mod_plot_ui("plot_1")
    ),
    
    tabPanel(
      title = "Table",
      mod_table_ui("table_1")
    )
  )
)

server <- function(input, output, session) {
  mod_home_server("table_0")
  mod_plot_server("plot_1")
  mod_table_server1("table_1")
}

shinyApp(ui, server)