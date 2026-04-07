mod_plot_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Plot Module"),
    selectInput(ns("var"), "Choose variable", choices = names(mtcars)),
    plotOutput(ns("plot"))
  )
}