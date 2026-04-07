mod_table_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Table Module"),
    tableOutput(ns("table"))
  )
}