mod_table_server1 <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$table <- renderTable({
      head(mtcars)
    })
    
  })
}