mod_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$plot <- renderPlot({
      hist(mtcars[[input$var]], main = input$var, xlab = input$var)
    })
    
  })
}