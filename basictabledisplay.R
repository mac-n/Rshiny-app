library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(DTOutput('tbl')),
  server = function(input, output) {
    output$tbl = renderDT(
      exportcosts, options = list(lengthChange = FALSE),editable=TRUE
    )
  }
)