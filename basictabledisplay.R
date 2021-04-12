library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(DTOutput('tbl')),
  server = function(input, output) {
    output$tbl = renderDT(
      exportcosts, options = list(lengthChange = FALSE),editable=list(target = "cell", disable = list(columns = c(0:0)))
    )
  }
)


