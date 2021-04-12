library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
    DTOutput('tbl')))),
  server = function(input, output) {
    output$tbl = renderDT(
      exportcosts, options = list( pageLength = 10, info = FALSE, lengthMenu = list(c(15, -1), c("10", "All"))),editable=list(target = "cell", disable = list(columns = c(0:0)))
    )
    
  
  }
)


