library(shiny)
library(DT)
library(FSelector)
shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
         numericInput("lamda", label = "lamda - cost weighting", value = 0.01),
         actionButton("go", "Go"),
         textOutput("x2")
      ),
      mainPanel(
    DTOutput('tbl')))),
  server = function(input, output) {
    output$tbl = renderDT(
      exportcosts, options = list( pageLength = 10, info = FALSE, lengthMenu = list(c(15, -1), c("10", "All"))),editable=list(target = "cell", disable = list(columns = c(0:0)))
    )
    output$x2<-eventReactive(input$go, {cfs(CDRSB~.,joinedcosts)
     
    })
  
  }
)


