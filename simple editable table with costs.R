library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(
    DTOutput('x1'),
    #verbatimTextOutput("print")
  ),
  server = function(input, output, session) {
    x = reactiveValues(df = NULL)
    
    observe({
      df <- exportcosts
      #df$Date = Sys.time() + seq_len(nrow(df))
      x$df <- df
    })
    
    output$x1 = renderDT(x$df,
       options = list( pageLength = 10, info = FALSE, lengthMenu = list(c(15, -1), c("10", "All"))),editable=list(target = "cell", disable = list(columns = c(0:0)))
    )
    
    proxy = dataTableProxy('x1')
    
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # problem starts here
      x$df[i, j] <- isolate(DT::coerceValue(v, x$df[i, j]))
    })
    
    output$print <- renderPrint({
      x$df
    })
  }
)