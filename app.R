library(shiny)
library(DT)
library(FSelector)

shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        numericInput("lamda", label = "lamda - cost weighting", value = 0.01),
        actionButton("go", "Go"),
        htmlOutput("x2")
            ),
      mainPanel(
        DTOutput('x1')))),
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
    
    output$x2<-eventReactive(input$go, {
      #list1<-cfs(CDRSB~.,joinedcosts)
      
      #list1<-c(input$lamda,x$df[,1])
      times<-x$df[,1]
      names(times)<-rownames(x$df)
      list1<-cost_cfs(CDRSB ~.,lamda=input$lamda,costs=times,joinedcosts[,c("CDRSB",rownames(exportcosts))])
      text<-paste(list1,collapse="<br>")
      val=80
      paste("<b>Selected Features</b> <br>",text, "<br> <b>Diagnosis Time: </b>",sum(times[list1])," seconds <br> <b>Diagnostic Accuracy: </b>",val,"%",sep="")
      
    })
    
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