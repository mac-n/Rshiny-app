library(shiny)
library(DT)
library(FSelector)
library(caret)
library(pROC)
library(randomForest)
shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        numericInput("lamda", label = "lamda - cost weighting", value = 0.01),
        actionButton("go", "Go"),
        htmlOutput("x2")
            ),
      mainPanel(
        selectInput(
          inputId = "choose",
          label = "Update selected:",
          choices = c("MMDATE","Feature selection default","All MMSE", "All ADAS", "All FAQ","Deselect All"),
          multiple = FALSE,
          selected="Feature selection default"
        ),
        multiInput(
          inputId = "id", label = "Subassessments :",
          choices =  rownames(exportcosts),
          selected = rownames(exportcosts), width = "400px",
          options = list(
            enable_search = FALSE,
            non_selected_header = "Choose between:",
            selected_header = "You have selected:"
          )),
        DTOutput('x1')))),
#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
  server = function(input, output, session) {
    x = reactiveValues(df = NULL)
    
    observe({
      
      df <- data.frame(input$id,exportcosts[input$id,])
      
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
      times<-x$df[,2]
      
      names(times)<-x$df[,1]
      tempdf<-joinedcosts[,c("CDRSB",names(times))]
      set.seed(100)
      y<-createDataPartition(tempdf$CDRSB,p=0.25,list=FALSE)
      fsdf<-tempdf[y,]
      tempdf<-tempdf[-y,]
      yy<-createDataPartition(tempdf$CDRSB,p=0.75,list=FALSE)
      traindf<-tempdf[yy,]
      testdf<-tempdf[-yy,]
      list1<-cost_cfs(CDRSB ~.,lamda=input$lamda,costs=times,tempdf)
      
      #text<-paste(list1,collapse="<br>")
      
      model<-randomForest(CDRSB~.,traindf)
      predicted<-predict(model,testdf)
      auc<-multiclass.roc(as.ordered(testdf$CDRSB),as.ordered(predicted))
      val=round(auc$auc,3)
      text=paste("<table>")
      for (i in 1:length(list1)){
      text=paste(text,"<tr><td>",list1[i],"</td><td>",round(model$importance[rownames(model$importance)==list1[i]],2),"</td></tr>")
      
      }
    text=paste(text,"</table><br>")
    paste("<b>Selected Features</b> <br>",text, "<br> <b>Diagnosis Time: </b>",sum(times[list1])," seconds <br> <b>Multiclass AUC: </b>",val,sep="")
    
    })
    
   
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      
      
      x$df[i, j] <- isolate(DT::coerceValue(v, x$df[i, j]))
    })
    observeEvent(input$choose, {
      updateMultiInput(
        session = session,
        inputId = "id",
        selected = input$choose
      )
    })
    output$print <- renderPrint({
      x$df
    })
  }
)