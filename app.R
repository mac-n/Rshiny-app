library(shiny)
library(DT)
library(FSelector)
library(caret)
library(pROC)
library(randomForest)
library(shinyWidgets)
shinyApp(
  ui = fluidPage(
    #from https://stackoverflow.com/questions/35251788/hide-values-of-sliderinput-in-shiny
    tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
            visibility: hidden !important;
    }'))),
    sidebarLayout(
      sidebarPanel(
        #numericInput("lamda", label = "lamda - cost weighting", value = 0.01),
        sliderInput(
          
          inputId = "lamda",
          
          label="Cost Weighting- set to zero for cost-insensitive feature selection",
          min=0,
          max=0.01,
          value=0,
          step=0.001,
          ticks=FALSE
        ),
        
        
          
        
          checkboxInput(
            inputId = "switch",
            label = "Feature Selection",
            value=TRUE
            
          ),
        
        
        
        
        
        
        actionButton("go", "Go"),
        htmlOutput("x2")
            ),
      mainPanel(
        selectInput(
          inputId = "choose",
          label = "Update selected:",
          choices = c("Feature selection default","All MMSE", "All ADAS", "All FAQ","Deselect All"),
          multiple = FALSE,
          selected="Feature selection default"
        ),
        multiInput(
          inputId = "id", label = "Assessment items :",
          choices =  rownames(allcosts)[-2]
,
          selected = rownames(exportcosts), width = "500px",
          options = list(
            enable_search = FALSE,
            non_selected_header = "Available items:",
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
      temp<-data.frame(input$id)
      colnames(temp)<-"FLDNAME"
      df <- allq[allq$FLDNAME %in% rownames(exportcosts),]
      colnames(df)<-c("Item","Assessment Time (seconds)","Assessment","Item Description")
      switch<-input$switch
      #df$Date = Sys.time() + seq_len(nrow(df))
      x$df <- df
      x$switch <-switch
    })
    
    output$x1 = renderDT(x$df,
       options = list( pageLength = 10, info = FALSE, lengthMenu = list(c(15, -1), c("10", "All"))),editable=list(target = "cell", disable = list(columns = c(0:0)))
    )
    
    proxy = dataTableProxy('x1')
    
    output$x2<-eventReactive(input$go, {
      #list1<-cfs(CDRSB~.,joinedcosts)
      print(x$switch)
      #list1<-c(input$lamda,x$df[,1])
      times<-x$df[,2]
      
      names(times)<-x$df[,1]
      tempdf<-joined[,c("CDRSB",names(times))]
      set.seed(100)
      y<-createDataPartition(tempdf$CDRSB,p=0.25,list=FALSE)
      fsdf<-tempdf[y,]
      if(x$switch){
      vec1<-cost_cfs(CDRSB ~.,lamda=input$lamda,costs=times,tempdf)}
      else{vec1<-names(times)}
      
      tempdf<-tempdf[-y,c("CDRSB",vec1)]
      yy<-createDataPartition(tempdf$CDRSB,p=0.75,list=FALSE)
      traindf<-tempdf[yy,]
      testdf<-tempdf[-yy,]
      
      
      #text<-paste(list1,collapse="<br>")
      
      model<-randomForest(CDRSB~.,traindf)
      vec2<-rownames(model$importance)[order(-model$importance)]
      predicted<-predict(model,testdf)
      auc<-multiclass.roc(as.ordered(testdf$CDRSB),as.ordered(predicted))
      val=round(auc$auc,3)
      text=paste("<table>")
      for (i in 1:length(vec2)){
      text=paste(text,"<tr><td>",vec1[i],"</td><td>",round(model$importance[rownames(model$importance)==vec2[i]],2),"</td></tr>")
      
      }
    text=paste(text,"</table><br>")
    text=paste("<b>Feature Importance</b><br>",text) 
    if (x$switch){text=paste("<b>Diagnosis Time: </b>",sum(times[vec1])," seconds <br>",text,sep="")}
    paste("<b>Multiclass AUC: </b>",val,"<br>",text,sep="")
    
    })
   
   observeEvent(input$switch,{
     print(input$switch)
     x$switch<-input$switch
     
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
      #print(input$choose)
      if(identical(input$choose,"Feature selection default")){
        #print("identical")
        temp = unique(c(rownames(exportcosts),input$id))
        
      }else if(identical(input$choose,"All MMSE")){
        #print("identical")
        temp=unique(c(rownames(allcosts)[grep("^MM",rownames(allcosts))],input$id)) 
      }else if(identical(input$choose,"All ADAS")){
        temp=unique(c(rownames(allcosts)[grep("SCORE$",rownames(allcosts))],input$id)) 
      }else if(identical(input$choose,"All FAQ")){
        temp=unique(c(rownames(allcosts)[grep("^FAQ",rownames(allcosts))],input$id)) 
      }else if(identical(input$choose,"Deselect All")){
        temp=""
      }
        
      
      updateMultiInput(
        session = session,
        inputId = "id",
        selected=temp
      )
    })
    output$print <- renderPrint({
      x$df
    })
  }
)