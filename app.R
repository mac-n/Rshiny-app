library(shiny)
library(DT)
library(FSelector)
library(caret)
library(pROC)
library(randomForest)
library(shinyWidgets)

#The data is not included in this Github repository as it is property of ADNI (adni.loni.usc.edu)
#this app runs with a data frame of individual CFA values and a vector of costs. 
shinyApp(
  ui = fluidPage(
    #from https://stackoverflow.com/questions/35251788/hide-values-of-sliderinput-in-shiny
    tags$head(tags$style(HTML(' .irs-to, .irs-min, .irs-max, .irs-single {
            visibility: hidden !important;
    }'))),
    sidebarLayout(
      sidebarPanel(
        #numericInput("lamda", label = "lamda - cost weighting", value = 0.01),
        sliderInput(
          
          inputId = "lamda",
          
          label="Cost Weighting- slide to left for cost-insensitive feature selection",
          #https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
          
          min=0,
          max=0.02,
          value=0,
          step=0.001,
          ticks=FALSE
          #width='250px'
        ),
        
        
          #checkbox to turn feature selection on and off
        
          checkboxInput(
            inputId = "switch",
            label = "Perform Feature Selection on Chosen Assessment Items",
            value=TRUE
            
          ),
        
        
        
        #Button to make it all happen
        
        
        actionButton("go", "Run Tests"),

        #this is where the AUC and feature importance is output after the tests are run. 
        htmlOutput("x2")
            ),

      mainPanel(
      	#dropdown menu to choose assessment items by group
        selectInput(
          inputId = "choose",
          label = "Add to your choices:",
          choices = c("Most informative items","All MMSE", "All ADAS", "All FAQ","All MoCA","Deselect All"),
          multiple = FALSE,
          selected="Most informative items"
        ),
        #multiInput to add assessment items individually.
        multiInput(
          inputId = "id", label = "Assessment items :",
          choices =  rownames(allcosts)[-2]
,
          selected = rownames(exportcosts), width = "500px",
          options = list(
            enable_search = FALSE,
            non_selected_header = "Available items:",
            selected_header = "You have chosen:"
          )),
        #show a table of assessment items
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
  	#initialise the reactiveValues - primarily the boolean feature selection option and the dataframe of items to use for testing
    x = reactiveValues(df = NULL)
    
    observe({
     # use only selected fields from the dataframe
      df <- allq[allq$FLDNAME %in% input$id,]
      colnames(df)<-c("Item","Assessment Time (seconds)","Assessment","Item Description")
      switch<-input$switch
      #df$Date = Sys.time() + seq_len(nrow(df))
      x$df <- df
      x$switch <-switch
    })
    #renderDT makes editable data tables
    output$x1 = renderDT(x$df,
       options = list( pageLength = 10, info = FALSE, lengthMenu = list(c(15, -1), c("10", "All"))),editable=list(target = "cell", disable = list(columns = c(0:0)))
    )
    
    proxy = dataTableProxy('x1')
    #the bulk of the calculation is done here when you press the buttom
    output$x2<-eventReactive(input$go, {
      #list1<-cfs(CDRSB~.,joinedcosts)
      #print(x$switch)
      #list1<-c(input$lamda,x$df[,1])
      times<-x$df[,2]
      
      names(times)<-x$df[,1]
      #use only selected items
      tempdf<-joined[,c("CDRSB",names(times))]
      set.seed(100)
      #create data frame for feature selection
      y<-createDataPartition(tempdf$CDRSB,p=0.25,list=FALSE)
      fsdf<-tempdf[y,]
      if(x$switch){
      vec1<-cost_cfs(CDRSB ~.,lamda=input$lamda,costs=times,tempdf)}
      else{vec1<-names(times)}
      #remove feature selection data 
      tempdf<-tempdf[-y,c("CDRSB",vec1)]
      #split remainder into training and test data
      yy<-createDataPartition(tempdf$CDRSB,p=0.75,list=FALSE)
      traindf<-tempdf[yy,]
      testdf<-tempdf[-yy,]
      
      
      #text<-paste(list1,collapse="<br>")
      #build model, get multiclass AUC
      model<-randomForest(CDRSB~.,traindf)
      vec2<-rownames(model$importance)[order(-model$importance)]
      predicted<-predict(model,testdf)
      auc<-multiclass.roc(as.ordered(testdf$CDRSB),as.ordered(predicted))
      #create HTML output of feature importance, AUC, total assessment time
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
   #feature selection switch
   observeEvent(input$switch,{
     print(input$switch)
     x$switch<-input$switch
     
   })
   #Allow for times to be edited
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      
      
      x$df[i, j] <- isolate(DT::coerceValue(v, x$df[i, j]))
    })

    #the dropdown menu
    observeEvent(input$choose, {
      #print(input$choose)
      if(identical(input$choose,"Most informative items")){
        #print("identical")
        temp = unique(c(rownames(exportcosts),input$id))
        
      }else if(identical(input$choose,"All MMSE")){
        #print("identical")
        temp=unique(c(rownames(allcosts)[grep("^MM",rownames(allcosts))],input$id)) 
      }else if(identical(input$choose,"All ADAS")){
        temp=unique(c(rownames(allcosts)[grep("SCORE$",rownames(allcosts))],input$id)) 
      }else if(identical(input$choose,"All FAQ")){
        temp=unique(c(rownames(allcosts)[grep("^FAQ",rownames(allcosts))],input$id))
      }else if(identical(input$choose,"All MoCA")){
        temp=unique(c(c("i6serial","visuo","naming","attention","language","abstraction","orientation","delayedRecall"),input$id))
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