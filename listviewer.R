data(mtcars)

ui <- shinyUI(
  fluidPage(
    reactjsonOutput( "rjed" )
  )
)

server <- function(input,output){
  output$rjed <- renderReactjson({
    reactjson( as.list( mtcars ) )
  })
  
  observeEvent(input$rjed_edit, {
    str(input$rjed_edit, max.level=2)
  })
}

runApp( list( ui = ui, server = server ) )