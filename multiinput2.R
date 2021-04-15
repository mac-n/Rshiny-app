
library(shiny)
library(shinyWidgets)
library(DT)
fruits <- c("Banana", "Blueberry", "Cherry",
            "Coconut", "Grapefruit", "Kiwi",
            "Lemon", "Lime", "Mango", "Orange",
            "Papaya")

ui <- fluidPage(
  tags$h2("Multi update"),
  multiInput(
    inputId = "my_multi",
    label = "Fruits :",
    choices = fruits,
    selected = "Banana",
    width = "350px"
  ),
  verbatimTextOutput(outputId = "res"),
  selectInput(
    inputId = "selected",
    label = "Update selected:",
    choices = fruits,
    multiple = TRUE
  ),
  textInput(inputId = "label", label = "Update label:")
)

server <- function(input, output, session) {
  
  output$res <- renderPrint(input$my_multi)
  
  observeEvent(input$selected, {
    updateMultiInput(
      session = session,
      inputId = "my_multi",
      selected = input$selected
    )
  })
  
  observeEvent(input$label, {
    updateMultiInput(
      session = session,
      inputId = "my_multi",
      label = input$label
    )
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
