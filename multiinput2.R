
library(shiny)
library(shinyWidgets)

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
  DTOutput(outputId = "res"),
  selectInput(
    inputId = "selected",
    label = "Update selected:",
    choices = fruits,
    multiple = TRUE
  ),
  textInput(inputId = "label", label = "Update label:")
)

server <- function(input, output, session) {
  x = reactiveValues(df = NULL)
  observe({
    df <- data.frame(input$my_multi)
    #df$Date = Sys.time() + seq_len(nrow(df))
    x$df <- df
  })
  
  output$res <- renderDT(x$df,
  options = list( pageLength = 10, info = FALSE, lengthMenu = list(c(15, -1), c("10", "All"))),editable=list(target = "cell", disable = list(columns = c(0:0)))
  )

  observeEvent(input$selected, {
    updateMultiInput(
      session = session,
      inputId = "my_multi",
      selected = input$selected,
      print(input$selected)
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
