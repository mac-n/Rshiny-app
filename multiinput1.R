# Only run examples in interactive R sessions

  
  library("shiny")
  library("shinyWidgets")
  
  
  # simple use
  
  ui <- fluidPage(
    multiInput(
      inputId = "id", label = "Fruits :",
      choices = c("Banana", "Blueberry", "Cherry",
                  "Coconut", "Grapefruit", "Kiwi",
                  "Lemon", "Lime", "Mango", "Orange",
                  "Papaya"),
      selected = "Banana", width = "350px"
    ),
    verbatimTextOutput(outputId = "res"),
    DTOutput('x1'),
  )
  
  server <- function(input, output, session) {
    output$res <- renderPrint({
      input$id
    })
  }
  
  shinyApp(ui = ui, server = server)
  
  
  # with options
  
  ui <- fluidPage(
    multiInput(
      inputId = "id", label = "Fruits :",
      choices = c("Banana", "Blueberry", "Cherry",
                  "Coconut", "Grapefruit", "Kiwi",
                  "Lemon", "Lime", "Mango", "Orange",
                  "Papaya"),
      selected = "Banana", width = "400px",
      options = list(
        enable_search = FALSE,
        non_selected_header = "Choose between:",
        selected_header = "You have selected:"
      )
    ),
    verbatimTextOutput(outputId = "res")
  )
  x = reactiveValues(df = NULL)
  observe({
    df <- data
    #df$Date = Sys.time() + seq_len(nrow(df))
    x$df <- df
  })
  server <- function(input, output, session) {
    x = reactiveValues(df = NULL)
    output$res <- renderPrint({
      input$id
    })
    observe({
      inputId = "id"
      df <- exportcosts
      #df$Date = Sys.time() + seq_len(nrow(df))
      x$df <- data.frame(df)
    })
  }
  
  shinyApp(ui = ui, server = server)
