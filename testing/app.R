library(shiny)

shinyApp(
  ui = fluidPage(
    textInput("txt", "Enter new query string"),
    helpText("Format: ?param1=val1&param2=val2"),
    actionButton("go", "Update"),
    hr(),
    verbatimTextOutput("query")
  ),
  server = function(input, output, session) {
    observeEvent(input$go, {
      updateQueryString(input$txt, mode = "push")
    })
    output$query <- renderText({
      query <- getQueryString()
      queryText <- paste(names(query), query,
                         sep = "=", collapse=", ")
      paste("Your query string is:\n", queryText)
    })
  }
)
