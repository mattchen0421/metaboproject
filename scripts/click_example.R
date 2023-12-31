if (interactive()) {
    library(shiny)
    
    shinyApp(
        ui = fluidPage(
            useShinyjs(),  # Set up shinyjs
            "Count:", textOutput("number", inline = TRUE), br(),
            actionButton("btn", "Click me"), br(),
            "The button will be pressed automatically every 3 seconds"
        ),
        server = function(input, output) {
            output$number <- renderText({
                input$btn
            })
            observe(input$btn,, {
                click("btn")
            })
        }
    )
}
