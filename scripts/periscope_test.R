library(shiny)
library(periscope)
library(ggplot2)
ui <- fluidPage(
    plotOutput("plot"),
    wellPanel(
        downloadablePlotUI(
            "object_id1",
            downloadtypes = c("png"),
            download_hovertext = "Download the plot and data here!",
            height = "400px",
            btn_valign = "bottom"
        )
    )
    
)

server <- function(input, output) {
    ss_userAction.Log <- periscope:::fw_get_user_log()
    output$plot <- renderPlot(test())
    data <- reactive(iris)
    plotInput <- function(variables) {
        p <- ggplot(data = data(), aes(Sepal.Length, Sepal.Width)) +
            geom_point()
        p
    }
    test <- reactive({
        ggplot(data = data(), aes(Sepal.Length, Sepal.Width)) +
            geom_point()
        
    })
      
    downloadablePlot(
        "object_id1",
         logger = ss_userAction.Log,
         filenameroot = "mydownload1",
         downloadfxns = list(png = plotInput),
         visibleplot = plotInput
    )
    
}

shinyApp(ui = ui, server = server)
