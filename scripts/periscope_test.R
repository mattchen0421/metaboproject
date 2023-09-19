library(shiny)
library(periscope)
library(ggplot2)
ui <- fluidPage(
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
    
    png <- reactive({

        print(
            ggplot(data = iris, aes(Sepal.Length, Sepal.Width)) +
                geom_point()
        )

    })
    plot <- reactive({

        print(
            ggplot(data = iris, aes(Sepal.Length, Sepal.Width)) +
                geom_point()
        )
    })

    downloadablePlot(
        "object_id1",
         logger = ss_userAction.Log,
         filenameroot = "mydownload1",
         downloadfxns = list(png = png),
         visibleplot = plot
    )
    
}

shinyApp(ui = ui, server = server)

app_dir = tempdir()
create_new_application('mysampleapp', location = app_dir, sampleapp = TRUE)
runApp(paste(app_dir, 'mysampleapp', sep = .Platform$file.sep))

create_new_application(name = 'mytestapp', location = "/R_profile/metaboproject/periscope_test")
