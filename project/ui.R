library(shiny)

# Define UI for application that draws a histogram
fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    fluidRow(
        column(width = 12,
            fileInput("file", "Upload data", buttonLabel = "Upload...")
        )
    ),
    # fluidRow(
    #     column(width = 2,
    #         downloadButton("download", label = "download and try example data")
    #     )
    # ),
    fluidRow(
        selectInput(
             "groups",
             "select group names",
             choices = c(),
             multiple = TRUE
        ),
        selectInput(
             "qc",
             "select qc names",
             choices = c()
        )
    ),
    fluidRow(
        tabsetPanel(
            tabPanel("info card",
                h2("資料集資訊"),
                textOutput("info_empty_row"),
                textOutput("info_miss"),
                textOutput("info_qc"),
                h3("各組數量"),
                tableOutput("group_sum")
            ),
            tabPanel("density plot",
                 sidebarLayout(
                    sidebarPanel(
                        selectInput("var_d", "variables", choices = c()),
                        checkboxInput("qc_d", "QC"),
                        tabsetPanel(
                            id = "switcher",
                            type = "hidden",
                            tabPanelBody("panel_check",
                                 selectInput("object_d", "select QC file", choices = c())       
                            ),
                            tabPanelBody("panel_empty", "")
                        ),
                        sliderInput("bw_d",
                            "change bandwidth",
                            min = 0.1, max = 2000000, 
                            value = 50000
                        )
                    ),
                    mainPanel(
                        plotOutput("density")
                    )
                 )
            ),
            tabPanel("correlation plot", 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("var_cor", "choose groups", choices = c()),
                     ),
                     mainPanel(
                         plotOutput("cor")  
                     )
                 )
            ),
            tabPanel("dumbbell plot", 
                 sidebarLayout(
                     sidebarPanel(
                        selectizeInput("group_db",
                                "groups",
                                choices = c(),
                                multiple = TRUE,
                                options = list(maxItems = 2)
                            ),
                        selectInput("var_db", "variables", choices = c())
                     ),
                     mainPanel(
                        plotOutput("dumbbell")
                     )
                 )
            ),
            tabPanel("box plot", 
                 sidebarLayout(
                    sidebarPanel(
                        checkboxGroupInput("var_b", "variables", choices = c())
                    ),
                    mainPanel(
                       plotOutput("box")
                    )
                 )
    
            )
        )
    )
)