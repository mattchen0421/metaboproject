# V2
library(shiny)
library(shinyjs)
library(plotly)
library(waiter)
library(shinyglide)
library(shinyWidgets)
library(htmltools)
library(sortable)
library(periscope)

fluidPage(
    shinythemes::themeSelector(),
    useShinyjs(),
    useWaiter(),
    shinyFeedback::useShinyFeedback(),
    # theme = bslib::bs_theme(bootswatch = "darkly"),
    navbarPage("目錄",

# info card ---------------------------------------------------------------

        tabPanel("info card",
             fluidRow(
                 column(width = 4,
                        fileInput("file", "Upload data", buttonLabel = "Upload..."),
                 ),
                 column(width = 4,
                        selectInput("sheet",
                            "select information sheet", choices = c()
                        ),
                 ),
             ),
            fluidRow(
                column(3,
                    h3("缺失值"),
                    textOutput("info_miss")
                ),
                column(3,
                    h3("欄位名稱"),
                    textOutput("info_id"),
                    textOutput("info_is"),
                    textOutput("info_var")
                ),
                column(3,
                    h3("各組數量"),
                    checkboxGroupInput("info_group",
                        "選擇條件",
                        choices = c()
                    )
                ),
                column(3, 
                    tableOutput("group_sum") 
                )
            ),
            fluidRow(
                h3("成對t檢定"),
            ),
            fluidRow(
                selectInput("t_test_id",
                    "choose id", choices = c()
                ),  
                selectInput("t_test_is",
                    "choose condition", choices = c()
                ),
                selectizeInput("t_test_change",
                   "choose pre and post", choices = c(),
                   multiple = TRUE, options = list(maxItems = 2)
                ),
                selectInput("t_test_var",
                    "choose variables", choices = c()
                ),
                column(6,
                    verbatimTextOutput("t_test")
                )
            )
        ),

# slope plot ------------------------------------------------------------
    tabPanel("slope plot", 
        sidebarLayout(
            sidebarPanel(width = 2, 
                selectInput("slope_is",
                     "choose x", choices = c()
                ),
                selectizeInput("slope_change",
                    "choose pre and post", choices = c(),
                    multiple = TRUE, options = list(maxItems = 2)
                ),
                selectInput("slope_id",
                     "choose id", choices = c()
                ),
                selectInput("slope_var",
                    "choose y", choices = c()
                )
            ),
            mainPanel(
                shinycssloaders::withSpinner(
                    plotlyOutput("slope", height = "600px")
                )
            )
        )
    ),      
# density plot ------------------------------------------------------------

        tabPanel("density plot",
            sidebarLayout(
                sidebarPanel(width = 3,
                    selectInput("density_var", "choose variables", choices = c()),
                    selectInput("density_group", "choose group", choices = c()),
                    sliderInput("density_bw",
                        "change bandwidth", min = 0, max = 1, value = 1
                    )
                ),
                mainPanel(
                    shinycssloaders::withSpinner(
                        plotlyOutput("density", height = "600px")
                    )
                )
            )
        ),

# Ridgeline plots ---------------------------------------------------------

        tabPanel("Ridgeline plots",
            sidebarLayout(
                sidebarPanel(width = 2,
                    checkboxGroupInput("ridges_var",
                        "choose variables", choices = c()
                    )
                ),
                mainPanel(
                    shinycssloaders::withSpinner(
                        plotOutput("ridges")
                    )
                )
            )
        ),

# correlation plot --------------------------------------------------------

        tabPanel("correlation plot", 
            sidebarLayout(
                sidebarPanel(width = 2,
                    selectInput("cor_is",
                        "choose condition", choices = c()    
                    ),
                    selectInput("cor_group",
                        "choose group", choices = c()    
                    ),
                ),
                mainPanel(
                    shinycssloaders::withSpinner(
                       plotOutput("cor")  
                    )
                )
            )
        ),

# dumbbell plot -----------------------------------------------------------

        tabPanel("dumbbell plot", 
            sidebarLayout(
                sidebarPanel(width = 2,
                    selectInput("dumbbell_id",
                        "choose id", choices = c()
                    ),  
                    selectInput("dumbbell_is",
                        "choose condition", choices = c()
                    ),
                    selectizeInput("dumbbell_change",
                        "choose pre and post",
                        choices = c(), multiple = TRUE,
                        options = list(maxItems = 2)
                    ),
                    selectInput("dumbbell_var",
                        "choose variables", choices = c()
                    ),
                ),
                mainPanel(
                    shinycssloaders::withSpinner(
                        plotlyOutput("dumbbell", height = "600px")
                    )
                )
            )
        ),

# box plot ----------------------------------------------------------------

        tabPanel("box plot", 
            sidebarLayout(
                sidebarPanel(width = 2,
                    selectInput("box_var",
                        "choose variables", choices = c()
                    ),
                    selectInput("box_is",
                        "choose condition", choices = c()
                    )
                 ),
                 mainPanel(
                     shinycssloaders::withSpinner(
                        plotOutput("box")
                     )
                 )
            )
        ),

# PLSDA -------------------------------------------------------------------
        tabPanel("PLSDA",
            sidebarLayout(
                sidebarPanel(width = 2,
                    numericInput("plsda_n",
                        "choose number of component",
                        value = 10, min = 2
                     ),
                     selectInput("plsda_is",
                        "choose condition", choices = c()
                     ),
                     actionButton("plsda_start", "start"),
                     actionButton("plsda_perf_start", "performance")
                 ),
                mainPanel(
                    glide(controls_position = "top", height = "800px",
                        shinyglide::screen(next_condition = "input.plsda_perf_start > 0",
                            pickerInput("plsda_score_n",
                                label = "Select which componemt to show", 
                                choices = c(),
                                options = list(
                                    `actions-box` = TRUE,
                                    `selected-text-format` = "count > 2"
                                ), 
                                multiple = TRUE
                            ),
                            downloadButton('plsda_download'),
                        shinycssloaders::withSpinner(
                                plotOutput("plsda_score")
                            )
                        ),
                        shinyglide::screen(
                            shinycssloaders::withSpinner(
                                plotlyOutput("plsda_perf", height = "600px")
                            )
                        )
                    )
                )    
            )
        ),

# sPLSDA ------------------------------------------------------------------
        tabPanel("sPLSDA",
            sidebarLayout(
                sidebarPanel(width = 2,
                    selectInput("splsda_is",
                        "choose condition", choices = c()
                    ),
                    numericInput("splsda_ncomp",
                        "choose number of component", value = 5,
                        max = 20, min = 2
                    ),
                    selectInput("splsda_keep",
                        "choose keepX",
                        choices = c("default")
                    ),
                    h6("number of variables to keep"),
                    textOutput("splsda_keepx"),
                    fluidRow(
                        column(width = 6, actionButton("splsda_tune", "tune")),
                        column(width = 6, actionButton("splsda_start", "start"))
                    ),
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("Overview",
                            shinycssloaders::withSpinner(
                                plotOutput("splsda_overview")
                            )
                        ),
                        tabPanel("Score plot",
                            selectizeInput("splsda_score_n",
                                "choose component to show",
                                choices = c(), multiple = TRUE,
                                options = list(maxItems = 2)
                            ),
                            shinycssloaders::withSpinner(
                                plotOutput("splsda_score")
                            )
                        ),
                        tabPanel("Loading plot",
                            fluidRow(
                                column(4,
                                    selectInput("splsda_loading_n",
                                        "choose component to show",
                                        choices = c()
                                    ),
                                ),
                                column(6,
                                    numericInput("splsda_loading_valve",
                                        "show variables with loading higher than chosen threshold",
                                        value = 0.5, min = 0, max = 1,
                                        step = 0.1, width = 500 
                                    ),
                                    actionButton("splsda_loading_refresh", "refresh"), 
                                )
                            ),
                            shinycssloaders::withSpinner(
                                plotOutput("splsda_loading")
                            )
                        ),
                        tabPanel("Correlation Circle Plot",
                            fluidRow(
                                column(4,
                                    selectizeInput("splsda_cor_n",
                                        "choose component to show",
                                        choices = c(), multiple = TRUE,
                                        options = list(maxItems = 2)
                                    ),
                                ),
                                column(4,
                                    numericInput("splsda_cor_vip",
                                        "show by importance",
                                        value = 10, step = 1, min = 1
                                    )
                                )
                            ),
                            shinycssloaders::withSpinner(
                                plotOutput("splsda_cor")
                            )
                        ),
                        tabPanel("Performance",
                            actionButton("splsda_perf_start", "start"),
                            verbatimTextOutput("splsda_perf_er"),
                            selectInput("splsda_perf_n", 
                                "choose component to show",
                                choices = c()
                            ),
                            shinycssloaders::withSpinner(
                                plotOutput("splsda_perf")
                            )
                        )
                    )
                )
            )
        ),

# SOM ---------------------------------------------------------------------

        tabPanel("SOM",
            sortable_js("som_plots"),
            sidebarLayout(
                sidebarPanel(width = 2,
                    selectInput("som_is",
                        "choose condition", choices = c()                
                    ),
                    sliderInput("som_rlen",
                        "choose rlen", value = 300, min = 1, max = 2000     
                    ),
                    actionButton("som_start", "start")
                ),
                mainPanel(width = 10,
                    tabsetPanel(
                        tabPanel("main",
                            tags$div(
                                id = "som_plots",
                                column(
                                    width = 4,
                                    wellPanel(
                                        plotOutput("som_map")
                                    )
                                ),
                                column(
                                    width = 4,
                                    wellPanel(
                                        plotOutput("som_pie")
                                    )
                                ),
                                column(
                                    width = 4,
                                    wellPanel(
                                        plotOutput("som_dist")
                                    )
                                ),
                                column(
                                    width = 4,
                                    wellPanel(
                                        plotOutput("som_changes")
                                    )
                                ),
                                column(
                                    width = 4,
                                    wellPanel(
                                        plotOutput("som_count")
                                    )
                                ),
                                column(
                                    width = 4,
                                    wellPanel(
                                        plotOutput("som_codes")
                                    )
                                ),
                            )
                        ),
                        tabPanel("heatmaps",
                            uiOutput("som_heatmaps_ui")
                        )
                    )
                )
            )
        )
    )
)
