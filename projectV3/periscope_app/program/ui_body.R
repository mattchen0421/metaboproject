# ----------------------------------------
# --          PROGRAM ui_body.R         --
# ----------------------------------------
# USE: Create UI elements for the
#      application body (right side on the
#      desktop; contains output) and
#      ATTACH them to the UI by calling
#      add_ui_body()
#
# NOTEs:
#   - All variables/functions here are
#     not available to the UI or Server
#     scopes - this is isolated
# ----------------------------------------

# -- IMPORTS --

# ----------------------------------------
# --      BODY ELEMENT CREATION         --
# ----------------------------------------

# -- Create Elements
setup <- tagList(
    useShinyjs(),
    useWaiter(),
    useShinyFeedback()
)
tabs <- tabItems(

# import tab --------------------------------------------------------------

    
    tabItem(tabName = "import",
        box(
            width = 12, collapsible = TRUE,
            valueBoxOutput("is_demo", width = 12),
            import_file_ui("data_import",
                title = "import data sheet",
                file_extensions = c(".xlsx")
            ),
            import_file_ui("info_import",
                title = "import info sheet",
                file_extensions = c(".xlsx")
            )
        )
    ),


# summary tab -------------------------------------------------------------


    tabItem(tabName = "summary",
        valueBoxOutput("info_miss", 3),
        box(title = "Names of id variable",
            width = 3, background = "aqua", collapsible = TRUE,
            textOutput("info_id")
        ),
        box(title = "Names of grouping variable",
            width = 3, background = "aqua", collapsible = TRUE,
            textOutput("info_is")
        ),
        box(title = "Names of data sheet variable",
            width = 3, background = "aqua", collapsible = TRUE,
            textOutput("info_vars")
        ),
        box(title = "Rows of each group",
            width = 6, background = "light-blue", collapsible = TRUE,
            checkboxGroupInput("info_group",
                "choose group", choices = c()
            ),
            tableOutput("group_sum")
        ),
        box(title = "testing",
            width = 6, background = "light-blue", collapsible = TRUE,
            uiOutput("t_test_var"),
            uiOutput("t_test_id"),
            uiOutput("t_test_is"),
            verbatimTextOutput("t_test")
        )
    ),

# slope plot --------------------------------------------------------------
    tabItem(tabName = "slope",
        box(
            width = 4,
            uiOutput("slope_var"),
            uiOutput("slope_is"),
            uiOutput("slope_change"),
        ),
        box(
            width = 8,
            withSpinner(
                plotlyOutput("slope", height = "600px")
            )
        )
    ),

# density plot ------------------------------------------------------------
    tabItem(tabName = "density",
        box(
            width = 4,
            uiOutput("density_var"),
            uiOutput("density_group"),
            uiOutput("density_bw"),
        ),
        box(
            width = 8,
            withSpinner(
                plotlyOutput("density", height = "600px")
            )
        )
    ),

# ridgeline plot ----------------------------------------------------------
    tabItem(tabName = "ridges",
        box(
            width = 4,
            uiOutput("ridges_var")
        ),
        uiOutput("ridges_ui")
    ),

# correlation plot --------------------------------------------------------
    tabItem(tabName = "corr",
        box(
            width = 4,
            uiOutput("cor_is"),
            uiOutput("cor_group")
        ),
        box(
            width = 8, height = 650,
            withSpinner(
                plotOutput("corr")  
            )
        )
    ),

# dumbbell plot -----------------------------------------------------------
    tabItem(tabName = "dumbbell",
        box(
            width = 4,
            uiOutput("dumbbell_var"),
            uiOutput("dumbbell_is"),
            uiOutput("dumbbell_change")
        ),
        box(
            width = 8,
            withSpinner(
                plotlyOutput("dumbbell", height = "600px")
            )
        )
    ),

# box plot ----------------------------------------------------------------

    tabItem(tabName = "box",
        box(
            width = 4,
            uiOutput("box_var"),
            uiOutput("box_is")
        ),
        box(
            width = 8,
            downloadablePlotUI("box",
                height = 600, btn_valign = "top"
            )
        )
    ),

# PLSDA -------------------------------------------------------------------
    tabItem(tabName = "plsda",
        column(
            width = 2, 
            box(
                width = NULL,
                uiOutput("plsda_n"),
                uiOutput("plsda_is"),
                actionButton("plsda_start", "start"),
                actionButton("plsda_perf_start", "performance")
            ),
            box(
                width = NULL,
                uiOutput("plsda_score_n"),
                downloadButton('plsda_download'),
            )
        ),
        column(
            width = 10,
            box(
                width = NULL, collapsible = TRUE,
                withSpinner(
                    plotOutput("plsda_score", height = "600px")
                )
            ),
            box(
                width = NULL, collapsible = TRUE, collapsed = TRUE,
                plotlyOutput("plsda_perf", height = "600px")
            )
        )
    )
    
)
    


# -- Register Elements in the ORDER SHOWN in the UI
add_ui_body(list(setup, tabs))
