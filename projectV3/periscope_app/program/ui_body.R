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
library(shinythemes)
library(shinyjs)
library(waiter)
library(shinycssloaders)

# ----------------------------------------
# --      BODY ELEMENT CREATION         --
# ----------------------------------------

# -- Create Elements
themeSelector()
useShinyjs()
useWaiter()
useShinyFeedback()

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
            uiOutput("slope_id"),
            uiOutput("slope_is"),
            uiOutput("slope_change"),
        ),
        box(
            width = 8,
            withSpinner(
                plotlyOutput("slope", height = "600px")
            )
        )
    )

    
)
    


# -- Register Elements in the ORDER SHOWN in the UI
add_ui_body(list(tabs))
