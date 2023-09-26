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
library(shinyFeedback)
library(waiter)

# ----------------------------------------
# --      BODY ELEMENT CREATION         --
# ----------------------------------------

# -- Create Elements
themeSelector()
useShinyjs()
useWaiter()
useShinyFeedback()

tabs <- tabItems(
    tabItem(tabName = "import",
        box(
            width = 12,
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
    tabItem(tabName = "summary",
        valueBoxOutput("info_miss", 3),
        box(title = "Names of id variable",
            width = 3, background = "aqua",
            textOutput("info_id")
        ),
        box(title = "Names of grouping variable",
            width = 3, background = "aqua",
            textOutput("info_is")
        ),
        box(title = "Names of data sheet variable",
            width = 3, background = "aqua",
            textOutput("info_vars")
        ),
        box(title = "Rows of each group",
            width = 4,
            checkboxGroupInput("info_group",
                "choose group", choices = c()
            ),
            tableOutput("group_sum")
        )
    )
    
)
    
# summary <- box(
#     title = "Data Summary",
#     width  = 12, collapsible = TRUE, collapsed = TRUE,
#   
#         box(
#             width = 3, title = "缺失值",
#             textOutput("info_miss")
#         ),
#         box(
#             width = 3, title = "欄位名稱",
#             textOutput("info_id"),
#             textOutput("info_is"),
#             textOutput("info_var")
#         ),
#         box(
#             width = 3, title = "各組數量",
#             checkboxGroupInput("info_group",
#                 "選擇條件",
#                 choices = c()
#             ),
#             tableOutput("group_sum") 
#         ),
#         box(
#             width = 3, title = "test",
#             verbatimTextOutput("test") 
#         )
    
        
   
    # fluidRow(
    #     h3("成對t檢定"),
    # ),
    # fluidRow(
    #     selectInput("t_test_id",
    #         "choose id", choices = c()
    #     ),  
    #     selectInput("t_test_is",
    #         "choose condition", choices = c()
    #     ),
    #     selectizeInput("t_test_change",
    #         "choose pre and post", choices = c(),
    #         multiple = TRUE, options = list(maxItems = 2)
    #     ),
    #     selectInput("t_test_var",
    #         "choose variables", choices = c()
    #     ),
    #     box(6,
    #         verbatimTextOutput("t_test")
    #     )
    # )
# )

# -- Register Elements in the ORDER SHOWN in the UI
add_ui_body(list(tabs))
