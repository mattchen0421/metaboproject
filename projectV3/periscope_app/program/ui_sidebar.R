# ----------------------------------------
# --       PROGRAM ui_sidebar.R         --
# ----------------------------------------
# USE: Create UI elements for the
#      application sidebar (left side on
#      the desktop; contains options) and
#      ATTACH them to the UI by calling
#      add_ui_sidebar_basic() or
#      add_ui_sidebar_advanced()
#
# NOTEs:
#   - All variables/functions here are
#     not available to the UI or Server
#     scopes - this is isolated
# ----------------------------------------

# -- IMPORTS --



# ----------------------------------------
# --     SIDEBAR ELEMENT CREATION       --
# ----------------------------------------

# -- Create Basic Elements
# file_input <- fileInput("file", "Upload data", buttonLabel = "Upload...")
# test <- import_file_ui("myid")
# sheet_input <- selectInput("sheet",
#     "select information sheet", choices = c()
# )
menu <- sidebarMenu(
    menuItem("import data", tabName = "import", icon = icon("file-import")),
    menuItem("summary", tabName = "summary", icon = icon("dashboard")),
    menuItem("slope plot", tabName = "slope", icon = icon("chart-line")),
    menuItem("density plot", tabName = "density", icon = icon("chart-area")),
    menuItem("ridgeline plot", tabName = "ridges", icon = icon("chart-area")),
    menuItem("correlation plot", tabName = "corr", icon = icon("chart-area")),
    menuItem("dumbbell plot", tabName = "dumbbell", icon = icon("chart-area")),
    menuItem("box plot", tabName = "box", icon = icon("chart-area"))
)

# -- Register Basic Elements in the ORDER SHOWN in the UI
add_ui_sidebar_basic(
    append = FALSE,
    list(
        menu
        # sheet_input,
        # test
    ), 
)



# -- Create Advanced Elements


# -- Register Advanced Elements in the ORDER SHOWN in the UI
add_ui_sidebar_advanced()
