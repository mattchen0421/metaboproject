# ----------------------------------------
# --       PROGRAM server_local.R       --
# ----------------------------------------
# USE: Session-specific variables and
#      functions for the main reactive
#      shiny server functionality.  All
#      code in this file will be put into
#      the framework inside the call to
#      shinyServer(function(input, output, session)
#      in server.R
#
# NOTEs:
#   - All variables/functions here are
#     SESSION scoped and are ONLY
#     available to a single session and
#     not to the UI
#
#   - For globally scoped session items
#     put var/fxns in server_global.R
#
# FRAMEWORK VARIABLES
#     input, output, session - Shiny
#     ss_userAction.Log - Reactive Logger S4 object
# ----------------------------------------

# -- IMPORTS --
library(thematic)
library(openxlsx)

# -- VARIABLES --


# -- FUNCTIONS --


# ----------------------------------------
# --          SHINY SERVER CODE         --
# ----------------------------------------
thematic_shiny()
data_sheet_import <- import_file_server(
    "data_import", trigger_return = "change", reset = reactive(input$data_demo)
)
info_sheet_import <- import_file_server("info_import", trigger_return = "change")
data_sheet <- reactive({
    if (input$data_demo) {
        return(read.xlsx("program/data/testing_file.xlsx", "Abundance", sep.names = " "))
    } else{
        return(data_sheet_import$data())
    }
})
output$test <- renderPrint(colnames(data_sheet()))