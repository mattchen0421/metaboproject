# ----------------------------------------
# --          PROGRAM global.R          --
# ----------------------------------------
# USE: Global variables and functions
#
# NOTEs:
#   - All variables/functions here are
#     globally scoped and will be available
#     to server, UI and session scopes
# ----------------------------------------

library(shiny)
library(periscope)
library(datamods)

# -- Setup your Application --
set_app_parameters(
    title = "Metabo project",
    titleinfo = NULL,
    loglevel = "DEBUG",
    showlog = FALSE,
    app_version = "1.0.0"
)

# -- PROGRAM --
