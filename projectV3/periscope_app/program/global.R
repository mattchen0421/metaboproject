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
library(shinydashboard)
library(tidyverse)
library(shinyFeedback)
library(plotly)

# -- Setup your Application --
set_app_parameters(
    title = "Metabo project",
    titleinfo = NULL,
    loglevel = "DEBUG",
    showlog = TRUE,
    app_version = "1.0.0"
)

# -- PROGRAM --

source("program/fxn/select_var_module.R")