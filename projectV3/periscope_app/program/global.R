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
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(shinydashboardPlus)
library(shinydashboard)
library(shinyFeedback)
library(shinyWidgets)
library(shinyglide)
library(sortable)
library(periscope)
library(datamods)
library(thematic)
library(waiter)

library(plotly)
library(ggforce)
library(ggpubr)
library(scales)
library(paletteer)

library(tidyverse)
library(openxlsx)
library(mixOmics)
library(kohonen)

# -- Setup your Application --
set_app_parameters(
    title = "Metabo project",
    titleinfo = NULL,
    loglevel = "DEBUG",
    showlog = FALSE,
    app_version = "1.0.0"
)

# -- PROGRAM --

source("program/fxn/select_var_module.R")
conflicted::conflicts_prefer(shinydashboard::box)
conflicted::conflicts_prefer(purrr::set_names)
conflicted::conflicts_prefer(shinydashboardPlus::dashboardPage)
conflicted::conflicts_prefer(shinyglide::screen)
