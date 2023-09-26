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


# import data -------------------------------------------------------------


data_sheet_import <- import_file_server(
    "data_import", trigger_return = "change"
)
info_sheet_import <- import_file_server(
    "info_import", trigger_return = "change"
)
data_sheet <- reactive({
    if (any(is.null(data_sheet_import$status()), is.null(info_sheet_import$status()))) {
        return(read.xlsx("program/data/testing_file.xlsx", "Abundance", sep.names = " "))
    } else{
        return(data_sheet_import$data())
    }
})
info_sheet <- reactive({
    if (any(is.null(data_sheet_import$status()), is.null(info_sheet_import$status()))) {
        return(read.xlsx("program/data/testing_file.xlsx", "info_test", sep.names = " "))
    } else{
        return(info_sheet_import$data())
    }
})
output$is_demo <- renderValueBox({
    if (any(is.null(data_sheet_import$status()), is.null(info_sheet_import$status()))) {
        return(valueBox(value = "using demo data", subtitle = ""))
    } else{
        return(valueBox(value = "using imported data", subtitle = ""))
    }
})


# create variables --------------------------------------------------------



is_vars <- reactive({
    str_subset(names(info_sheet()), "_id", negate = TRUE)[-1]
})

id_vars <- reactive({
    str_subset(names(info_sheet()), "_id")
})

var_names <- reactive(names(data_sheet())[-1])
filename <- reactive(names(data_sheet())[1])

output$test <- renderPrint(data())

# create data table -------------------------------------------------------

by <- reactive({
    req(info_sheet(), data_sheet())
    setNames(names(info_sheet())[1], names(data_sheet())[1])
})

data <- reactive({
    data_sheet() |>
        right_join(info_sheet(), by = by()) |> 
        mutate(across(ends_with("_id"), ~factor(.x)))
})

# summary tab -------------------------------------------------------------

output$info_miss <- renderValueBox({
    valueBox(
        data()[!apply(data() |> is.na(), 1, all),] |> 
        is.na() |>
        sum(),
        "個缺失值",
        icon = icon("circle-exclamation")
    )
})

output$info_id <- renderText({
    paste0(id_vars(), collapse = ", ")
})

output$info_is <- renderText({
    paste0(is_vars(), collapse = ", ")
})

output$info_vars <- renderText({
    paste0(var_names(), collapse = ", ")
})

output$group_sum <- renderTable({
    req(input$info_group)
    summarise(data(), n(), .by = input$info_group)
})
observeEvent(is_vars(),
    updateCheckboxGroupInput(session, "info_group", choices = is_vars())
)

output$t_test <- renderPrint({
    req(data(), input$t_test_is, input$t_test_var, input$t_test_id, input$t_test_change)
    data_wide <- pivot_wider(
        data(),
        names_from = input$t_test_is,
        values_from = input$t_test_var,
        id_cols = input$t_test_id
    )
    t.test(
        pull(data_wide, input$t_test_change[1]),
        pull(data_wide, input$t_test_change[2]),
        paired = TRUE,
        alternative = "two.sided"
    ) 
})