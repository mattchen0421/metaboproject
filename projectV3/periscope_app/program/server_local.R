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
library(scales)
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

output$t_test_var <- renderUI({
    selectInput("t_test_var",
        "select a data variable", choices = var_names()       
    )
})
output$t_test_id <- renderUI({
    selectInput("t_test_id",
        "select an id", choices = id_vars()       
    )
})
output$t_test_is <- renderUI({
    selectInput("t_test_is",
        "select a group", choices = is_vars()       
    )
})
t_test_data_wide <- reactive({
    req(data(), input$t_test_is, input$t_test_var, input$t_test_id)
    data_wide <- pivot_wider(
        data(),
        names_from = input$t_test_is,
        values_from = input$t_test_var,
        id_cols = input$t_test_id
    )
    two <- length(colnames(data_wide)) == 3
    # feedbackDanger("t_test_is", TRUE, "select a different group")
    req(two)
    data_wide
})
output$t_test <- renderPrint({
    t.test(
        pull(t_test_data_wide(), 2),
        pull(t_test_data_wide(), 3),
        paired = TRUE,
        alternative = "two.sided"
    )
})


# slope plot --------------------------------------------------------------

output$slope <- renderPlotly({
    req(data(), input$slope_change, input$slope_var, input$slope_id)
    slope_data <- pivot_wider(
        data(),
        names_from = input$slope_is,
        values_from = input$slope_var,
        id_cols = input$slope_id,
        unused_fn = list
    )
    var1 <- input$slope_change[1]
    var2 <- input$slope_change[2]
    p <- ggplot(slope_data, aes(text = .data[[filename()]])) +
        geom_violin(aes(var1, .data[[var1]])) +
        geom_violin(aes(var2, .data[[var2]])) +
        geom_point(
            aes(var1, .data[[var1]]),
            position = position_jitter(seed = 1, width = 0.1)
        ) +
        geom_point(
            aes(var2, .data[[var2]]),
            position = position_jitter(seed = 1, width = 0.1)
        ) +
        geom_segment(
            aes(
                x = var1, y = .data[[var1]],
                xend = var2, yend = .data[[var2]],
                color = .data[[var1]] > .data[[var2]]
            ),
            position = position_jitter(seed = 1, width = 0.1)
        ) +
        scale_y_continuous(labels = comma) +
        theme(legend.position = "none") +
        labs(x = NULL, y = NULL) +
        scale_color_manual(values = c("#F76D5E", "#72D9FF"))
    ggplotly(p, tooltip = "text")
})

output$slope_var <- renderUI({
    selectInput("slope_var",
        "choose data variable", choices = var_names()
    )
})
output$slope_id <- renderUI({
    selectInput("slope_id",
        "choose id", choices = id_vars()
    )
})
output$slope_is <- renderUI({
    selectInput("slope_is",
        "choose x", choices = is_vars()
    )
})
output$slope_change <- renderUI({
    selectizeInput("slope_change",
        "choose pre and post", multiple = TRUE, options = list(maxItems = 2),
        choices = data() |> 
            pull(input$slope_is) |> 
            unique()
    )
})