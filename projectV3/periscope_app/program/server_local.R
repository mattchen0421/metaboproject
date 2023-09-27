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

is_varsP <- reactive({
    code <- str_which(paste0(is_vars(), "_id"), paste(id_vars(), collapse = '|'))
    is_vars()[code]        
})
id_varsP <- reactive({
    set_names(id_vars(), is_varsP())
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

output$info_is <- renderText({
    paste0(is_varsP(), collapse = ", ")
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
output$t_test_is <- renderUI({
    selectInput("t_test_is",
        "select a group", choices = is_varsP()       
    )
})

t_test_data_wide <- reactive({
    req(data(), input$t_test_is, input$t_test_var)
    data_wide <- pivot_wider(
        data(),
        names_from = input$t_test_is,
        values_from = input$t_test_var,
        id_cols = unname(id_varsP()[input$t_test_is])
    )
    two <- length(colnames(data_wide)) == 3
    req(two)
    data_wide
})
output$t_test <- renderPrint({
    req(t_test_data_wide())
    t.test(
        pull(t_test_data_wide(), 2),
        pull(t_test_data_wide(), 3),
        paired = TRUE,
        alternative = "two.sided"
    )
})


# slope plot --------------------------------------------------------------

output$slope_var <- renderUI({
    selectInput("slope_var",
        "choose data variable", choices = var_names()
    )
})
output$slope_is <- renderUI({
    selectInput("slope_is",
        "choose x", choices = is_varsP()
    )
})
output$slope_change <- renderUI({
    req(input$slope_is)
    selectizeInput("slope_change",
        "choose pre and post", multiple = TRUE, options = list(maxItems = 2),
        choices = data() |> 
            pull(input$slope_is) |> 
            unique()
    )
})

output$slope <- renderPlotly({
    req(data(), length(input$slope_change) == 2, input$slope_var, input$slope_is)
    slope_data <- pivot_wider(
        data(),
        names_from = input$slope_is,
        values_from = input$slope_var,
        id_cols = unname(id_varsP()[input$slope_is]),
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

# density plot ------------------------------------------------------------

output$density_var <- renderUI({
    selectInput("density_var", "choose variables", choices = var_names())
})
output$density_group <- renderUI({
    selectInput("density_group", "choose group", choices = is_vars())
})
output$density_bw <- renderUI({
    req(input$density_var)
    sliderInput("density_bw",
        "change bandwidth",
        max = bw.nrd(data() |> drop_na() |> pull(input$density_var)),
        min = bw.nrd(data() |> drop_na() |> pull(input$density_var)) / 100,
        value = bw.nrd(data() |> drop_na() |> pull(input$density_var))
    )
})

output$density <- renderPlotly({
    req(input$density_var, input$density_group, input$density_bw)
    p <- ggplot(data(),
        aes(
            .data[[input$density_var]],
            fill = factor(.data[[input$density_group]])
        )
    ) +
        geom_density(alpha = 0.5, bw = input$density_bw) +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        labs(fill = NULL) 
    ggplotly(p) |> 
        plotly::layout(
            legend = list(
                orientation = 'h', x = 0, y = 1,
                font = list(size = 20)
            )
        )
})

# ridgeline plot ----------------------------------------------------------

output$ridges_var <- renderUI({
    checkboxGroupInput("ridges_var",
        "choose variables", choices = var_names()
    )
})

ridges_height <- reactive({
    ifelse(length(input$ridges_var) > 4, length(input$ridges_var)*150, 600)
})

output$ridges <- renderPlot(height = function() ridges_height(), res = 144, {
    req(input$ridges_var)
    ggplot(
        data() |>
            pivot_longer(all_of(input$ridges_var), names_to = "vars", values_to = "values"),
        aes(x = values, y = vars, fill = after_stat(quantile))
    ) +
        ggridges::stat_density_ridges(
            quantile_lines = TRUE,
            calc_ecdf = TRUE,
            geom = "density_ridges_gradient",
            quantiles = c(0.05, 0.95),
            color = 3,
            scale = 0.8
        ) +
        scale_fill_manual(
            name = "Prob.",
            values = c("pink", "white", "lightblue"),
            labels = c("(0, 5%]", "(5%, 95%]", "(95%, 1]")
        ) +
        scale_x_continuous(labels = comma)
})
        
output$ridges_ui <- renderUI({
    box(
        width = 8, height = ridges_height() + 50,
        withSpinner(
            plotOutput("ridges")
        )
    )
})

# correlation plot --------------------------------------------------------

output$cor_is <- renderUI({
    selectInput("cor_is",
        "choose condition", choices = is_vars()
    )
})
output$cor_group <- renderUI({
    req(input$cor_is)
    selectInput("cor_group",
        "choose group",
        choices = data() |> 
            pull(input$cor_is) |> 
            unique() 
    )
})

output$corr <- renderPlot(height = 600, res = 144, {
    req(input$cor_is, input$cor_group, data())
    cor(
        data() |>
            filter(.data[[input$cor_is]] == input$cor_group) |> 
            select(all_of(var_names())) |> 
            drop_na()
    ) |> 
        round(2) |> 
        corrplot::corrplot(
            method = "color",
            order = 'alphabet',
            type = "lower",
            diag = FALSE,
            tl.srt = 45,
            # tl.col = "white",
            tl.cex = 6/length(var_names()) + 0.5
        )
})

# dumbbell plot -----------------------------------------------------------

output$dumbbell_var <- renderUI({
    selectInput("dumbbell_var",
        "choose variables", choices = var_names()
    )
})
output$dumbbell_is <- renderUI({
    selectInput("dumbbell_is",
        "choose condition", choices = is_varsP()
    )
})
output$dumbbell_change <- renderUI({
    req(input$dumbbell_is)
    selectizeInput("dumbbell_change",
        "choose pre and post", multiple = TRUE, options = list(maxItems = 2),
        choices = data() |> 
            pull(input$dumbbell_is) |> 
            unique()
    )
})

output$dumbbell <- renderPlotly({
    req(input$dumbbell_is, input$dumbbell_var)
    dumbbell_data <- data() |>
        pivot_wider(
            names_from = input$dumbbell_is,
            values_from = input$dumbbell_var,
            id_cols = unname(id_varsP()[input$dumbbell_is]),
            unused_fn = list
        )
    req(length(input$dumbbell_change) == 2)
    p <- ggplot(dumbbell_data,
        aes(y = reorder(
                    .data[[id_varsP()[input$dumbbell_is]]],
                    .data[[input$dumbbell_change[1]]]
            ),
            text = .data[[id_varsP()[input$dumbbell_is]]]
        )
    ) +
        geom_segment(
            aes(
                x = .data[[input$dumbbell_change[1]]],
                xend = .data[[input$dumbbell_change[2]]],
                yend = .data[[id_varsP()[input$dumbbell_is]]],
                color = .data[[input$dumbbell_change[2]]] > .data[[input$dumbbell_change[1]]]
            )
        ) +
        geom_point(
            aes(x = .data[[input$dumbbell_change[1]]]),
            size = 3
            
        ) +
        geom_point(
            aes(x = .data[[input$dumbbell_change[2]]],
                # shape = paste0(.data[[input$dumbbell_ex]]),
                color = .data[[input$dumbbell_change[2]]] > .data[[input$dumbbell_change[1]]]),
            size = 3
        ) +
        theme(
            axis.ticks = element_blank(),
            panel.border = element_blank()
        ) +
        labs(
            x = NULL, 
            y = NULL, 
            title = "Dumbbell Chart",
            shape = NULL
        ) +
        scale_x_continuous(labels = comma) +
        scale_color_brewer(palette = "Set1", na.value = NA) +
        guides(color = FALSE)  
    ggplotly(p, tooltip = "text") 
})

# box plot ----------------------------------------------------------------

output$box_var <- renderUI({
    selectInput("box_var",
        "choose variables", choices = var_names()
    )
})
output$box_is <- renderUI({
    selectInput("box_is",
        "choose condition", choices = is_vars()
    )
})
box_plot <- reactive({
    req(input$box_is, input$box_var)
    p <- ggplot(
        data() |>
            pivot_longer(
                all_of(input$box_var),
                names_to = "vars",
                values_to = "values"
            ),
        aes(vars, values, fill = .data[[input$box_is]])
    ) +
        geom_boxplot() + 
        geom_jitter() +
        scale_y_continuous(labels = comma)
    print(p)
})

downloadablePlot("box", 
    logger = ss_userAction.Log,
    filenameroot = "box_plot1",
    downloadfxns = list(png = box_plot),
    visibleplot = box_plot
)