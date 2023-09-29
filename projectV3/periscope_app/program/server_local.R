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
        "choose variables (>2)", choices = var_names()
    )
})

ridges_var <- reactive({
    input$ridges_var
}) |> 
    debounce(1000)

ridges_height <- reactive({
    ifelse(length(ridges_var()) > 4, length(ridges_var())*150, 600)
})
output$ridges <- renderPlot(res = 144, {
    req(length(ridges_var()) > 1)
    ggplot(
        data() |>
            pivot_longer(all_of(ridges_var()), names_to = "vars", values_to = "values"),
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
        scale_x_continuous(labels = comma) +
        theme(
            legend.position = "top",
            legend.direction = "horizontal"
        )
})
        
output$ridges_ui <- renderUI({
    box(
        width = 8, status = "primary",
        withSpinner(
            plotOutput("ridges", height = ridges_height())
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

output$corr <- renderPlot(res = 144, {
    req(input$cor_is, input$cor_group, data())
    data_cor <- cor(
        data() |>
            filter(.data[[input$cor_is]] == input$cor_group) |> 
            select(all_of(var_names())) |> 
            drop_na()
    ) |> 
        round(2) 
    p <- ggcorrplot::ggcorrplot(
            data_cor,
            type = "lower", outline.color = "white",
            color = c("#9C0824", "white", "#26456E")
        ) 
        # corrplot::corrplot(
        # data_cor,
        # method = "color", order = 'alphabet', type = "lower",
        # diag = FALSE, tl.srt = 45, tl.cex = 6/length(var_names()) + 0.5
        # )
    print(p)
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
        guides(color = "none")  
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

# PLSDA -------------------------------------------------------------------
hideElement("plsda_perf_start")
hideElement("plsda_download")

output$plsda_n <- renderUI({
    req(var_names())
    numericInput("plsda_n",
        "choose number of component",
         min = 2, max = length(var_names()), value = length(var_names())
    )
})
output$plsda_is <- renderUI({
    req(is_vars())
    selectInput("plsda_is",
        "choose condition", choices = is_vars()
    )
})
output$plsda_score_n <- renderUI({
    req(input$plsda_n)
    pickerInput("plsda_score_n", multiple = TRUE,
        label = "Select which componemt to show",
        choices = 1:input$plsda_n,
        options = list(
            `actions-box` = TRUE,
            `selected-text-format` = "count > 2"
        ),
        selected = c(1, 2)
    )
})


output$plsda_download <- downloadHandler(
    contentType = "image/png",
    filename = function() {
        "plsda_score.png"
    },
    content = function(file) {
        ggsave(file, plot = plsda_score_plot(), device = "png")
    }
)

plsda <- eventReactive(input$plsda_start, {
    waiter_show()
    plsda_X <- data() |>
        column_to_rownames(filename()) |>
        select(all_of(var_names()))
    plsda_Y <- data() |>
        pull(input$plsda_is) |>
        factor()
    plsda <- mixOmics::plsda(plsda_X, plsda_Y, ncomp = input$plsda_n)
    waiter_hide()
    plsda
})
observeEvent(input$plsda_perf_start, {
    if (input$plsda_perf_box$collapsed) {
        updateBox("plsda_perf_box", action = "toggle")
    }
})
plsda_perf <- eventReactive(input$plsda_perf_start, {
    req(plsda())
    waiter_show()
    plsda_perf <- perf(
        plsda(), validation = 'Mfold', folds = 3, 
        progressBar = FALSE, 
        nrepeat = 10
    )   
    overall <- plsda_perf$error.rate$overall |> 
        as.data.frame() |> 
        mutate("class" = "overall", "component" = 1:input$plsda_n)
    
    BER <- plsda_perf$error.rate$BER |> 
        as.data.frame() |> 
        mutate("class" = "BER", "component" = 1:input$plsda_n) 
    
    sd <- as.data.frame(plsda_perf$error.rate.sd$overall) |> 
        bind_rows(as.data.frame(plsda_perf$error.rate.sd$BER)) |> 
        pivot_longer(cols = 1:3, names_to = "dist", values_to = "sd")
    
    perf_data <- bind_rows(overall, BER) |> 
        pivot_longer(cols = 1:3, names_to = "dist", values_to = "value") |> 
        bind_cols(sd[,2])
    waiter_hide()
    perf_data
})
plsda_score_n <- reactive({
    input$plsda_score_n
}) |> 
    debounce(1000)


plsda_score_plot <- reactive({
    req(plsda(), plsda_score_n())
    final_data <- plsda()$variates$X |> 
        bind_cols(
            condition = data() |>
                pull(input$plsda_is) |>
                factor()
        )
    p <- ggplot(final_data, aes(color = condition)) +
        geom_autopoint(size = 0.1) +
        geom_autodensity(alpha = .3) +
        stat_ellipse(aes(x = .panel_x, y = .panel_y)) +
        facet_matrix(
            rows = vars(as.numeric(plsda_score_n())),
            layer.diag = 2
        ) +
        theme(
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size = 12)
        )
    print(p)
})

output$plsda_score <- renderPlot(res = 144, {
    req(length(input$plsda_score_n) > 1)
    plsda_score_plot()
})

output$plsda_perf <- renderPlotly({
    req(plsda_perf())
    p <- ggplot(
        plsda_perf(),
        aes(
            x = component, y = value,
            group = interaction(dist, class), color = dist)
    ) +
        geom_point() +
        geom_path(aes(linetype = class)) +
        scale_x_continuous(breaks = 1:isolate(input$plsda_n)) +
        geom_errorbar(aes(ymin = value - sd, ymax = value + sd)) +
        ylab("Classification erro rate")
    ggplotly(p)
})

observeEvent(plsda(), {
    showElement("plsda_perf_start")  
    showElement("plsda_download")    
})
    

# sPLSDA ------------------------------------------------------------------
output$splsda_is <- renderUI({
    selectInput("splsda_is",
        "choose condition", choices = is_vars()
    )
})

#  * tuning ---------------------------------------------------------------
## store tune object
splsda_tune <- eventReactive(input$splsda_tune, {
    req(data())
    waiter_show()
    X <- data() |>
        column_to_rownames(filename()) |>
        select(all_of(var_names()))
    Y <- data() |>
        pull(input$splsda_is) |>
        factor()
    splsda_tune <- tune.splsda(
        X, Y, ncomp = 5, validation = 'Mfold',
        folds = 5, dist = 'max.dist',
        test.keepX = c(1:10, seq(20, 100, 10)),
        nrepeat = 10
    )
    waiter_hide()
    splsda_tune
})
## output keepx after tuning 
output$splsda_keepx <- renderUI({
    descriptionBlock(
        header = splsda_tune()$choice.keepX[1:input$splsda_ncomp] |> paste0(collapse = ", "),
        text = "keepX used after tuning"
    )
})
## update input after tuning
observeEvent(splsda_tune(), {
    updateSelectInput(session, "splsda_keep",
        choices = c("default", "tuned"),
        selected = "tuned"
    )
    updateNumericInput(session, "splsda_ncomp",
        value = splsda_tune()$choice.ncomp$ncomp, max = 5
    )
})
## store splsda object
splsda_var <- eventReactive(input$splsda_start, {
    req(data())
    waiter_show()
    X <- data() |>
        column_to_rownames(filename()) |>
        select(all_of(var_names()))
    Y <- data() |>
        pull(input$splsda_is) |>
        factor()
    if (input$splsda_keep == "default") {
        splsda <- splsda(X, Y, ncomp = input$splsda_ncomp)
    }
    else {
        splsda <- splsda(
            X, Y, ncomp = input$splsda_ncomp,
            keepX = splsda_tune()$choice.keepX[1:input$splsda_ncomp]
        )
    }
    waiter_hide()
    variates <- as_tibble(splsda$variates$X)
    loading <- splsda$loadings$X
    list(
        "splsda"= splsda, "X" = X, "Y" = Y,
        "variates" = variates, "loading" = loading
    )
})


#  * overview -------------------------------------------------------------

output$splsda_overview <- renderPlot(res = 144, {
    p <- ggplot(
        splsda_var()[["variates"]] |>
            bind_cols(condition = splsda_var()[["Y"]]),
        aes(color = condition)
    ) +
        geom_autopoint(size = 0.1) +
        geom_autodensity(alpha = .3) +
        stat_ellipse(aes(x = .panel_x, y = .panel_y)) +
        facet_matrix(rows = vars(1:isolate(input$splsda_ncomp)), layer.diag = 2) +
        theme(
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size = 12)
        )
    print(p)
})


#  * score -------------------------------------------------------------------
output$splsda_score_n <- renderUI({
    req(input$splsda_ncomp)
    selectizeInput("splsda_score_n",
        "choose component to show",
        choices = paste0("comp", 1:input$splsda_ncomp),
        multiple = TRUE, options = list(maxItems = 2)
    )
})

output$splsda_score <- renderPlot(res = 144, {
    req(length(input$splsda_score_n) == 2)
    p <- ggplot(
        splsda_var()[["variates"]],
        aes(
            x = .data[[input$splsda_score_n[1]]],
            y = .data[[input$splsda_score_n[2]]],
            color = splsda_var()[["Y"]])
    ) +
        geom_point() +
        stat_ellipse() +
        stat_stars() +
        labs(color = isolate(input$splsda_is))
    print(p)
})

#  * loading --------------------------------------------------------------

output$splsda_loading_n <- renderUI({
    selectInput("splsda_loading_n",
        "choose component to show",
        choices = paste0("comp", 1:input$splsda_ncomp)
    )
})

splsda_loading_height <- reactive({
    if (splsda_loading_data() |> pull(var) |> length() < 7) {
        return(600)
    }else {
        return(splsda_loading_data() |> pull(var) |> length()*80)
    }
})

splsda_loading_data <- eventReactive(input$splsda_loading_refresh,
    ignoreNULL = FALSE, ignoreInit = FALSE, {
    req(input$splsda_loading_n, splsda_var(), input$splsda_loading_valve)
    data <- splsda_var()[["loading"]] |>
        data.frame() |>
        rownames_to_column("var") |> 
        filter(abs(.data[[input$splsda_loading_n]]) > input$splsda_loading_valve)
    empty <- nrow(data) == 0
    feedbackWarning(
         "splsda_loading_valve", empty, "Lower the threshold"
    )
    data
})

output$splsda_loading <- renderPlot(res = 144, {
        p <- ggplot(
            splsda_loading_data(),
            aes(
                .data[[isolate(input$splsda_loading_n)]],
                reorder(var, abs(.data[[isolate(input$splsda_loading_n)]])))
        ) +
            geom_bar(
                aes(fill = ifelse(.data[[isolate(input$splsda_loading_n)]] > 0, "pink", "lightblue")),
                stat = "identity", show.legend = FALSE
            ) +
            geom_text(
                aes(
                    label = round(.data[[isolate(input$splsda_loading_n)]], 2),
                    hjust = ifelse(.data[[isolate(input$splsda_loading_n)]] < 0, 1.25, -0.25),
                    vjust = 0.5
                ),
                size = 5
            ) +
            xlab("Loading") +
            ylab("Group") +
            scale_x_continuous(limits = c(-1, 1))
        print(p)
    })

output$splsda_loading_ui <- renderUI({
    withSpinner(
        plotOutput("splsda_loading", height = splsda_loading_height())
    )
})


#  * corr -----------------------------------------------------------------
output$splsda_cor_n <- renderUI({
    selectizeInput("splsda_cor_n",
        "choose component to show",
        multiple = TRUE, options = list(maxItems = 2),
        choices = setNames(1:input$splsda_ncomp, paste0("comp", 1:input$splsda_ncomp))
    )
})

output$splsda_cor_vip <- renderUI({
    req(var_names())
    numericInput("splsda_cor_vip",
        "show by importance",
        value = length(var_names()), step = 1, min = 1, max = length(var_names())
    )
})
splsda_cor_vip <- reactive({
    req(input$splsda_cor_n, input$splsda_cor_n)
    vip <- vip(splsda_var()[["splsda"]])[ ,as.numeric(input$splsda_cor_n)] |> 
        rowSums()
    cor <- plotVar(
        splsda_var()[["splsda"]],
        comp = as.numeric(input$splsda_cor_n),
        plot = FALSE
    )
    vip[row.names(cor)]
})
splsda_cor_data <- reactive({
    req(input$splsda_cor_n)
    plotVar(
        splsda_var()[["splsda"]],
        comp = as.numeric(input$splsda_cor_n),
        plot = FALSE
    ) |> 
        add_column("importance" = splsda_cor_vip()) |> 
        arrange(desc(importance)) |> 
        slice_head(n = input$splsda_cor_vip)
}) |> 
    debounce(500)

output$test <- renderPrint({
    c(splsda_cor_vip())
})
output$splsda_cor <- renderPlot(res = 144, {
    req(length(input$splsda_cor_n) == 2, splsda_cor_data())
    p <- ggplot(splsda_cor_data(), aes(x, y)) +
        geom_circle(aes(x0 = 0, y0 = 0, r = 0.5)) +
        geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
        geom_point(aes(color = sqrt(x^2 + y^2)), size = 4) +
        ggrepel::geom_text_repel(aes(label = names), box.padding = 0.5, max.overlaps = Inf) +
        scale_x_continuous(limits = c(-1, 1)) +
        scale_y_continuous(limits = c(-1, 1)) +
        paletteer::scale_color_paletteer_c("grDevices::Oslo", limits = c(0, 1)) +
        labs(title = "Correlation Circle Plot", x = "comp1", y = "comp2") +
        guides(color = "none")
    print(p)
})


#  * performance ----------------------------------------------------------

output$splsda_perf_n <- renderUI({
    selectInput("splsda_perf_n", 
        "choose component to show",
        choices = paste0("comp", 1:input$splsda_ncomp)
    )
})
splsda_perf <- eventReactive(input$splsda_perf_start, {
    req(splsda_var())
    waiter_show()
    splsda_perf <- perf(
        splsda_var()[["splsda"]], folds = 5, validation = "Mfold", 
        dist = "max.dist", progressBar = FALSE, nrepeat = 10
    )
    waiter_hide()
    splsda_perf
})

output$splsda_perf_er <- renderPrint({
    splsda_perf()$error.rate.class 
})

output$splsda_perf <- renderPlot(res = 144, {
    req(splsda_perf())
    ggplot(
        splsda_perf()$features$stable[[input$splsda_perf_n]] |> 
            as.data.frame(),
        aes(x = Freq, y = reorder(Var1, Freq))
    ) +
        geom_col(fill = "lightblue") +
        labs(
            title = "Feature stability",
            x = "variables selected across CV folds",
            y = "Stability frequency"
        ) 
})


# SOM ---------------------------------------------------------------------


#  * SOM object -----------------------------------------------------------

output$som_is <- renderUI({
    selectInput("som_is",
        "choose condition", choices = is_vars()            
    )
})

som <- eventReactive(input$som_start, {
    req(data())
    waiter_show()
    data_som <- data() |> 
        select(all_of(var_names())) |> 
        as.matrix()
    grid_size <- ceiling(nrow(data_som) ^ (1/2.5))
    som <- data_som |>
        scale() |>
        kohonen::som(
            grid = somgrid(
                grid_size, grid_size,
                topo = "hexagonal", neighbourhood.fct = "gaussian"),
            rlen = as.numeric(input$som_rlen)
        )
    som_grid <- som[[4]]$pts |>
        as_tibble() |>
        mutate(id = row_number())
    som_pts <- tibble(
        id = som[[2]],
        dist = som[[3]],
        condition = drop_na(data()) |> pull(input$som_is)
    ) |>
        left_join(som_grid, by = "id")
    
    ndist <- unit.distances(som$grid)
    cddist <- as.matrix(object.distances(som, type = "codes"))
    cddist[abs(ndist - 1) > .001] <- NA
    neigh.dists <- colMeans(cddist, na.rm = TRUE)
    
    som_grid <- som_grid |>
        mutate(dist = neigh.dists)
    
    # som_code <- som_grid |> 
    #     bind_cols(getCodes(som))
    
    waiter_hide()
    list(
        "som" = som, "som_grid" = som_grid, "som_pts" = som_pts
    )
})


#  * mapping --------------------------------------------------------------

output$som_map <- renderPlot(res = 108, {
    p <- ggplot(som()$som_grid, aes(x0 = x,y0 = y))+
        geom_circle(aes(r = 0.5)) +
        theme(
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom"
        ) +
        geom_jitter(
            data = som()$som_pts,
            aes(x, y, col = condition),
            alpha = 0.5, size = 3
        )
    print(p)
})

#  * pie ------------------------------------------------------------------

output$som_pie <- renderPlot(res = 108, {
    pts <- som()$som_pts |> 
        group_by(id, x, y) |> 
        count(condition) |> 
        ungroup() |> 
        dplyr::filter(!is.na(condition))
    
    p <- ggplot(som()$som_grid, aes(x0 = x,y0 = y))+
        geom_circle(aes(r = 0.5)) +
        theme(
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom"
        ) + 
        geom_arc_bar(
            data = pts,
            aes(
                x0 = x, y0 = y, r0 = 0, r = 0.5,
                amount = n, fill = condition
            ),
            stat = 'pie'
        )
    print(p)
})


#  * dist -----------------------------------------------------------------

output$som_dist <- renderPlot(res = 108, {
    p <- ggplot(som()$som_grid, aes(x0 = x,y0 = y)) +
        geom_regon(
            aes(sides = 6, angle = pi/2, r = 0.58, fill = dist),
            color = "black"
        ) +
        theme(
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom"
        ) +
        scale_fill_viridis_c() 
    print(p)
})

#  * changes --------------------------------------------------------------

output$som_changes <- renderPlot(res = 108, {
    data_changes <- tibble(
        x = 1:as.numeric(isolate(input$som_rlen)),
        y = som()$som[["changes"]]
    )
    p <- ggplot(data_changes, aes(x = x, y = y)) +
        geom_line() +
        labs(
            x = "iteration", y = "Mean distance to closest unit"
        )
    print(p)
})   

#  * count ----------------------------------------------------------------

output$som_count <- renderPlot(res = 108, {
    som_count <- som()$som_pts |> 
        group_by(x, y) |> 
        count(id, .drop = FALSE)
    
    ggplot(som_count, aes(x0 = x, y0 = y)) +
        geom_regon(
            aes(sides = 6, angle = pi/2, r = 0.58, fill = n),
            color = "black"
        ) +
        theme(
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom"
        ) +
        scale_fill_paletteer_c("grDevices::Heat", -1)
})

#  * codes ----------------------------------------------------------------


output$som_codes <- renderPlot(res = 108, {
    plot(som()$som, type = "codes", main = "")
})

#  * cluster --------------------------------------------------------------

output$som_elbow <- renderPlot(res = 108, {
    factoextra::fviz_nbclust(getCodes(som()$som), kmeans, method = "wss")
})

output$som_cluster <- renderPlot(res = 108, {
    set.seed(111)
    req(som(), input$som_cluster_n)
    clust <- kmeans(getCodes(som()$som), input$som_cluster_n)
    data_cluster <- som()$som_grid |>
        bind_cols("cluster" = clust[["cluster"]])
    
    p <- ggplot(data_cluster, aes(x0 = x, y0 = y)) +
        geom_regon(
            aes(sides = 6, angle = pi/2, r = 0.58, fill = factor(cluster)),
            color = "black", alpha = 0.5
        ) +
        geom_jitter(
            data = som()$som_pts,
            aes(x, y, col = condition),
            alpha = 0.5, size = 3
        ) +
        theme(
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "none"
        ) 
        scale_fill_paletteer_d("ggsci::category20_d3")
    print(p)
})

# heat map ----------------------------------------------------------------

output$som_heatmaps_ui <- renderUI({
    ui <- tagList(
        tags$div(
            id = "som_heatmaps",
            lapply(var_names(), function(i) {
                box(
                    width = 4, title = paste("Heatmap of ", i),
                    plotOutput(paste0("som_heatmap", i))
                )
            }),
            sortable_js("som_heatmaps")
        )
    )
    ui
})

# 在這裡添加圖表生成的代碼
observeEvent(input$som_start, {
    data_code <- som()$som_grid |> 
        bind_cols(getCodes(som()$som))
    lapply(var_names(), function(i) {
        output[[paste0("som_heatmap", i)]] <- renderPlot({
            ggplot(data_code, aes(x0 = x, y0 = y)) +
                geom_regon(
                    aes(
                        sides = 6, angle = pi/2, r = 0.58,
                        fill = .data[[i]], 
                    ),
                    color = "black"
                ) +
                theme(
                    panel.background = element_blank(),
                    axis.ticks = element_blank(),
                    panel.grid = element_blank(),
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    legend.position = "bottom",
                    legend.title = element_blank()
                ) +
                scale_fill_viridis_c(option = "magma") 
        })
    })    
})
