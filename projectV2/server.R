# V2
library(shiny)
library(shinyjs) 
library(tidyverse)
library(scales)
library(openxlsx)
library(plotly)
library(mixOmics)
library(ggforce)
library(ggpubr)
library(kohonen)
library(paletteer)
tidymodels::tidymodels_prefer()

function(input, output, session) {
    thematic::thematic_shiny()
# file input --------------------------------------------------------------
# * read xlsx ---------------------------------------------------------------
    sheet_names <- reactive({
        req(input$file)
        getSheetNames(input$file$datapath)
    })
    file_list <- reactive({
        req(input$file)
        setNames(
            lapply(
                sheet_names(),
                read.xlsx, xlsxFile = input$file$datapath, sep.names = " "),
            sheet_names()
        )
    })
    ## update 
    observeEvent(input$file,
        updateSelectInput(session, "sheet", choices = sheet_names()[-1])
    )
    
# * sheet input -------------------------------------------------
    info_sheet <- reactive({
        req(input$sheet)
        file_list()[[input$sheet]] 
    })
    abundance <- reactive({
        req(file_list())
        file_list()[[1]] 
    })
    
    is_vars <- reactive({
        str_subset(names(info_sheet()), "_id", negate = TRUE)[-1]
    })
    
    id_vars <- reactive({
        str_subset(names(info_sheet()), "_id")
    })
    

    var_names <- reactive(names(abundance())[-1])
    filename <- reactive(names(abundance())[1])
# * data input --------------------------------------------------------------
    by <- reactive({
        req(info_sheet(), abundance())
        setNames(names(info_sheet())[1], names(abundance())[1])
    })

    data <- reactive({
        req(file_list(), by())
        abundance() |>
            right_join(info_sheet(), by = by()) |> 
            mutate(across(ends_with("_id"), ~factor(.x)))
    })
    
# info card ---------------------------------------------------------------
    
    output$info_miss <- renderText({
        data()[!apply(data() |> is.na(), 1, all),] |> 
            is.na() |>
            sum() |> 
            paste0("個缺失值")
    })
    output$info_id <- renderText({
        c("id 名稱: ", paste0(id_vars(), collapse = ", "))
    })
    output$info_is <- renderText({
        c("組別名稱: ", paste0(is_vars(), collapse = ", "))
    })
    output$info_var <- renderText(
        c("變數名稱: ", paste0(var_names(), collapse = ", "))
    )
    output$group_sum <- renderTable({
        req(input$info_group)
        summarise(data(), n(), .by = input$info_group)
    })
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
    ## update
    observeEvent(info_sheet(),
        updateCheckboxGroupInput(session, "info_group", choices = is_vars())
    )
    observeEvent(is_vars(), {
        updateSelectInput(session, "t_test_is", choices = is_vars())
    })
    observeEvent(var_names(), {
        updateSelectInput(session, "t_test_var", choices = var_names())
    })
    observeEvent(id_vars(), {
        updateSelectInput(session, "t_test_id", choices = id_vars())
    })
    observeEvent(input$t_test_is, {
        updateSelectInput(session, "t_test_change",
            choices = data() |> 
                pull(input$t_test_is) |> 
                unique()
        )
    })
    

# slope plot ------------------------------------------------------------

    output$slope <- renderPlotly({
        req(data(), input$slope_change)
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
    
    ## update
    observeEvent(is_vars(), {
        updateSelectInput(session, "slope_is", choices = is_vars())    
    })
    observeEvent(var_names(), {
        updateSelectInput(session, "slope_var", choices = var_names())
    })
    observeEvent(id_vars(), {
        updateSelectInput(session, "slope_id", choices = id_vars())
    })
    observeEvent(input$slope_is, {
        updateSelectInput(session, "slope_change",
                          choices = data() |> 
                              pull(input$slope_is) |> 
                              unique()
        )
    })

# density plot ------------------------------------------------------------

    output$density <- renderPlotly({
        req(input$density_var)
        p <- ggplot(data() ,
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
                    orientation = 'h',
                    x = 0,
                    y = 1,
                    font = list(size = 20)
                )
            )
    })
    
    ## update
    observeEvent(data(), {
        updateSelectInput(session, "density_var", choices = var_names())
    })
    observeEvent(data(), {
        updateSelectInput(session, "density_group", choices = is_vars())
    })
    observeEvent(input$density_var, {
        updateSliderInput(session, "density_bw",
            max = bw.nrd(data() |> drop_na() |> pull(input$density_var)),
            min = bw.nrd(data() |> drop_na() |> pull(input$density_var)) / 100,
            value = bw.nrd(data() |> drop_na() |> pull(input$density_var))
        )
    })

# Ridgeline plots ---------------------------------------------------------
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
    ## update
    observeEvent(var_names(),
        updateCheckboxGroupInput(session, "ridges_var", choices = var_names())
    )
# correlation plot --------------------------------------------------------
    output$cor <- renderPlot(height = 600, res = 144, {
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
    ## update
    observeEvent(is_vars(), {
        updateSelectInput(session, "cor_is", choices = is_vars())
    })
    observeEvent(input$cor_is, {
        updateSelectInput(session, "cor_group",
            choices = data() |> 
                pull(input$cor_is) |> 
                unique()
        )
    })

# dumbbell plot ------------------------------------------------------------
    output$dumbbell <- renderPlotly({
        req(input$dumbbell_is, input$dumbbell_var, input$dumbbell_id)
        
        dumbbell_data <- data() |>
            pivot_wider(
                names_from = input$dumbbell_is,
                values_from = input$dumbbell_var,
                id_cols = input$dumbbell_id,
                unused_fn = list
            )
        
        if (length(input$dumbbell_change) == 2) {
            p <- ggplot(dumbbell_data,
                aes(y = reorder(
                    .data[[input$dumbbell_id]],
                    .data[[input$dumbbell_change[1]]]
                    ),
                    text = .data[[input$dumbbell_id]])
            ) +
                geom_segment(
                    aes(
                        x = .data[[input$dumbbell_change[1]]],
                        xend = .data[[input$dumbbell_change[2]]],
                        yend = .data[[input$dumbbell_id]],
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
        }
        
    })
    # update
    observeEvent(is_vars(), {
        updateSelectInput(session, "dumbbell_is", choices = is_vars())
    })
    observeEvent(var_names(), {
        updateSelectInput(session, "dumbbell_var", choices = var_names())
    })
    observeEvent(id_vars(), {
        updateSelectInput(session, "dumbbell_id", choices = id_vars())
    })
    observeEvent(input$dumbbell_is, {
        updateSelectInput(session, "dumbbell_change",
            choices = data() |> 
                pull(input$dumbbell_is) |> 
                unique()
        )
    })
    observeEvent(is_vars(), {
        updateSelectInput(session, "dumbbell_ex",
                          choices = c(is_vars(), NULL)
        )
    })

# box plot ----------------------------------------------------------------
    output$box <- renderPlot(height = 600, res = 144, {
        req(input$box_var)
        ggplot(
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
    })   
    ## update
    observeEvent(var_names(),
        updateSelectInput(session, "box_var", choices = var_names()) 
    )
    observeEvent(is_vars(),
        updateSelectInput(session, "box_is", choices = is_vars()) 
    )

# PLSDA -------------------------------------------------------------------
    hideElement("plsda_perf_start")
    plsda <- eventReactive(input$plsda_start, {
        req(data())
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
    output$plsda_perf <- renderPlotly({
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
    output$plsda_score <- renderPlot(height = 600, res = 144, {
        req(length(input$plsda_score_n) > 1)
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
                rows = vars(as.numeric(input$plsda_score_n)),
                layer.diag = 2
            ) +
            theme(
                strip.background = element_blank(),
                strip.placement = "outside",
                strip.text = element_text(size = 12)
            )
        print(p)
    })
    
    ## update
    observeEvent(var_names(), {
        updateNumericInput(session, "plsda_n",
            max = length(var_names()), value = length(var_names())                  
        )
    })
    observeEvent(is_vars(),
        updateSelectInput(session, "plsda_is", choices = is_vars())
    )
    observeEvent(plsda(),
        showElement("plsda_perf_start")    
    )
    observeEvent(input$plsda_n,
        updatePickerInput(session, "plsda_score_n", choices = 1:input$plsda_n)             
    )

# sPLSDA ------------------------------------------------------------------

# * tuning ----------------------------------------------------------------
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
    output$splsda_keepx <- renderText(
        splsda_tune()$choice.keepX[1:input$splsda_ncomp]
    )
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

# * overview --------------------------------------------------------------

    output$splsda_overview <- renderPlot(height = 600, res = 144, {
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

#  * score ----------------------------------------------------------------

    output$splsda_score <- renderPlot(height = 600, res = 144, {
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
    splsda_loading_data <- reactive({
        data <- splsda_var()[["loading"]] |>
            data.frame() |>
            rownames_to_column("var") |> 
            filter(abs(.data[[input$splsda_loading_n]]) > input$splsda_loading_valve)
        empty <- nrow(data) == 0
        shinyFeedback::feedbackWarning(
            "splsda_loading_valve", empty, "Lower the threshold"
        )
        data
    })
    splsda_loading_height <- reactive({
        if (splsda_loading_data() |> pull(var) |> length() < 7) {
            return(600)
        }else {
            return(splsda_loading_data() |> pull(var) |> length()*80)
        }
    })
    output$splsda_loading <- renderPlot(
        height = function() splsda_loading_height(), res = 144, {
        req(splsda_loading_data())
        p <- ggplot(
            splsda_loading_data(),
            aes(
                .data[[input$splsda_loading_n]],
                reorder(var, abs(.data[[input$splsda_loading_n]])))
            ) +
            geom_bar(
                aes(fill = ifelse(.data[[input$splsda_loading_n]] > 0, "pink", "lightblue")),
                stat = "identity", show.legend = FALSE
            ) +
            geom_text(
                aes(
                    label = round(.data[[input$splsda_loading_n]], 2),
                    hjust = ifelse(.data[[input$splsda_loading_n]] < 0, 1.25, -0.25),
                    vjust = 0.5
                ),
                size = 5
            ) +
            xlab("Loading") +
            ylab("Group") +
            scale_x_continuous(limits = c(-1, 1))
        print(p)
    })

#  * cor ------------------------------------------------------------------
    splsda_cor_vip <- reactive({
        vip(splsda_var()[["splsda"]])[ ,as.numeric(input$splsda_cor_n)] |> 
        rowSums()
    })
    splsda_cor_data <- reactive({
        plotVar(
            splsda_var()[["splsda"]],
            comp = as.numeric(input$splsda_cor_n),
            plot = FALSE
        ) |> 
            add_column("importance" = rank(desc(splsda_cor_vip()))) |> 
            filter(importance <= input$splsda_cor_vip)
    })    
    output$splsda_cor <- renderPlot(height = 800, res = 144, {
        req(length(input$splsda_cor_n) == 2, splsda_cor_data())
        # "#8B7E66"
        ggplot(splsda_cor_data(), aes(x, y)) +
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
    })
    
#  * performance ----------------------------------------------------------
    splsda_perf <- eventReactive(input$splsda_perf_start, {
        req(splsda_var())
        waiter_show()
        splsda_perf <- perf(
            splsda_var()[["splsda"]], folds = 5, validation = "Mfold", 
            dist = "max.dist", progressBar = FALSE, nrepeat = 50
        )
        waiter_hide()
        splsda_perf
    })
    output$splsda_perf_er <- renderPrint({
        splsda_perf()$error.rate.class 
    })
    output$splsda_perf <- renderPlot(height = 600, res = 144, {
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

#  * * update ---------------------------------------------------------------

    observeEvent(is_vars(),
        updateSelectInput(session, "splsda_is", choices = is_vars())
    )
    observeEvent(splsda_tune(), {
        updateSelectInput(session, "splsda_keep",
            choices = c("default", "tuned"),
            selected = "tuned"
        )
        updateNumericInput(session, "splsda_ncomp",
            value = splsda_tune()$choice.ncomp$ncomp
        )
    })
    observeEvent(input$splsda_start, {
        updateSelectizeInput(session, "splsda_score_n",
            choices = paste0("comp", 1:input$splsda_ncomp)
        )
        updateSelectInput(session, "splsda_loading_n",
            choices = paste0("comp", 1:input$splsda_ncomp)
        )
        updateSelectInput(session, "splsda_cor_n",
            choices = setNames(1:input$splsda_ncomp, paste0("comp", 1:input$splsda_ncomp))
        )
        updateSelectInput(session, "splsda_perf_n",
            choices = paste0("comp", 1:input$splsda_ncomp)
        )
        
    })
    observeEvent(var_names(), {
        updateNumericInput(session, "splsda_cor_vip",
            max = length(var_names())                   
        )
    })
    

# SOM ---------------------------------------------------------------------
    som <- eventReactive(input$som_start, {
        req(data())
        # waiter_show()
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
        
        som_code <- som_grid |> 
            bind_cols(getCodes(som))
        
        waiter_hide()
        list(
            "som" = som, "som_grid" = som_grid, "som_pts" = som_pts,
            "som_code" = som_code
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
            ) +
            labs(title = "Mapping plot")
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
            ) +
            labs(title = "Fraction of each type")
        print(p)
    })

#  * dist -----------------------------------------------------------------

    output$som_dist <- renderPlot(res = 108, {
        p <- ggplot(som()$som_grid, aes(x0 = x,y0 = y)) +
            geom_circle(aes(r = 0.5, fill = dist)) +
            theme(
                panel.background = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                legend.position = "bottom"
            ) +
            scale_fill_viridis_c() +
            # scale_fill_paletteer_c("grDevices::Blues", -1) +
            labs(title = "Neighbour distance plot")
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
                x = "iteration", y = "Mean distance to closest unit",
                title = "Training progress"
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
            labs(title = "Counts plot") +
            scale_fill_paletteer_c("grDevices::Heat", -1)
    })

#  * heatmap --------------------------------------------------------------
    output$dynamic_plots <- renderUI({
        num_plots <- input$num_plots
        
        # 生成一個plotOutput列表
        plot_outputs <- lapply(1:num_plots, function(i) {
            wellPanel(
                plotOutput(paste0("plot", i))
            )
            
        })
        
        # 返回plotOutput列表
        do.call(tagList, plot_outputs)
    })
    
    # 在這裡添加圖表生成的代碼
    observeEvent(input$num_plots, {
        lapply(1:input$num_plots, function(i) {
            output[[paste0("plot", i)]] <- renderPlot({
                # 這裡添加每個圖表的生成代碼
                x <- rnorm(100)
                y <- rnorm(100)
                title <- paste("Plot", i)
                plot(x, y, main = title)
            })
        })    
    })
    
      

    
    
    output$som_heat <- renderPlot({
        ggplot(som()$som_code, aes(x0 = x, y0 = y)) +
            geom_regon(
                aes(
                    sides = 6, angle = pi/2, r = 0.58,
                    fill = .data[[input$som_var]]
                ),
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
            scale_fill_viridis_c(option = "magma") +
            labs(title = paste("Heatmap of", input$som_var))
    })
    
#  * * update -------------------------------------------------------------
    observeEvent(is_vars(),
        updateSelectInput(session, "som_is", choices = is_vars())
    )
    observeEvent(var_names(),
        updateSelectInput(session, "som_var", choices = var_names())
    )
}




