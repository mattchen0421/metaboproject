library(shiny)
library(tidyverse)
library(scales)
function(input, output, session) {
    thematic::thematic_shiny()
    #--- data import
    data_org <- reactive({
        req(input$file)
        readxl::read_excel(input$file$datapath)
    })
    var_names <- reactive(names(data_org()[-1]))
    
    observeEvent(input$file,
                 updateSelectInput(
                     session, "groups",
                     choices = str_extract(drop_na(data_org())$FileName, "[:alpha:]+") |> 
                         unique() 
                 )
    )
    
    data_qc <- reactive({
        req(input$qc)
        data_org() |> 
            filter(str_detect(FileName, input$qc))
    })
    observeEvent(input$file,
                 updateSelectInput(
                     session, "qc",
                     choices = str_extract(drop_na(data_org())$FileName, "[:alpha:]+") |> 
                         unique() 
                 )
    )
    
    
    data <- reactive({
        req(input$file, input$qc, input$groups)
        data_org() |>
            mutate(
                group = str_extract(FileName, paste0(input$groups, collapse = "|")),
                id = str_extract(FileName, "[:digit:]+")
            ) |> 
            filter(!str_detect(FileName, input$qc)) |> 
            drop_na()
    })
    
    data_long <- reactive(
        data() |> 
            pivot_longer(cols = 2:10, names_to = "type", values_to = "value") 
    )
    
    data_wide <- reactive(
        data() |> 
            select(-FileName) |> 
            pivot_wider(names_from = group, values_from = all_of(var_names()))
    )
    
    output$download <- downloadHandler(
        filename = function() {
            "example_data.xlsx"
        },
        content = function(file) {
            readxl::read_excel("IgQuantResult_4Std.xlsx") |> 
            readr::write_excel_csv(file)
        }
    )
  
    #--- information
    output$info_empty_row <- renderText(
        apply(is.na(data_org()), 1, all) |> 
            sum() |> 
            paste0("行空行")
        )
    output$info_miss <- renderText({
        data_org()[!apply(data_org() |> is.na(), 1, all),] |> 
            is.na() |>
            sum() |> 
            paste0("個缺失值")
    })
    output$info_qc <- renderText(
        nrow(data_qc()) |> 
            paste0("個QC樣本")
    )
    output$group_sum <- renderTable({
        summarise(data(), n(), .by = group)
    })
    
    #--- density plot
    output$density <- renderPlot(height = 800, res = 144, {
        req(input$var_d)
        p <- ggplot(data(), aes(.data[[input$var_d]], fill = group)) +
            geom_density(alpha = 0.5, bw = input$bw_d) +
            scale_x_continuous(labels = comma) +
            scale_y_continuous(labels = comma) 
        ifelse(
            input$qc_d,
            return(p + 
               geom_vline(
                   xintercept = data_qc() |> 
                       filter(FileName == input$object_d) |> 
                       pull(input$var_d)
               )
            ),
            return(p)
        )
    })

    observeEvent(data(),
        updateSelectInput(session, "var_d", choices = var_names())
    )
    observeEvent(input$qc_d, {
        updateTabsetPanel(
            inputId = "switcher", 
            selected = ifelse(input$qc_d, "panel_check", "panel_empty")
        )
    })
    observeEvent(data(),
        updateSelectInput(session, "object_d", choices = data_qc()$FileName)
    )
    #--- cor plot
    output$cor <- renderPlot(height = 800, res = 144, {
        cor(
            data()|> 
                filter(group == input$var_cor) |> 
                select(where(is.numeric))
        ) |> 
        round(2) |> 
        corrplot::corrplot(
            method = "color",
            order = 'alphabet',
            type = "lower",
            diag = FALSE
        )
    })
    observeEvent(data(),
        updateSelectInput(session, "var_cor", choices = data()$group |> unique())
    )
    #--- dumbbell plot
    dumbbell_1 <- reactive(paste0(input$var_db, "_", input$group_db[1]))
    dumbbell_2 <- reactive(paste0(input$var_db, "_", input$group_db[2]))
    output$dumbbell <- renderPlot(height = 800, res = 144, {
        req(input$var_db, input$group_db)
        if (length(input$group_db) == 2) {
            ggplot(data_wide(), aes(y = reorder(id, .data[[dumbbell_1()]]))) +
                geom_segment(
                    aes(
                        x = .data[[dumbbell_1()]],
                        xend = .data[[dumbbell_2()]], 
                        yend = id
                    )
                ) +
                geom_segment(
                    aes(
                        x = .data[[dumbbell_1()]],
                        xend = (.data[[dumbbell_2()]] + .data[[dumbbell_1()]])/2, 
                        yend = id
                    ),
                    arrow = arrow(length = unit(0.2, "cm"))
                ) +
                geom_point(aes(x = .data[[dumbbell_1()]]), size = 3, color = "#a3c4dc") +
                geom_point(aes(x = .data[[dumbbell_2()]]), size = 3, color = "#0e668b") +
                theme(
                    axis.ticks = element_blank(),
                    panel.border = element_blank()
                ) +
                labs(
                    x = NULL, 
                    y = NULL, 
                    title = "Dumbbell Chart"
                ) +
                scale_x_continuous(labels = comma)
        }
        
    })
    observeEvent(data(),
        updateSelectInput(session, "var_db", choices = var_names())
    )
    observeEvent(data(),
        updateSelectInput(session, "group_db", choices = input$groups)
    )
  
    #--- box plot
    output$box <- renderPlot(height = 800, res = 144, {
        ggplot(
            data_long() |> filter(type %in% input$var_b),
            aes(type, value, fill = group)
        ) +
            geom_boxplot() + 
            labs(
            title = "Box plot", 
            subtitle = ""
            ) +
            scale_y_continuous(labels = comma)
    })     
    observeEvent(data(),
        updateCheckboxGroupInput(session, "var_b", choices = var_names()) 
    )
}