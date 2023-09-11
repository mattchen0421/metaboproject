p <- ggplot(
    data(),
    aes(
        .data[[input$scatter_is]],
        .data[[input$scatter_var]],
        text = .data[[names(data())[1]]]
    )
) +
    geom_boxplot(fill = "lightblue") +
    geom_point(
        aes(color = .data[[input$scatter_ex]], group = .data[[input$scatter_id]]),
        size = 4,
        position = position_dodge(0.5)
    ) +
    geom_line(
        aes(group = .data[[input$scatter_id]], color = .data[[input$scatter_id]]), 
        position = position_dodge(0.5),
        linewidth = 0.5
    ) +
    scale_y_continuous(labels = comma)
ggplotly(p, tooltip = "text")