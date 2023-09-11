library(tidyverse)
library(ggcorrplot)
library(ggalt)

data_org <- readxl::read_excel("testing.xlsx")

groups <- c("post", "pre", "other")
data <- readxl::read_excel("testing.xlsx") |>
    mutate(
        group = str_extract(FileName, paste0(groups, collapse = "|")),
        id = str_extract(FileName, "[:digit:]+")
    ) |> 
    filter(!str_detect(FileName, "QC")) |> 
    drop_na()

    
post_data <- data |> 
    filter(str_detect(data$FileName, "post")) |> 
    mutate(
        id = str_extract(FileName, "[:digit:]+") |> as.numeric(),
        .before = everything()
    ) |> 
    arrange(id)

pre_data <- data |> 
    filter(str_detect(data$FileName, "pre")) |> 
    mutate(
        id = str_extract(FileName, "[:digit:]+") |> as.numeric(),
        .before = everything()
    ) |> 
    arrange(id)

long_data <- data |> 
    pivot_longer(cols = 2:10, names_to = "type", values_to = "value")

wide_data <- data |> 
    select(-FileName) |> 
    pivot_wider(
        names_from = group,
        values_from = all_of(names(data_org[-1])),
    )
data_qc <- data_org |> 
        filter(str_detect(FileName, "QC"))

data_qc[1, "Fucosylation"]|> as.numeric()

data[!c("id")] 
#---cor plot

corr <- cor(post_data |> select(where(is.numeric), -id)) |> 
    round(2)    
corrplot::corrplot(
    corr,
    method = "color",
    order = 'alphabet',
    type = "lower",
    diag = FALSE
)


#--- dumbbell plot

data() |> 
    mutate(
        group = if_else(str_detect(FileName, "post"), "post", "pre"),
        id = str_extract(FileName, "[:digit:]+")
    ) |> 
    select(id, group, any_of(input$var_db)) |> 
    pivot_wider(names_from = group, values_from = any_of(input$var_db)) |> 
    drop_na() |> 
    ggplot(aes(
        x = pre, xend = post,
        y = reorder(id, id |> str_rank(numeric = TRUE))
    )) + 
    ggalt::geom_dumbbell(
        colour_x = "#a3c4dc",
        colour_xend = "#0e668b",
        size_x = 2,
        size_xend = 2
    ) +
    labs(
        x = NULL, 
        y = NULL, 
        title = "Dumbbell Chart"
    ) +
    theme(
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()
    ) +
    scale_x_continuous(labels = comma)


#--- dumbbell plot 2
ggplot(wide_data) +
    geom_segment(aes(x = Fucosylation_post, xend = Fucosylation_pre,
                     y = id, yend = id)) +
    geom_point(aes(x = Fucosylation_post, y = id), size = 3) +
    geom_point(aes(x = Fucosylation_pre, y = id), size = 3)



#--- density plot
ggplot(data, aes(Fucosylation, fill = group)) +
    geom_density(alpha = 0.5, bw = 100000000) +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma)

#--- boxplot
ggplot(long_data |> filter(type == "Galactosylation"), aes(type, value, fill = group)) +
    geom_boxplot() + 
    labs(
        title="Box plot", 
        subtitle="",
        x="type of amino acids",
        y= "log value"
    ) 
# ------
ggplot(long_data |> drop_na(), aes(type, value)) +
    geom_point(aes(color = group), alpha = 0.8, shape = 1) + 
    labs(
        title="", 
        subtitle="",
        x="type of amino acids",
        y= "log value"
    ) +
    coord_flip() +
    scale_y_log10()
