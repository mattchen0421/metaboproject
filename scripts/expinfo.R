library(openxlsx)
library(tidyverse)
library(scales)
library(ggridges)

sheet_names <- getSheetNames("data/IgQuantResult_4Std - 副本.xlsx")
file_list <- lapply(sheet_names, read.xlsx, xlsxFile = "data/IgQuantResult_4Std - 副本.xlsx")
names(file_list) <- sheet_names

info_sheet <- file_list[[sheet_names[2]]]
abundance <- file_list[[1]]

is_vars <- str_subset(names(info_sheet), "_id", negate = TRUE)[-1]
id_vars <- str_subset(names(info_sheet), "_id")

var_names <- names(abundance)[-1]


summarise(data, n(), .by = c("condition_1", "condition_2"))


by_file_name <- names(info_sheet)[1]
names(by_file_name) <- names(abundance)[1]

data <- abundance |> 
    right_join(info_sheet, by = by_file_name) |> 
    mutate(across(ends_with("_id"), ~factor(.x)))

data |> 
    select(prepost_id, condition_2, condition_3, Galactosylation) 


data_wide <- pivot_wider(
    data,
    names_from = condition_3,
    values_from = "Galactosylation",
    id_cols = prepost_id,
    unused_fn = list
)

data_wide$condition_2 |> paste0() 
ggplot(data, aes(x = Galactosylation, y = reorder(prepost_id, Galactosylation))) +
    geom_line() +
    geom_point(aes(color = condition_3), size = 3) +
    theme(legend.position = "bottom")


data |> 
    select(Monogalactosylation, condition_1, subject_id) |> 
    view()

cor(
    data |>
        select(where(is.numeric)) |> 
        filter(condition_1 = TRUE) |> 
        drop_na()
) |> 
    round(2) |> 
        corrplot::corrplot(
            method = "color",
            order = 'alphabet',
            type = "lower",
            diag = FALSE
    ) 

ggplot(
    data |>
        select(all_of("Monogalactosylation"), all_of(is_vars)),
    aes(Monogalactosylation, fill = condition_1)
) +
    geom_boxplot() + 
    labs(
        title = "Box plot", 
        subtitle = ""
    ) +
    scale_x_continuous(labels = comma)

data_long <- data |> 
    pivot_longer(all_of(var_names[1:5]), names_to = "vars", values_to = "values")

ggplot(data_long, aes(x = values, y = vars, fill = stat(quantile))) +
    stat_density_ridges(quantile_lines = TRUE,
                        calc_ecdf = TRUE,
                        geom = "density_ridges_gradient",
                        quantiles = c(0.05, 0.95)) +
    scale_fill_manual(name = "Prob.", values = c("#E2FFF2", "white", "#B0E0E6"),
                      labels = c("(0, 5%]", "(5%, 95%]", "(95%, 1]")) +
    scale_x_continuous(labels = comma)

ggplot(data = vaccinations,
       aes(axis1 = survey, axis2 = response, y = freq)) +
    ggalluvial::geom_alluvium(aes(fill = response)) +
    ggalluvial::geom_stratum() +
    geom_text(stat = "stratum",
              aes(label = after_stat(stratum))) +
    scale_x_discrete(limits = c("Survey", "Response"),
                     expand = c(0.15, 0.05)) +
    theme_void()

CGPfunctions::newggslopegraph(dataframe = df,
                Times = Year,
                Measurement = GDP,
                Grouping = Country)


t.test(data_wide$pre, data_wide$post, paired = TRUE, alternative = "two.sided") 
    
p <- ggplot(data, aes(condition_3, Galactosylation, text = FileName)) +
    geom_boxplot(fill = "lightblue") +
    geom_point(
        aes(group = prepost_id),
        size = 2,
        position = position_dodge(0.5)
    ) +
    geom_line(
        aes(group = prepost_id, color = prepost_id), 
        position = position_dodge(0.5),
        linewidth = 0.5
    ) +
    scale_y_continuous(labels = comma) +
    theme(legend.position = "none") +
    geom_segment(data = data_wide2,
                 aes(
                     x = "pre", y = pre,
                     xend = "post", yend = post,
                     color = pre > post
                 )
    ) 

p

ggplotly(p, tooltip = "text")

ggplot(data, aes(Galactosylation, Monogalactosylation)) +
    geom_hex()

ggplot(data, aes(x = condition_3, y = Galactosylation)) +
    geom_dotplot(binaxis = "y", stackdir = "center")

p2 <- ggplot(
    pivot_wider(
        data,
        names_from = condition_3,
        values_from = "Galactosylation",
        id_cols = prepost_id,
        unused_fn = list
    ), 
    aes(text = FileName)
) +
    geom_boxplot(aes("pre", pre), color = "#FFFFBF") +
    geom_boxplot(aes("post", post)) +
    geom_point(aes("pre", pre, group = ), position = position_jitter(seed = 1))+
    geom_point(aes("post", post), position = position_jitter(seed = 1)) +
    geom_segment(
        aes(
            x = "pre", y = pre,
            xend = "post", yend = post,
            color = pre > post
        ),
        position = position_jitter(seed = 1)
    ) +
    scale_y_continuous(labels = comma) +
    theme(legend.position = "none") +
    labs(x = FALSE, y = FALSE) +
    scale_color_manual(values = c("#F76D5E", "#72D9FF"))
ggplotly(p2, tooltip = "text")



