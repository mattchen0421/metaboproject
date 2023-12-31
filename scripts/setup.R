library(openxlsx)
library(tidyverse)
library(scales)
library(plotly)
library(shinyglide)
library(mixOmics)
library(ggforce)
library(kohonen)
library(shinyjs)
library(factoextra)



sheet_names <- getSheetNames("data/testing_file.xlsx")
file_list <- lapply(sheet_names, read.xlsx, xlsxFile = "data/testing_file.xlsx")
names(file_list) <- sheet_names

info_sheet <- file_list[[sheet_names[2]]]
abundance <- file_list[[1]]

is_vars <- str_subset(names(info_sheet), "_id", negate = TRUE)[-1]
id_vars <- str_subset(names(info_sheet), "_id")

code <- str_which(paste0(is_vars, "_id"), paste(id_vars, collapse = '|'))

is_varsP <- is_vars[code]   
id_varsP <- set_names(id_vars, is_varsP)

var_names <- names(abundance)[-1]
filename <- names(abundance)[1]
by_file_name <- names(info_sheet)[1]
names(by_file_name) <- names(abundance)[1]

data <- abundance |> 
    right_join(info_sheet, by = by_file_name) |> 
    mutate(across(ends_with("_id"), ~factor(.x)))

# data_long <- data |> 
#     pivot_longer(all_of(var_names[1:5]), names_to = "vars", values_to = "values")
# 
# data_long2 <- data |> 
#     pivot_longer(all_of(is_vars[1]), names_to = "vars", values_to = "values")
# 
data_wide <- pivot_wider(
    data,
    names_from = condition_1,
    values_from = "Galactosylation",
    id_cols = prepost_id
)



cor(
    data |>
        filter(.data[[is_vars[1]]] == cor_group) |>
        select(all_of(var_names)) |>
        drop_na()
) |> 
    round(2) |> 
    corrplot::corrplot()
