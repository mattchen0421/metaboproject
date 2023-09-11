data_org <- readxl::read_excel("IgQuantResult_4Std.xlsx")

data <- readxl::read_excel("testing.xlsx") |>
        mutate(
            group = str_extract(FileName, paste0(groups, collapse = "|")),
            id = str_extract(FileName, "[:digit:]+")
        ) |> 
        filter(!str_detect(FileName, "QC")) |> 
        drop_na()

groups <- c("post", "pre", "other")


summarise(data, n(), .by = group)




apply(is.na(data_org), 1, all) |> 
    sum()

data_org[!apply(data_org |> is.na(), 1, all),] |> 
    is.na() |>
    sum()

str_extract(drop_na(data_org)$FileName, "[:alpha:]+") |> unique() 
str_extract(FileName, "[:alpha:]+")
