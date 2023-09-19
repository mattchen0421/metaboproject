data_som <- data |> 
    select(all_of(var_names)) |> 
    as.matrix()
grid_size <- ceiling(nrow(data_som) ^ (1/2.5))
som <- data_som |>
    scale() |>
    kohonen::som(
        grid = somgrid(
            grid_size, grid_size,
            topo = "hexagonal", neighbourhood.fct = "gaussian"),
        rlen = 300
    )
som_grid <- som[[4]]$pts |>
    as_tibble() |>
    mutate(id = row_number())
som_pts <- tibble(
    id = som[[2]],
    dist = som[[3]],
    condition = drop_na(data) |> pull("condition_1")
) |>
    left_join(som_grid, by = "id")


ndist <- unit.distances(som$grid)
cddist <- as.matrix(object.distances(som, type = "codes"))
cddist[abs(ndist - 1) > .001] <- NA
neigh.dists <- colMeans(cddist, na.rm = TRUE)

som_grid <- som_grid |>
    mutate(dist = neigh.dists)


# count -------------------------------------------------------------------
som_count <- som_pts |> 
    group_by(x, y) |> 
    count(id, .drop = FALSE)

ggplot(som_count, aes(x0 = x, y0 = y)) +
    geom_regon(aes(sides = 6, angle = pi/2, r = 0.58, fill = n, ), color = "black") +
    theme(
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom"
    ) 
# pie ---------------------------------------------------------------------
pts <- som_pts |> 
    group_by(id, x, y) |> 
    count(condition) |> 
    ungroup() |> 
    dplyr::filter(!is.na(condition))
    
ggplot(som_grid, aes(x0 = x,y0 = y))+
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
# circle ------------------------------------------------------------------


ggplot(som_grid, aes(x0 = x,y0 = y))+
    geom_circle(aes(r = 0.5)) +
    theme(
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom"
    ) +
    geom_jitter(data = som_pts, aes(x, y, col = condition), alpha = 0.5, size = 3)


# changes -----------------------------------------------------------------

plot(som, type="changes")

data_changes <- tibble(
    x = 1:300,
    y = som[["changes"]]
)
ggplot(data_changes, aes(x = x, y = y)) +
    geom_line() +
    labs(x = "iteration", y = "Mean distance to closest unit", title = "Training progress")

# code --------------------------------------------------------------------


ggsom::geom_class(
    som, class = pull(drop_na(data), "condition_1")
)

# cluster -----------------------------------------------------------------

fviz_nbclust(som$codes[[1]], kmeans, method = "wss")
getCodes(som)
clust <- kmeans(som$codes[[1]], 6)

data_cluster <- som_grid |> 
    bind_cols("cluster" = clust[["cluster"]])

ggplot(data_cluster, aes(x0 = x, y0 = y)) +
    geom_regon(
        aes(sides = 6, angle = pi/2, r = 0.58, fill = cluster),
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
    labs(title = "cluster plot") +
    scale_fill_paletteer_c("grDevices::Heat", -1)

plot(
    som, type = "codes",
    bgcol = rainbow(9)[clust$cluster],
    main = "Cluster Map"
)
# heatmap -----------------------------------------------------------------

data_code <- som_grid |> 
    bind_cols(getCodes(som))

ggplot(data_code, aes(x0 = x, y0 = y)) +
    geom_regon(
        aes(sides = 6, angle = pi/2, r = 0.58, fill = Galactosylation, ),
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
    scale_fill_viridis_c(option = "magma")
paste("abc", filename)

test <- tags$div(
    id = "test",
    lapply(var_names, function(i) {
        column(width = 6,
           wellPanel(
               plotOutput(paste0("som_heatmap", i))
           )
        )
    })
)
    
