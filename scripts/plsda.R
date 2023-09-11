X <- data |> 
    column_to_rownames(filename) |> 
    dplyr::select(all_of(var_names))
Y <- data |> 
    pull("condition_1") |>
    factor()

plsda <- plsda(X,Y, ncomp = 9)

perf.plsda <- perf(
    plsda, validation = 'Mfold', folds = 3, 
    progressBar = FALSE,  # Set to TRUE to track progress
    nrepeat = 10
)         # We suggest nrepeat = 50

plot(perf.plsda, sd = TRUE)

overall <- perf.plsda$error.rate$overall |> 
    as.data.frame() |> 
    mutate("class" = "overall", "component" = 1:9)

BER <- perf.plsda$error.rate$BER |> 
    as.data.frame() |> 
    mutate("class" = "BER", "component" = 1:9) 

sd <- as.data.frame(perf.plsda$error.rate.sd$overall) |> 
    bind_rows(as.data.frame(perf.plsda$error.rate.sd$BER)) |> 
    pivot_longer(cols = 1:3, names_to = "dist", values_to = "sd")

perf_data <- bind_rows(overall, BER) |> 
    pivot_longer(cols = 1:3, names_to = "dist", values_to = "value") |> 
    bind_cols(sd[,2])

p <- ggplot(perf_data, aes(x = component, y = value, group = interaction(dist, class), color = dist)) +
    geom_point() +
    geom_path(aes(linetype = class)) +
    scale_x_continuous(breaks = 1:9) +
    geom_errorbar(aes(ymin = value - sd, ymax = value + sd))
ggplotly(p)

plotIndiv(plsda)

final_data <- plsda$variates$X |> 
    bind_cols(condition = Y)

ggplot(final_data, aes(color = condition)) +
    geom_autopoint(size = 0.1) +
    geom_autodensity(alpha = .3) +
    stat_ellipse(aes(x = .panel_x, y = .panel_y)) +
    facet_matrix(rows = vars(1:5), layer.diag = 2) +
    theme(
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12)
    )

