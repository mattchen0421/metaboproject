
# main --------------------------------------------------------------------

X <- data |> 
    column_to_rownames(filename) |> 
    dplyr::select(all_of(var_names))


Y <- data |> 
    pull(pair_prepost) |>
    factor()

tune_splsda <- tune.splsda(
    X, Y, ncomp = 5, validation = 'Mfold', 
    folds = 5, dist = 'max.dist', 
    test.keepX = 1:10, nrepeat = 10, progressBar = TRUE
)

plot(tune_splsda, sd = TRUE)
tune_splsda$choice.ncomp$ncomp
tune_splsda$choice.keepX

splsda <- splsda(X, Y, ncomp = 5) 

variates <- splsda$variates$X |> as_tibble()

ggplot(
    variates |> 
        bind_cols(condition = Y)
    , aes(color = condition)
) +
    geom_autopoint() +
    geom_autodensity(alpha = .3) +
    stat_ellipse(aes(x = .panel_x, y = .panel_y)) +
    facet_matrix(rows = vars(1:5), layer.diag = 2) +
    theme(
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12)
    )

ggplot(variates, aes(comp1, comp2, color = Y)) +
    geom_point() +
    stat_ellipse() +
    stat_stars()

plotVar(splsda, comp = c(1,2))

splsda_vip <- vip(splsda)[,c(1, 2)] |> 
    rowSums()

cor <- plotVar(splsda, comp = c(1,2), plot = FALSE)

splsda_vip[row.names(cor)]

final <- cor |> 
    add_column("importance" = splsda_vip[row.names(cor)]) |> 
    arrange(desc(importance)) |> 
    slice_head(n = 2)
    

splsda_cor <- plotVar(splsda, comp = c(1,2), plot = FALSE) |> 
    add_column("importance" = desc(splsda_vip) |> rank()) |> 
    filter(importance <= 5)
vip(splsda)[, c(1, 2)]
ggplot(splsda_cor, aes(x, y)) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.5), color = "lightblue") +
    geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = "lightblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "lightblue", linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "lightblue", linewidth = 1) +
    geom_point(aes(color = sqrt(x^2 + y^2)), size = 4) +
    ggrepel::geom_text_repel(aes(label = names), box.padding = 1) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_continuous(limits = c(-1, 1)) +
    paletteer::scale_color_paletteer_c("ggthemes::Red-Blue Diverging", limits = c(-1, 1)) +
    labs(title = "Correlation Circle Plot", x = "comp1", y = "comp2")

splsda_loading <- splsda$loadings$X |>
    data.frame() |> 
    rownames_to_column("var") |> 
    filter(abs(comp2) > 1 )
nrow(splsda_loading)
splsda_loading |> pull(var) |> length()
ggplot(splsda_loading, aes(comp2, reorder(var, abs(comp2)))) +
    geom_bar(
        aes(fill = ifelse(comp2 > 0, "pink", "lightblue")),
        stat = "identity", show.legend = FALSE
    ) +
    geom_text(
        aes(
            label = round(comp2, 2),
            hjust = ifelse(comp2 < 0, 1.25, -0.25),
            vjust = 0.5
        ),
        size = 5
    ) +
    xlab("Loading") +
    ylab("Group") +
    scale_x_continuous(limits = c(-1, 1))



setNames(paste0("Comp", 1:5), 1:5)


splsda <- splsda(X, Y, ncomp = 5, keepX = c(5, 5, 5, 5, 5)) 

splsda_perf <- perf(
    splsda, folds = 5, validation = "Mfold", 
    dist = "max.dist", progressBar = TRUE, nrepeat = 50
)
splsda_perf$error.rate.class |> as.data.frame() |> class()

select.name <- selectVar(splsda, comp = 1)$name

# Then extract the stability values from perf:
stability <- splsda_perf$features$stable$comp1[select.name]

# Just the head of the stability of the selected var:
cbind(selectVar(splsda, comp = 1)$value, stability)

vip(splsda) |> view()

stable.comp1 <- splsda_perf$features$stable$comp1 |> 
    as.data.frame()
stable <- splsda_perf$features$stable$comp1 
barplot(stable, xlab = 'variables selected across CV folds', 
        ylab = 'Stability frequency',
        main = 'Feature stability for comp = 1')
ggplot(stable.comp1, aes(x = Freq, y = reorder(Var1, Freq))) +
    geom_col(fill = "lightblue") +
    labs(
        title = "Feature stability",
        x = "variables selected across CV folds",
        y = "Stability frequency"
    ) 
