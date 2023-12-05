if(!require(pacman)) install.packages("pacman"); pacman::p_load(data.table, ggplot2, patchwork, ggpubr)

# Cost =========================================================================
cost <- fread("cost.csv")
cost_order <- cost[, .(cost, value)] |> 
  tibble::deframe() |>
  sort() |>
  names() |>
  unique()

strat_order <- copy(cost)[, value := sum(value), by = "strategy"
                    ][, .(strategy, value)] |>
  tibble::deframe() |>
  sort(decreasing = FALSE) |>
  names() |>
  unique()

cost <- cost[, `:=` (cost = factor(cost, levels = cost_order, ordered = TRUE),
                     strategy = factor(strategy, levels = strat_order, ordered = TRUE))]
  
p1 <- ggplot(cost, aes(x = strategy, y = value, fill = cost)) + 
  geom_col(position = "stack") + 
  labs(x = "",
       y = "Cost in USD",
       fill = "") + 
  scale_y_continuous(labels = scales::label_dollar()) + 
  theme_bw() +
  theme(legend.position = "bottom", legend.justification = .1) + 
  geom_text(aes(label = scales::label_dollar()(after_stat(y)), group = strategy),
            stat = "summary", fun = "sum", hjust = .5, vjust = -.3) + 
  scale_fill_manual(values = c("darkgrey", colorRampPalette(colors = c("#5DD2E9", "#003866"))(4))) + 
  coord_cartesian(ylim = c(3e6, NA))

costTable <- copy(cost)[, value := scales::label_dollar()(value)
                        ][, .("Strategy" = strategy, "Cost Category" = cost, "USD" = value)
                          ][USD != "$0"
                            ][, Strategy := factor(Strategy, levels = c("Status Quo", "Sweeps", "Housing (Choice)", "Housing (MOUD)"))]
p1 + ggtexttable(costTable, rows = NULL) + plot_layout(widths = c(1.8, 1))
ggsave("cost.png", width = 16, height = 8)
# Housing uptake ===============================================================
uptake <- fread("housing_uptake.csv")

ggplot(uptake, aes(x = housing_uptake, y = value, col = strategy)) + 
  geom_line() + 
  geom_point(alpha = 0.5) +
  theme_bw() + 
  labs(x = "Proportion taking up\n'Housing (MOUD)' from 'No Treatment'",
       y = "All Deaths",
       col = "") + 
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("#59CD90", "#3FA7D6","#773344", "#EF452E"))
ggsave("housing_uptake.png", height = 4, width = 10)
# Overdose Rate ================================================================
od <- fread("od_rate.csv")

ggplot(od, aes(x = od_rate_mult, y = value, col = strategy)) + 
  geom_line() + 
  geom_point(alpha = 0.5) +
  theme_bw() + 
  labs(x = "Multiplier for Default Overdose Rates\n(in Housing Only)",
       y = "Fatal Overdoses",
       col = "") + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("#59CD90", "#3FA7D6","#773344", "#EF452E")) + 
  scale_x_continuous(breaks = seq(0, 3, by = .5))
ggsave("od_rate.png", height = 4, width = 10)  
