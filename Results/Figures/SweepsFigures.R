if(!require(pacman)) install.packages("pacman"); pacman::p_load(data.table, ggplot2, patchwork, ggpubr, wesanderson, svglite)

# Cost =========================================================================
cost <- fread("cost.csv")
cost_order <- cost[strategy == "Housing (MOUD)", .(cost, value)] |> 
  tibble::deframe() |>
  sort() |>
  names() |>
  unique()

strat_order <- copy(cost)[, value := sum(value), by = "strategy"
                    ][, .(strategy, value)] |>
  tibble::deframe() |>
  sort(decreasing = TRUE) |>
  names() |>
  unique()

cost <- cost[, `:=` (cost = factor(cost, levels = cost_order, ordered = TRUE),
                     strategy = factor(strategy, levels = strat_order, ordered = TRUE))]
  
p1 <- ggplot(cost, aes(x = strategy, y = value, fill = cost)) + 
  geom_col(position = "stack") + 
  labs(x = "",
       y = "Cost, in millions USD",
       fill = "",
       title = "Figure 2. Cost of Strategies by Category", 
       subtitle = "(1-year Time Horizon)") + 
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-6)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.justification = .1) + 
  scale_fill_manual(values = c("red", "#F38D3A", "#2A9D8F", "#60AFFF", "#B6A39E", "#773344")) + 
  coord_cartesian(ylim = c(3e6, NA)) + 
  theme(text = element_text(size = 16))
ggsave("cost_notbl.svg", height = 5, width = 9)

costTable <- copy(cost)[, value := scales::label_dollar()(value)
                        ][, .("Strategy" = strategy, "Cost Category" = cost, "USD" = value)
                          ][USD != "$0"
                            ][, Strategy := factor(Strategy, levels = c("Status Quo", "Sweeps", "Housing (MOUD)", "Housing First"), ordered = TRUE)]
setorder(costTable, Strategy)
p1 + ggtexttable(costTable, rows = NULL) + plot_layout(widths = c(1.8, 1))
ggsave("cost.svg", width = 16, height = 8)
# Housing uptake ===============================================================
uptake <- fread("housing_uptake.csv")

ggplot(uptake, aes(x = housing_uptake, y = value, col = strategy)) + 
  geom_line() + 
  geom_point(alpha = 0.5) +
  theme_bw() + 
  labs(x = "Proportion taking up\n'Housing (MOUD)' from 'No Treatment'",
       y = "All Deaths",
       col = "") + 
  theme(legend.position = "bottom",
        text = element_text(size = 16)) +
  scale_color_manual(values = c("#59CD90", "#3FA7D6","#773344", "#EF452E"))
ggsave("housing_uptake.svg", height = 4, width = 10)
# Overdose Rate ================================================================
od <- fread("od_rate.csv")

ggplot(od, aes(x = od_rate_mult, y = value, col = strategy)) + 
  geom_line() + 
  geom_point(alpha = 0.5) +
  theme_bw() + 
  labs(x = "Multiplier for Default Overdose Rates\n(in Housing Only)",
       y = "Fatal Overdoses",
       col = "",
       title = "Figure 3. Fatal Overdoses by Strategy and Housing Overdose Multiplier",
       subtitle = "(Applied to Both Housing Strategies)") + 
  theme(legend.position = "bottom",
        text = element_text(size = 16)) + 
  scale_color_manual(values = c("#59CD90", "#3FA7D6","#773344", "#EF452E")) + 
  scale_x_continuous(breaks = seq(0, 3, by = .5))
ggsave("od_rate.svg", height = 4, width = 10)  
