############ GOALS

## How has federal funding as a share of total R&D expenditure at R1 universities changed over time?
# analysis:
# viz: Animated choropleth map showing change in federal funding over time by state — specifically designed so each congressional rep can find their state

# How dependent are R1 universities on federal funding for research?
# analysis: Time series analysis showing the trend in federal dependency
# viz:


############ LIBRARIES
library(dplyr)
library(ggplot2)
library(gganimate)
library(viridis)
library(classInt)
library(urbnmapr)
library(gifski)
library(scales)
library(tidyr)

############ DATA
herd = read.csv('data/herd.csv')


############ WRANGLE
# calculate federal % of total by state per year
map_data <- herd |>
  group_by(state, herd_year) |>
  summarise(
    federal = sum(data[funding_type == "federal"], na.rm = TRUE),
    total   = sum(data[funding_type == "total"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(state_pct = federal / total)

# check
summary(map_data$state_pct)
range(map_data$herd_year)
head(map_data)



############ MAP

# calculate federal % of total by state per year
map_data <- herd |>
  group_by(state, herd_year, carnegie_cycle) |>
  summarise(
    federal = sum(data[funding_type == "federal"], na.rm = TRUE),
    total   = sum(data[funding_type == "total"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(state_pct = federal / total)

# z-score by year
map_data <- map_data |>
  group_by(herd_year) |>
  mutate(state_pct_z = scale(state_pct)[,1]) |>
  ungroup()

# jenks breaks on z-scores across all years
breaks <- classIntervals(map_data$state_pct_z, n = 5, style = "jenks")$brks

# classify
map_data <- map_data |>
  mutate(dependency_class = cut(state_pct_z,
                                breaks = breaks,
                                labels = c("Low", "Below Average", "Average", "Above Average", "High"),
                                include.lowest = TRUE))

# get urbnmapr states including dc
us_states <- urbnmapr::states

# prep map data
map_data_clean <- map_data |>
  rename(state_abbv = state)

# expand to all states and years
all_states_years <- expand.grid(
  state_abbv = unique(us_states$state_abbv),
  herd_year = unique(map_data_clean$herd_year)
)

map_data_full <- all_states_years |>
  left_join(map_data_clean, by = c("state_abbv", "herd_year"))

map_plot_data <- us_states |>
  left_join(map_data_full, by = "state_abbv", relationship = "many-to-many")

# state centroids for labels
state_centroids <- us_states |>
  group_by(state_abbv) |>
  summarise(long = mean(long), lat = mean(lat))

# build map
p <- ggplot(map_plot_data, aes(x = long, y = lat, group = group, fill = dependency_class)) +
  geom_polygon(color = "white", linewidth = 0.2) +
  geom_text(data = state_centroids,
            aes(x = long, y = lat, label = state_abbv),
            inherit.aes = FALSE, size = 4, color = "black") +
  scale_fill_viridis_d(option = "mako",
                       begin = .7,
                       end = .2,
                       direction = -1,
                       name = "Federal Dependency",
                       na.value = "grey85",
                       guide = "legend") +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18)) +
  labs(title = "Federal Share of R&D Expenditure at R1 Universities",
       subtitle = "Year: {current_frame}") +
  transition_manual(herd_year)

anim <- animate(p, fps = 2, width = 900, height = 600, renderer = gifski_renderer())
anim_save("outputs/federal-dependency-map.gif", anim)


############ TABLES
# benchmark metrics
benchmark_table <- map_data |>
  group_by(dependency_class) |>
  summarise(
    n_state_years = n(),
    mean_fed_share = mean(state_pct, na.rm = TRUE),
    min_fed_share = min(state_pct, na.rm = TRUE),
    max_fed_share = max(state_pct, na.rm = TRUE)
  ) |>
  arrange(dependency_class)
write.csv(benchmark_table, "outputs/benchmark_table.csv", row.names = FALSE)


# benchmark table with states ranked
benchmark_table_state <- map_data |>
  group_by(state, carnegie_cycle) |>
  summarise(mean_fed_share = mean(state_pct, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = carnegie_cycle, values_from = mean_fed_share)
write.csv(benchmark_table, "artifacts/benchmark_table.csv", row.names = FALSE)


# high/low states
benchmark_table_slice <- map_data |>
  group_by(state, carnegie_cycle) |>
  summarise(mean_fed_share = mean(state_pct, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = carnegie_cycle, values_from = mean_fed_share) |>
  filter(!is.na(`2021`)) |>
  mutate(category = case_when(
    rank(desc(`2021`), ties.method = "first") <= 3 ~ "High",
    rank(`2021`, ties.method = "first") <= 3 ~ "Low",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(category))
write.csv(benchmark_table_slice, "outputs/benchmark_table_slice.csv", row.names = FALSE)


# volatility, two tables for project page
#High SD = most volatile/sensitive
#High range = biggest swings.
volatility_table <- map_data |>
  group_by(state) |>
  summarise(
    mean_fed_share = mean(state_pct, na.rm = TRUE),
    sd_fed_share = sd(state_pct, na.rm = TRUE),
    min_fed_share = min(state_pct, na.rm = TRUE),
    max_fed_share = max(state_pct, na.rm = TRUE),
    range_fed_share = max_fed_share - min_fed_share
  ) |>
  arrange(desc(sd_fed_share))
write.csv(volatility_table, "outputs/volatility_table.csv", row.names = FALSE)


############ TIDY
rm(list = ls())
gc()

