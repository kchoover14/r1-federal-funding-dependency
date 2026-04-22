############ LIBRARIES
library(forecast)
library(ggplot2)

############ DATA
herd = read.csv('data/herd.csv')

############ WRANGLE
# national level time series
national_ts <- herd |>
  group_by(herd_year, carnegie_cycle) |>
  summarise(
    federal = sum(data[funding_type == "federal"], na.rm = TRUE),
    total   = sum(data[funding_type == "total"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(fed_share = federal / total)




############ LINEAR MODELS FOR TIME SERIES DATA
# simple model to identify trends, relative to the 1973 baseline
lm_model <- lm(fed_share ~ herd_year, data = national_ts)
summary(lm_model)
#The R² of 0.815 confirms a strong linear decline — federal share drops ~0.004 per year.

# add carnegie cycle to test if R1 expansion changed federal dependency
# did the rate change relative to a benchmark
# center herd_year around the mean so the intercept represents federal share at the average year rather than year 0
national_ts <- national_ts |>
  mutate(herd_year_c = herd_year - mean(herd_year))
lm_model2 <- lm(fed_share ~ herd_year_c + as.factor(carnegie_cycle), data = national_ts)
summary(lm_model2)
#cycles don't matter, only the year trend does

# did the rate of decline changed across cycles
# tests whether the slope changed at each Carnegie transition point, which is more theoretically meaningful
lm_model3 <- lm(fed_share ~ herd_year * as.factor(carnegie_cycle), data = national_ts)
summary(lm_model3)
#  model is overparameterized



############ TIME SERIES
# create time series object
fed_ts <- ts(national_ts$fed_share,
             start = min(national_ts$herd_year),
             frequency = 1)

# fit arima for trends
arima_model <- auto.arima(fed_ts)
summary(arima_model)

# RESULTS
# ARIMA(1,1,1) means:
# 1 autoregressive term — current value depends on previous value
# 1 difference — the series is non-stationary, confirming the declining trend
# 1 moving average term — smooths out short-term fluctuations
# MAPE of 1.55% suggests that the model fits the historical data well.


# forecast 10 years forward
# 10 is minimum number of years the impact of the current admin will last
arima_forecast <- forecast(arima_model, h = 10)
summary(arima_forecast)

# plot
autoplot(arima_forecast) +
  labs(title = "Forecast: Federal Share of R&D Expenditure at R1 Universities",
       x = "Year",
       y = "Federal Share") +
  theme_minimal()

# extract forecast data
forecast_df <- data.frame(
  year = 2025:2034,
  mean = as.numeric(arima_forecast$mean),
  lo80 = as.numeric(arima_forecast$lower[,1]),
  hi80 = as.numeric(arima_forecast$upper[,1]),
  lo95 = as.numeric(arima_forecast$lower[,2]),
  hi95 = as.numeric(arima_forecast$upper[,2])
)

# historical data
historical_df <- data.frame(
  year = time(fed_ts),
  fed_share = as.numeric(fed_ts)
)

# plot
ggplot() +
  geom_ribbon(data = forecast_df, aes(x = year, ymin = lo95, ymax = hi95),
              fill = "steelblue", alpha = 0.15) +
  geom_ribbon(data = forecast_df, aes(x = year, ymin = lo80, ymax = hi80),
              fill = "steelblue", alpha = 0.25) +
  geom_line(data = historical_df, aes(x = year, y = fed_share), color = "black", linewidth = 0.8) +
  geom_line(data = forecast_df, aes(x = year, y = mean), color = "steelblue", linewidth = 0.8) +
  geom_vline(xintercept = 2024.5, linetype = "dashed", color = "grey40") +
  annotate("text", x = 2025, y = 0.72, label = "Forecast", hjust = 0, size = 3, color = "grey40") +
  scale_x_continuous(breaks = seq(1974, 2034, by = 5), limits = c(1974, 2038)) +
  scale_y_continuous(labels = scales::percent, limits = c(0.47, 0.80)) +
  labs(title = "Federal Share of R&D Expenditure at R1 Universities",
       subtitle = "Shaded region shows 80% and 95% confidence intervals.\nLower bound reflects scenario of continued federal funding cuts.",
       x = "Year",
       y = "Federal Share of Total R&D",
       caption = "Source: NSF HERD Survey. Forecast: ARIMA(1,1,1).") +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 9, color = "grey40"),
        plot.margin = margin(10, 40, 10, 10))
ggsave("outputs/federal-dependency-forecast.png", width = 8, height = 5, dpi = 300)

############ TIDY
rm(list = ls())
gc()