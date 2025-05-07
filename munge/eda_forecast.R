library(forecast)
library(ggplot2)
library(dplyr)

df_full=read.csv('~/rug/thesis/data/influencer_sample160325.csv') %>% rename(channel_uid=channelId) %>%  filter(!is.na(engagements))
cor(df_full$engagements_rate,df_full$potentialReach,use = 'complete.obs')
model_reg=lm(engagements_rate~potentialReach,data=df_full)
summary(model_reg)
model_data <- model.frame(model_reg)
df_full$predicted <- predict(model_reg,newdata = df_full)

# Plot actual vs predicted
ggplot(df_full, aes(x = predicted, y = engagements_rate)) +
  geom_point(alpha = 0.6) +  # scatter plot
  geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed") +  # 45-degree line
  labs(title = "Predicted vs Actual Engagement Rate",
       x = "Predicted Engagement Rate",
       y = "Actual Engagement Rate") +scale_y_continuous(limits=c(0,1))+
  theme_minimal()

resid_vals <- residuals(model_reg)
model_data$residuals <- resid_vals

# Plot residuals vs potentialReach
ggplot(model_data, aes(x = potentialReach, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Potential Reach",
       x = "Potential Reach",
       y = "Residuals") +scale_x_log10(labels=scales::comma_format())+
  theme_minimal()

plot_forecast <- function(x_ts, model,model_name,metric_name, h = 10, ci_width = 0.05) {
  
  n_obs <- length(x_ts)
  time_index <- seq_len(n_obs)
  future_index <- seq(n_obs + 1, n_obs + h)
  
  # Historical DataFrame
  historical_df <- data.frame(
    time = time_index,
    engagement_rate = as.numeric(x_ts)
  )
  
  if (is.numeric(model)) {
    # Proper forecasting model
    last_value <- tail(model[!is.na(model)], 1) # use last non-NA
    
    forecast_values <- rep(last_value, h)
    
    forecast_df <- data.frame(
      time = future_index,
      forecast = forecast_values,
      lower_80 = forecast_values * (1 - ci_width),
      upper_80 = forecast_values * (1 + ci_width),
      lower_95 = forecast_values * (1 - ci_width * 2),
      upper_95 = forecast_values * (1 + ci_width * 2)
    )
    
  } else {
    # TTR models (SMA, EMA, etc.) are just smoothed numeric vectors

    fc <- forecast::forecast(model, h = h,bootstrap=T,PI=T)
    
    forecast_df <- data.frame(
      time = future_index,
      forecast = as.numeric(fc$mean),
      lower_80 = as.numeric(fc$lower[, 1]),
      upper_80 = as.numeric(fc$upper[, 1]),
      lower_95 = as.numeric(fc$lower[, 2]),
      upper_95 = as.numeric(fc$upper[, 2])
    )
  }
  # Final Plot (consistent for all models)
  ggplot() +
    geom_line(data = historical_df, aes(x = time, y = engagement_rate), color = "black") +
    geom_line(data = forecast_df, aes(x = time, y = forecast), color = "blue") +
    geom_ribbon(data = forecast_df, aes(x = time, ymin = lower_95, ymax = upper_95), fill = "lightblue", alpha = 0.4) +
    geom_ribbon(data = forecast_df, aes(x = time, ymin = lower_80, ymax = upper_80), fill = "lightgrey", alpha = 0.4) +
    labs(title = glue::glue("{metric_name} Forecast"),
         subtitle = glue::glue("Model {model_name}"),
         x = "Post Index",
         y = metric_name) +
    theme_minimal()
}

# Example of usage
# x_ts <- df$engagement_rate %>% tail(24)
df_temp=df_full %>% filter(channel_uid=="51b52b775df43f1ea0abf5db8789446d"& !is.na(video_plays))
x_ts <- df_temp%>% pull(video_plays)
model_arima <- forecast::auto.arima(x_ts)
plot_forecast(x_ts, model_arima,model_name = 'Auto Arima' ,metric_name = 'Video plays',h = 6)


model_ema <- TTR::EMA(x_ts)
plot_forecast(x_ts, model_ema,model_name = 'EMA' ,metric_name = 'Video plays' ,h = 6)

model_ets=forecast::ets(x_ts)
plot_forecast(x_ts,model_ets,model_name = 'ETS' ,metric_name = 'Video plays')

model_theta=forecast::thetaf(x_ts)
plot_forecast(x_ts,model_theta,model_name = 'Theta' ,metric_name = 'Video plays')

# x_xts=xts::as.xts(df$engagement_rate,order.by=df$published_date,names='engagement_rate')
# x_ts <- ts(as.numeric(x_xts), frequency = 365, start = c(as.numeric(format(start(x_xts), "%Y")), as.numeric(format(start(x_xts), "%j"))))
x_xts=xts::as.xts(x_ts,order.by=df_temp$published_date)
model_lm=forecast::tslm(x_ts~trend)
plot_forecast(x_ts,model_lm)

nodel_stlm=forecast::stlm(df$engagement_rate)

model_ar=ar_fun(x_ts,10)
plot_forecast(x_ts,model_ar)

model_nnet=nnetar(x_ts,10)
plot_forecast(x_ts,model_nnet)
ar_fun <- function(x, h){forecast(Arima(x, order=c(4,0,0)), h=h)}
test_metrics=forecast::tsCV(x_ts,ar_fun,h=6)
# Remove NAs created by cross-validation
# Calculate RMSE, MAE, MAPE safely ignoring NA
# RMSE
rmse <- sqrt(colMeans(test_metrics^2, na.rm = TRUE))

# MAE
mae <- colMeans(abs(test_metrics), na.rm = TRUE)

# MAPE
# For MAPE, you need actual values aligned properly
# Here's the trick:
n <- length(x_ts)
mape <- numeric(6)

for (h in 1:6) {
  # Predicted errors are x_ts[t+h] - forecast[t]
  actuals <- x_ts[(h + 1):n]
  forecasts <- x_ts[1:(n - h)] + test_metrics[1:(n - h), h]  # reconstruct forecast = actual - error
  mape[h] <- mean(abs((actuals - forecasts) / actuals), na.rm = TRUE) * 100
}

# Final metrics table
metrics <- data.frame(
  Horizon = 1:6,
  RMSE = rmse,
  MAE = mae,
  MAPE = mape
)

print(metrics)

