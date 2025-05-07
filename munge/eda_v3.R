library(dplyr)
library(ggplot2)
library(evir)
library(qrmtools)
library(purrr)
# df=read.csv('~/rug/thesis/data/influencer_sample160325.csv') %>% rename(channel_uid=channelId) %>%  filter(!is.na(engagements))

df=read.csv('data/test_vv230225.csv') %>% mutate(engagement_rate=(reactions+comments)/potentialReach,loss=-reactions/potentialReach,
                                                 published_date=lubridate::as_date(publishedDate))

library(PtProcess)
library(lubridate)

# --- 1. Prepare your data ---

# Convert to time (days since first post)
# df <- df[order(df$published_date), ]
df$t_days <- as.numeric(difftime(df$published_date,
                                 min(df$published_date),
                                 units = "days"))

# Build data.frame like Tangshan: must have $time and $magnitude
# We'll use engagement_rate like "magnitude" for now
PostData <- data.frame(
  time = df$t_days,
  magnitude = df$engagement_rate   # rename just for compatibility
)

# Define time window
TT <- range(PostData$time)

# --- 2. Define a basic Poisson model (no self-excitation yet) ---

# You can reuse 'etas_gif' if you load it
# or for a simple Poisson, use 'const_gif' (built-in)

# If you want an ETAS-like model (posts triggering posts) you could keep etas_gif
# But likely for posts, a homogeneous Poisson or intensity depending on engagement_rate is enough

p <- c(0.007, 2.3, 0.98, 0.008, 0.94)

x <- mpp(
  data    = PostData,
  gif     = etas_gif,
  marks   = list(dexp_mark, NULL),
  params  = p,
  gmap    = expression(params),
  mmap    = expression(1),  # fixed b-value (engagement mark distribution)
  TT      = range(PostData$time)
)

# --- 3. Plotting ---

par.default <- par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(2,1), mar=c(4.1, 4.1, 0.5, 1))

# 3a. Plot intensity
plot(x, log=TRUE, xlab="Days since first post", ylab="Cumulative Intensity")

# 3b. Plot events (engagement_rate over time)
plot(PostData$time, PostData$magnitude,
     type="h",
     xlim=TT,
     xlab="Days since first post",
     ylab="Engagement Rate")

par(par.default)


##Forecast next 10 posts
library(forecast)
library(TTR)

x_ts=df$engagement_rate
model_arima=forecast::auto.arima(x_ts)
model_ma=TTR::SMA(x_ts,4)
h=6
fc <- forecast::forecast(model_arima, h = h)

# Convert forecast to a data frame for ggplot
fc_df <- data.frame(
  time = as.numeric(time(fc$mean)),
  forecast = as.numeric(fc$mean),
  lower_80 = as.numeric(fc$lower[,1]),
  upper_80 = as.numeric(fc$upper[,1]),
  lower_95 = as.numeric(fc$lower[,2]),
  upper_95 = as.numeric(fc$upper[,2])
)
# Create a time index
n_obs <- length(x_ts)
time_index <- seq_len(n_obs)
future_index <- seq(n_obs + 1,n_obs+h)

# Build a combined data frame
historical_df <- data.frame(
  time = time_index,
  engagement_rate = x_ts
)
forecast_df <- data.frame(
  time = future_index,
  forecast = as.numeric(fc$mean),
  lower_80 = as.numeric(fc$lower[,1]),
  upper_80 = as.numeric(fc$upper[,1]),
  lower_95 = as.numeric(fc$lower[,2]),
  upper_95 = as.numeric(fc$upper[,2])
)
# Plot with ggplot
ggplot() +
  geom_line(data = historical_df, aes(x = time, y = engagement_rate), color = "black") +
  geom_line(data = forecast_df, aes(x = time, y = forecast), color = "blue") +
  geom_ribbon(data = forecast_df, aes(x = time, ymin = lower_95, ymax = upper_95), fill = "lightblue", alpha = 0.4) +
  geom_ribbon(data = forecast_df, aes(x = time, ymin = lower_80, ymax = upper_80), fill = "lightgrey", alpha = 0.4) +
  labs(title = "Engagement Rate Forecast",
       subtitle = "Including Past Observations and Future Predictions",
       x = "Post Index",
       y = "Engagement Rate") +
  theme_minimal()
##Correlation
df_full=read.csv('~/rug/thesis/data/influencer_sample160325.csv') %>% rename(channel_uid=channelId) %>%  filter(!is.na(engagements))
cor(df_full$engagements_rate,df_full$potentialReach,use = 'complete.obs')
model_reg=lm(engagements_rate~potentialReach,data=df_full)
summary(model_reg)
