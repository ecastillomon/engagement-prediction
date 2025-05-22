df_mat=df_m_f %>% 
  # select(channel_uid,audience_growth,mean_audience,predictions_4_w,normalized_engagement_rate,posts_per_day) %>% 
  mutate(predicted_audience_growth=(predictions_1_w-mean_audience)/mean_audience,
         residual_audience_growth=audience_growth_1-predicted_audience_growth) %>% 
  filter(!is.na(predictions_4_w)) %>% group_by(source) %>% 
  mutate(normalized_engagement_rate_log=log(normalized_engagement_rate+1),
         normalized_engagement_rate_z = scale(normalized_engagement_rate)[,1],
         posts_per_day_z = scale(posts_per_day)[,1],
         predicted_audience_growth_z = scale(predicted_audience_growth)[,1],
         residual_audience_growth_z = scale(residual_audience_growth)[,1]
  ) %>% ungroup()

## Causal
model <- lm(residual_audience_growth ~ normalized_engagement_rate + posts_per_day+source , data = df_mat)

# View the results
summary(model)


## Causal
model <- lm(residual_audience_growth ~ normalized_engagement_rate+source  , data = df_mat)

# View the results
summary(model)

## Causal
model <- lm(residual_audience_growth ~ posts_per_day +source , data = df_mat)

# View the results
summary(model)
