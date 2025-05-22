df_mat=df_w_f %>% 
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



model_reg=lm(audience_growth_1~predicted_audience_growth+normalized_engagement_rate+n_posts,data=df_mat)
summary(model_reg)
model_data = model.frame(model_reg)
df_mat$predicted = predict(model_reg,newdata = df_mat)

# Plot actual vs predicted
ggplot(df_mat, aes(x = predicted, y = audience_growth)) +
  geom_point(alpha = 0.6) +  # scatter plot
  geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed") +  # 45-degree line
  labs(title = "Predicted vs Actual Growth",
       x = "Predicted Growth",
       y = "Actual Growth") +scale_y_continuous(limits=c(0,1))+
  theme_minimal()->p1
ggsave(p1,filename='output/linear_model_predicted.png')



model_reg=lm(audience_growth_1~normalized_engagement_rate,data=df_mat)
summary(model_reg)


model_reg=lm(audience_growth_1~posts_per_day,data=df_mat)
summary(model_reg)

model_reg=lm(audience_growth_1~n_posts,data=df_mat)
summary(model_reg)


model_reg=lm(audience_growth_1~video_views_sum,data=df_mat)
summary(model_reg)

model_reg=lm(audience_growth_1~comments_sum,data=df_mat)
summary(model_reg)

model_reg=lm(audience_growth_1~shares_sum,data=df_mat)
summary(model_reg)

library(broom)
library(tidyr)
# df_ig=df_full %>% filter(source=='instagram-content') 
models_by_source <- df_mat %>%
  group_by(source) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(audience_growth_1~predicted_audience_growth+normalized_engagement_rate+n_posts, data = .x)),
    summary = map(model, glance),
    coefficients = map(model, tidy)
  ) %>% ungroup() %>% select(-data)
models_by_source %>%
  select( summary,source) %>%
  unnest(summary) %>%
  ggplot(aes(x = source, y = r.squared,fill=source)) +
  scale_fill_brewer(type='qual',palette = 'Set1')+
  geom_col() +
  coord_flip() +
  labs(
    title = "R-squared of Linear model",
    x = "Audience Tier",
    y = "R-squared"
  ) +
  theme_minimal()


models_by_source %>%
  select( coefficients,source) %>%
  unnest(coefficients) %>%
  ggplot(aes(x = term, y = estimate,fill=term)) +
  geom_col() +
  coord_flip() +scale_fill_brewer(type='qual',palette = 'Set2')+
  labs(
    title = "Coefficients of Linear model",
    x = "Term",
    y = "Coefficient"
  ) +
  theme_minimal()+facet_wrap(source~.,scales='fixed')+theme(legend.position = 'none')


model_reg=lm(audience_growth_1~predicted_audience_growth+normalized_engagement_rate+posts_per_day+source,data=df_mat)
summary(model_reg)


library(lme4)

model_reg <- lmer(
  audience_growth_1 ~ predicted_audience_growth + normalized_engagement_rate + posts_per_day + 
    (1  | source), 
  data = df_mat
)

summary(model_reg)


model_reg <- lmer(
  audience_growth_1 ~ predicted_audience_growth + normalized_engagement_rate + posts_per_day + 
    (1 + normalized_engagement_rate | source),
  data = df_mat
)

summary(model_reg)



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
 


models_by_source <- df_mat %>%
  group_by(source) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(residual_audience_growth~posts_per_day+normalized_engagement_rate, data = .x)),
    summary = map(model, glance),
    coefficients = map(model, tidy)
  ) %>% ungroup() %>% select(-data)

coef_table <- models_by_source %>%
  select(source, coefficients) %>%
  unnest(coefficients) %>%
  select(source, term, estimate, std.error, statistic, p.value) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  arrange(source, term)

# Create LaTeX table
coef_table %>%
  kbl(format = "latex", booktabs = TRUE, caption = "Regression Coefficients by Source") %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

models_by_source %>%
  select( summary,source) %>%
  unnest(summary) %>%
  ggplot(aes(x = source, y = r.squared,fill=source)) +
  scale_fill_brewer(type='qual',palette = 'Set1')+
  geom_col() +
  coord_flip() +
  labs(
    title = "R-squared of Linear model",
    x = "Audience Tier",
    y = "R-squared"
  ) +
  theme_minimal()


models_by_source %>%
  select( coefficients,source) %>%
  unnest(coefficients) %>%
  ggplot(aes(x = term, y = estimate,fill=term)) +
  geom_col() +
  coord_flip() +scale_fill_brewer(type='qual',palette = 'Set2')+
  labs(
    title = "Coefficients of Linear model",
    x = "Term",
    y = "Coefficient"
  ) +
  theme_minimal()+facet_wrap(source~.,scales='fixed')+theme(legend.position = 'none')
library(lme4)

# Mixed-effects model: random slope of normalized_engagement_rate by source
mixed_model <- lmer(residual_audience_growth ~ posts_per_day + (normalized_engagement_rate | source), data = df_mat)

# View summary
summary(mixed_model)


df_mat %>% ggplot(aes(x=normalized_engagement_rate))+stat_ecdf()+scale_x_log10()
