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

