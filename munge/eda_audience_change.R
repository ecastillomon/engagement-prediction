df_full


model_reg=lm(audience_change~posts_per_day,data=df_full)
summary(model_reg)


models_audience_change <- df_full %>%
  group_by(source) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(audience_change ~ posts_per_day, data = .x)),
    summary = map(model, glance),
    coefficients = map(model, tidy)
  ) %>% ungroup() %>% select(-data)
models_audience_change %>%
  select( summary,source) %>%
  unnest(summary) %>%
  ggplot(aes(x = source, y = r.squared)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "R-squared of Audience Change vs. Posts per Day",
    x = "Audience Tier",
    y = "R-squared"
  ) +
  theme_minimal()
models_audience_change %>%
  select( coefficients,source) %>%
  unnest(coefficients) %>% 
  ggplot(aes(x = source, y = estimate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Coefficients Audience Change vs. Posts per Day",
    x = "Audience Tier",
    y = "Coefficients"
  ) +
  theme_minimal()+facet_wrap(term~.)



models_audience_change_er <- df_full %>%
  group_by(source) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(audience_change ~ engagement_rate_10, data = .x)),
    summary = map(model, glance),
    coefficients = map(model, tidy)
  ) %>% ungroup() %>% select(-data)
models_audience_change_er %>%
  select( summary,source) %>%
  unnest(summary) %>%
  ggplot(aes(x = source, y = r.squared)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "R-squared of Audience Change vs. Engagement Rate last 10 posts",
    x = "Audience Tier",
    y = "R-squared"
  ) +
  theme_minimal()
models_audience_change_er %>%
  select( coefficients,source) %>%
  unnest(coefficients) %>% 
  ggplot(aes(x = source, y = estimate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Coefficients Audience Change vs. Engagement Rate last 10 posts",
    x = "Audience Tier",
    y = "Coefficients"
  ) +
  theme_minimal()+facet_wrap(term~.)



df_forecast=c('output/summary_df_2505090034.csv','output/summary_df_2505081752.csv',
              'output/summary_df_2505111838.csv') %>% 
  purrr::map_dfr(function(x){read.csv(x)} )


df_forecast_optim=df_forecast %>% group_by(channel_uid,metric_name) %>% 
  arrange(desc(test_MeanAbsolutePercentageError)) %>% filter(row_number()==1) %>% ungroup()
# df_forecast_sum=df_forecast_optim %>% group_by(source,model_name,metric_name) %>% summarise(n=n()) %>% ungroup()


df_sum_f=df_sum %>% left_join(df_forecast_optim %>% filter(metric_name=='potentialReach') %>% select(-source),by=c('channel_uid'))
df_forecast_sum=df_sum_f %>% 
  group_by(source,metric_name,tier) %>% 
  summarise(n=n(),across(matches('test_'),list(mean=mean,max=max,min=min),.names="{.col}_{.fn}" )) %>% ungroup()


df_forecast_sum %>% 
  ggplot(aes(x=source,y=test_MeanAbsolutePercentageError_mean))+geom_col()

df_sum_f %>% 
  ggplot(aes(x=starting_audience,y=test_MeanAbsoluteError))+geom_point()+scale_x_log10()+
  geom_smooth(method='lm')+
  scale_x_log10()+facet_wrap(source~.,scales = 'free')


# library(knitr); knit('munge/eda_audience_v1.rmd',output = 'munge/eda_audience_v1.pdf')
rmarkdown::render('munge/eda_audience_v1.rmd', output_format = 'pdf_document', output_file = 'eda_audience_v1.pdf', output_dir = 'munge')

