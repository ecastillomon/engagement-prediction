library(dplyr)
library(ggplot2)
library(purrr)
library(lubridate)
source('lib/load_files.R')
df_sum=df_full %>% group_by(channel_uid) %>% arrange(publishedDate) %>% 
  summarise(n=n(),starting_audience=first(potentialReach),
            last_audience=last(potentialReach),source=first(source),tier=first(tier),
            engagements_rate_sd=sd(engagements_rate)) %>% ungroup() %>% 
  mutate(observed_audience_change=(last_audience-starting_audience)/starting_audience)
df_sum %>% ggplot(aes(x=starting_audience,y=observed_audience_change))+
  geom_point()+scale_x_continuous(labels=scales::comma_format())+
  scale_y_continuous(labels=scales::percent_format())


df_sum %>% ggplot(aes(x=starting_audience,y=last_audience))+
  geom_point()+scale_x_continuous(labels=scales::comma_format())+
  scale_y_continuous(labels=scales::comma_format())


##Channel
df_sum %>% group_by(source,tier) %>% summarise(n=n()) %>% ungroup() %>% tidyr::pivot_wider(names_from = tier,values_from = n,values_fill = 0)



## Forecasting results
df_forecast=c('output/summary_df_2505090034.csv','output/summary_df_2505081752.csv',
              'output/summary_df_2505111838.csv') %>% 
  purrr::map_dfr(function(x){read.csv(x)} )


df_forecast_optim=df_forecast %>% group_by(channel_uid,metric_name) %>% 
  arrange(desc(test_MeanAbsolutePercentageError)) %>% filter(row_number()==1) %>% ungroup()
# df_forecast_sum=df_forecast_optim %>% group_by(source,model_name,metric_name) %>% summarise(n=n()) %>% ungroup()


df_sum_f=df_sum %>% left_join(df_forecast_optim %>% filter(metric_name=='engagements_rate') %>% select(-source),by=c('channel_uid'))
df_forecast_sum=df_sum_f %>% 
  group_by(source,metric_name,tier) %>% 
  summarise(n=n(),across(matches('test_'),list(mean=mean,max=max,min=min),.names="{.col}_{.fn}" )) %>% ungroup()


df_forecast_sum %>% 
  ggplot(aes(x=tier,y=test_MeanAbsoluteError_mean))+geom_col()+facet_wrap(source~.)

df_sum_f %>% 
  ggplot(aes(x=starting_audience,y=test_MeanAbsoluteError))+geom_point()+scale_x_log10()+
  geom_smooth(method='lm')+
  scale_x_log10()+facet_wrap(source~.,scales = 'free')

library(broom)
library(tidyr)
# df_ig=df_full %>% filter(source=='instagram-content') 
models_by_tier <- df_full %>%
  group_by(tier,source) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(engagements_rate ~ potentialReach, data = .x)),
    summary = map(model, glance),
    coefficients = map(model, tidy)
  ) %>% ungroup() %>% select(-data)
models_by_tier %>%
  select(tier, summary,source) %>%
  unnest(summary) %>%
  ggplot(aes(x = tier, y = r.squared)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "R-squared of Engagement vs. Potential Reach by Tier",
    x = "Audience Tier",
    y = "R-squared"
  ) +facet_wrap(source~.)+
  theme_minimal()
models_by_tier %>%
  select(tier, coefficients,source) %>%
  unnest(coefficients) 



models_by_source <- df_full %>%
  group_by(source) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(engagements_rate ~ potentialReach, data = .x)),
    summary = map(model, glance),
    coefficients = map(model, tidy)
  ) %>% ungroup() %>% select(-data)
models_by_source %>%
  select( summary,source) %>%
  unnest(summary) %>%
  ggplot(aes(x = source, y = r.squared)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "R-squared of Engagement vs. Potential Reach ",
    x = "Audience Tier",
    y = "R-squared"
  ) +
  # facet_wrap(source~.)+
  theme_minimal()
models_by_source %>%
  select( coefficients,source) %>%
  unnest(coefficients) 



df_full %>% filter(channel_uid=='c149b6fafe123318b768e2e1c716abeb') %>% arrange(publishedDate) %>% 
  ggplot(aes(x=publishedDate,y=engagements_rate))+geom_line()+scale_y_log10()

df_full %>% filter(channel_uid=='44aac43453f33f5d9ff09e28a8f8dabc') %>% arrange(publishedDate) %>% 
  ggplot(aes(x=publishedDate,y=engagements_rate))+geom_line()+ scale_y_log10()



df_forecast_results=c('output/summary_complete_df_2505121209.csv') %>% 
  purrr::map_dfr(function(x){read.csv(x)} )


models_by_audience <- df_sum_f %>%
  group_by(source) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(test_MeanAbsoluteError ~ last_audience, data = .x)),
    summary = map(model, glance),
    coefficients = map(model, tidy)
  ) %>% ungroup() %>% select(-data)
models_by_audience_sum=models_by_audience %>% select(source, summary) %>% unnest()


df_sum_f %>% ggplot(aes(x=observed_audience_change,y=test_MeanAbsoluteError))+geom_point()+scale_y_log10()+scale_x_log10()

df_full %>% filter(channel_uid=="c149b6fafe123318b768e2e1c716abeb") %>% ggplot(aes(x=published_date,y=engagements_rate))+geom_line()+scale_y_log10()
