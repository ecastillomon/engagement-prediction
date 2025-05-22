

# df_forecast_pred=read.csv('output/predictions_2505172132.csv') %>%
#   mutate(predictions = lapply(predictions, jsonlite::fromJSON)) %>%
#   tidyr::unnest_wider(predictions, names_sep = "_") %>%
#   rename(h_1 = predictions_1, h_2 = predictions_2, h_3 = predictions_3, h_4 = predictions_4) %>% 
#   mutate(predictions_4_w=h_4)
# 


# 
# df_test=df_sum %>% 
#   left_join(df_forecast_pred,by=c('channel_uid')) %>% 
#   mutate(audience_growth_pred= (predictions_4_w-last_audience)/last_audience)

extract_vector <- function(x) {
  as.numeric(stringr::str_extract_all(x, "\\d+\\.\\d+")[[1]])
}


df_forecast_results=c('output/summary_complete_df_2505181611.csv',
                      'output/summary_complete_df_2505191426.csv',
                      'output/summary_complete_df_2505182027.csv') %>% 
  #c('output/summary_complete_df_2505181611.csv') %>% 
  purrr::map_dfr(function(x){read.csv(x)} ) %>% 
  filter(metric_name=='mean_audience') %>% 
  rename(channel_uid=channel) %>% 
  mutate(across(c(y_pred, y_test, y_train), ~ map(.x, extract_vector))) %>% 
  tidyr::unnest_wider(y_pred, names_sep = "_") %>%
  rename(h_1 = y_pred_1, h_2 = y_pred_2, h_3 = y_pred_3, h_4 = y_pred_4) %>% 
  # rename(across(matches('h_'),function(x){gsub('h_','predictions_')})) %>% 
  rename_with(~ gsub('h_', 'predictions_', .x) %>% paste0(.,'_w'),matches('^h_')) %>%
  select(-c(source))
  # mutate_at(c('y_pred','y_test','y_train'),function(x){ ~ map(x, extract_vector)})
df_forecast_pred=df_forecast_results %>% inner_join(df_forecast_optim %>% 
                                                      select(-c(source)),by=c('channel_uid','model_name','metric_name'))

df_w_f=df_w %>% 
  left_join(df_forecast_pred,by=c('channel_uid','row_id'='len_train_window'))

# df_m_f=df_m %>% 
#   left_join(df_forecast_results,by=c('channel_uid','row_id'='len_train_window'))

# df_w %>% filter(channel_uid=="a94fb6713c3d34a699b97606751ad086") %>% 
#   ggplot(aes(x=published_date_w,y=last_audience))+geom_line()



df_w_sum=df_w %>% group_by(channel_uid) %>% count()

df_w_sum_source=df_w %>% group_by(source) %>% summarise(n=n(),n_channels=unique(channel_uid) %>% length())
df_w_sum_source
# df_w %>% filter(channel_uid=="4969170b498032739d656e8c2b0130c9") %>% 
#   ggplot(aes(x=published_date_w,y=last_audience))+geom_line()

## Forecasting results
df_forecast=c('output/summary_df_2505181611.csv','output/summary_df_2505191426.csv',
              'output/summary_df_2505182027.csv') %>% 
  purrr::map_dfr(function(x){read.csv(x)} )

##Optimal model for channel-metric
df_forecast_optim=df_forecast %>% group_by(channel_uid,metric_name) %>% 
  arrange(desc(test_MeanAbsolutePercentageError)) %>% filter(row_number()==1) %>% ungroup()


df_sum_f=df_sum %>% left_join(df_forecast_optim %>% filter(metric_name=='mean_audience') %>% select(-source),by=c('channel_uid'))

