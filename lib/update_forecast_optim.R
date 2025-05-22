## Forecasting results
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

write.csv(df_sum_f,'data/forecast_optim.csv',row.names = FALSE)
