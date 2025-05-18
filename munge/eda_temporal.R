df_sum_time=df_full %>% group_by(channel_uid,published_date_m,published_date_y) %>% count() %>% 
  # filter(channel_uid=="40bc077123683dba97cd786c23ca1d3a") %>% 
  group_by(channel_uid) %>% summarise(years=unique(published_date_y) %>% length(),months=length(published_date_m),n=sum(n))

df_channel_month=df_full %>% group_by(channel_uid,published_date_m) %>% 
  summarise(first_audience=first(audience),last_audience=last(audience)) %>% ungroup() %>% 
  mutate(change_audience=(last_audience-first_audience)/first_audience) %>% filter(change_audience!=0)
df_channel_month_sum=df_channel_month %>% group_by(channel_uid) %>% count()
