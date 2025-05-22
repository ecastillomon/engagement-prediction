#valid_sources=c('youtube-content','instagram-content','facebook-content','tiktok-content','instagram-reels','instagram-stories','instagram-igtv')
valid_sources=c('youtube-content','instagram-content','facebook-content','tiktok-content','instagram-reels','instagram-stories','instagram-igtv')
df_full=read.csv('~/rug/thesis/data/influencer_sample010525.csv') %>% 
  filter(source %in% valid_sources) %>% 
  mutate(
    metrics = map(metrics, ~ jsonlite::fromJSON(.x)),
    reactions = map_dbl(metrics, ~ {
      if (is.data.frame(.x)) {
        matched_rows <- .x %>% filter(stringr::str_detect(name, "like|reaction|diggs"))
        if (nrow(matched_rows) > 0) {
          as.numeric(matched_rows$value[1])
        } else {
          NA_real_
        }
      } else {
        NA_real_
      }
    })) %>% 
  # filter(channel_uid=="40bc077123683dba97cd786c23ca1d3a") %>%
  mutate_at(c('comments','video_views','reactions','engagements','video_plays','shares','potentialReach'),as.numeric) %>% 
  mutate(source=gsub('-content.*','-content',source),
         source=stringr::str_replace(source,'instagram-.*','instagram-content'),
         published_date=lubridate::as_datetime(publishedDate)) %>% 
  mutate(published_date_y=lubridate::floor_date(published_date,unit='years'),published_date_m=lubridate::floor_date(published_date,unit='months'),
         published_date_w=lubridate::floor_date(published_date,unit='weeks',week_start=7),
         published_year=lubridate::year(published_date),published_month=lubridate::month(published_date)) %>% 
  arrange(channel_uid,publishedDate) %>% 
  # rename(channel_uid=channelId) %>%  
  filter(!is.na(engagements) & potentialReach>0) %>% 
  mutate(engagements_rate=engagements/potentialReach,audience=potentialReach) %>% 
  group_by(channel_uid) %>% mutate(audience_change=(lead(audience,1)-audience)/audience, 
                                   engagement_rate_10=zoo::rollmean(engagements_rate,k=10,fill=NA)) %>% ungroup() %>% 
  mutate(tier = case_when(
    potentialReach < 1000 ~ 'less than 1,000',
    potentialReach >= 1000 & potentialReach < 10000 ~ '1,000–9,999',
    potentialReach >= 10000 & potentialReach < 50000 ~ '10,000–49,999',
    potentialReach >= 50000 & potentialReach < 100000 ~ '50,000–99,999',
    potentialReach >= 100000 & potentialReach < 500000 ~ '100,000–499,999',
    potentialReach >= 500000 ~ '500,000+',
    TRUE ~ 'Unknown')) %>% 
  mutate(tier = factor(tier,
    levels = c('less than 1,000','1,000–9,999','10,000–49,999','50,000–99,999','100,000–499,999','500,000+','Unknown'),ordered = TRUE)) %>% 
  group_by(channel_uid) %>% arrange(published_date) %>%
  mutate(posts_last_month = sapply(seq_along(published_date), function(i) {
    sum(published_date >= published_date[i] - days(30) & published_date <= published_date[i])
  })) %>% ungroup() %>% 
  mutate(posts_per_day=posts_last_month/30, day_of_week = wday(published_date, label = TRUE, abbr = TRUE),  
         hour_of_post = hour(published_date))  
# df_full %>% write.csv('data/influencer_sample010525_transformed.csv',row.names = FALSE)

df_sum=df_full %>% group_by(channel_uid) %>% arrange(publishedDate) %>% 
  summarise(n=n(),starting_audience=first(potentialReach),
            last_audience=last(potentialReach),source=first(source),tier=first(tier),
            engagements_rate_sd=sd(engagements_rate)) %>% ungroup() %>% 
  mutate(observed_audience_change=(last_audience-starting_audience)/starting_audience)


df_w=df_full %>% 
  group_by(published_date_w,channel_uid) %>% 
  summarise(n_posts=n(),across(c('engagements','video_views','video_plays','shares','comments','reactions'),
                               list(sum=function(x)sum(x,na.rm=TRUE))),
            first_audience=first(potentialReach),
            last_audience=last(potentialReach),source=first(source),tier=first(tier),
            mean_audience=mean(potentialReach,na.rm=TRUE),
            engagements_rate_sd=sd(engagements_rate)) %>% 
  ungroup() %>% 
  mutate(posts_per_day=n_posts/7,normalized_engagement_rate=engagements_sum/mean_audience,
         normalized_vv_rate=video_views_sum/mean_audience,normalized_vp_rate=video_plays_sum/mean_audience,normalized_shares_rate=shares_sum/mean_audience,
         normalized_comments_rate=shares_sum/mean_audience,normalized_reactions_rate=reactions_sum/mean_audience,
         ) %>% 
  group_by(channel_uid) %>% 
  mutate(min_published_date_w=min(published_date_w),
         last_audience_lag=dplyr::lag(last_audience,1),
         audience_growth=(last_audience-dplyr::lag(last_audience,1))/dplyr::lag(last_audience,1),
         audience_growth_1=dplyr::lead(audience_growth,1),
         audience_growth_2=dplyr::lead(audience_growth,2),
         audience_growth_3=dplyr::lead(audience_growth,3),
         audience_growth_4=dplyr::lead(audience_growth,4)) %>% ungroup() %>% 
  tidyr::complete(published_date_w,channel_uid) %>% 
  mutate(across(c(n_posts,audience_growth),function(x)coalesce(x,0))) %>% 
  # filter(channel_uid=='c149b6fafe123318b768e2e1c716abeb') %>%
  group_by(channel_uid) %>%
  mutate(first_non_zero_growth_date = min(published_date_w[audience_growth != 0], na.rm = TRUE)) %>% 
  # tidyr::fill(source, tier, first_audience, last_audience, .direction = "down") %>%
  ungroup() %>% 
  filter(published_date_w>=min_published_date_w & published_date_w>=first_non_zero_growth_date) %>%
  group_by(channel_uid) %>% 
  mutate(row_id=row_number()) %>% ungroup()

df_w_test=df_w %>% filter(channel_uid=="08c242dca64f305598abbd56e7b22c58") 


df_m=df_full %>% 
  group_by(published_date_m,channel_uid) %>% 
  summarise(n_posts=n(),across(c('engagements','video_views','shares','comments','reactions'),
                               list(sum=function(x)sum(x,na.rm=TRUE))),
            first_audience=first(potentialReach),
            last_audience=last(potentialReach),source=first(source),tier=first(tier),
            mean_audience=mean(potentialReach,na.rm=TRUE),
            engagements_rate_sd=sd(engagements_rate)) %>% 
  ungroup() %>% 
  mutate(posts_per_day=n_posts/30,normalized_engagement_rate=engagements_sum/mean_audience) %>% 
  group_by(channel_uid) %>% 
  mutate(min_published_date_m=min(published_date_m),
         last_audience_lag=dplyr::lag(last_audience,1),
         audience_growth=(last_audience-dplyr::lag(last_audience,1))/dplyr::lag(last_audience,1),
         audience_growth_1=dplyr::lead(audience_growth,1),
         audience_growth_2=dplyr::lead(audience_growth,2),
         audience_growth_3=dplyr::lead(audience_growth,3),
         audience_growth_4=dplyr::lead(audience_growth,4)) %>% ungroup() %>% 
  tidyr::complete(published_date_m,channel_uid) %>% 
  mutate(across(c(n_posts,audience_growth),function(x)coalesce(x,0))) %>% 
  # filter(channel_uid=='c149b6fafe123318b768e2e1c716abeb') %>%
  group_by(channel_uid) %>%
  mutate(first_non_zero_growth_date = min(published_date_m[audience_growth != 0], na.rm = TRUE)) %>% 
  # tidyr::fill(source, tier, first_audience, last_audience, .direction = "down") %>%
  ungroup() %>% 
  filter(published_date_m>=published_date_m & published_date_m>=first_non_zero_growth_date) %>%
  group_by(channel_uid) %>% 
  mutate(row_id=row_number()) %>% ungroup()


# df_w %>% write.csv('data/influencer_sample010525_weekly.csv',row.names = FALSE)
  
  
# df_x %>% ggplot(aes(x=published_date,y=posts_per_day))+geom_line()+geom_line(aes(y=audience_change),color='red')
