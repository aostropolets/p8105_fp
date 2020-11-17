P8105 - Tweet cleaning
================

Making datasets less than 100MB just wit `drop_na`

``` r
trump = 
  read_csv("./datasets/hashtag_donaldtrump.csv") %>% 
  drop_na() %>%
  write.csv("./datasets/trump.csv")

biden = 
  read_csv("./datasets/hashtag_joebiden.csv") %>% 
  drop_na() %>% 
  write.csv("./datasets/biden.csv")
```

Importing datasets and making dates “tidy-er”

``` r
trump_df = 
  read_csv("./datasets/trump.csv") %>% select(!X1) %>% 
  separate(created_at, into = c("creation_date", "creation_time"), sep = " ") %>% 
  separate(creation_date, into = c("creation_year", "creation_month", "creation_day"), sep = "-") %>% 
  separate(user_join_date, into = c("join_date", "join_time"), sep = " ") %>% 
  separate(join_date, into = c("join_year", "join_month", "join_day"), sep = "-") %>% 
  mutate(hashtag = "Trump")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X1 = col_double(),
    ##   created_at = col_datetime(format = ""),
    ##   tweet_id = col_double(),
    ##   likes = col_double(),
    ##   retweet_count = col_double(),
    ##   user_id = col_double(),
    ##   user_join_date = col_datetime(format = ""),
    ##   user_followers_count = col_double(),
    ##   lat = col_double(),
    ##   long = col_double(),
    ##   collected_at = col_datetime(format = "")
    ## )

    ## See spec(...) for full column specifications.

``` r
biden_df = 
  read_csv("./datasets/biden.csv") %>% select(!X1) %>% 
  separate(created_at, into = c("creation_date", "creation_time"), sep = " ") %>% 
  separate(creation_date, into = c("creation_year", "creation_month", "creation_day"), sep = "-") %>% 
  separate(user_join_date, into = c("join_date", "join_time"), sep = " ") %>% 
  separate(join_date, into = c("join_year", "join_month", "join_day"), sep = "-") %>% 
  mutate(hashtag = "Biden")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   X1 = col_double(),
    ##   created_at = col_datetime(format = ""),
    ##   tweet_id = col_double(),
    ##   likes = col_double(),
    ##   retweet_count = col_double(),
    ##   user_id = col_double(),
    ##   user_join_date = col_datetime(format = ""),
    ##   user_followers_count = col_double(),
    ##   lat = col_double(),
    ##   long = col_double(),
    ##   collected_at = col_datetime(format = "")
    ## )
    ## See spec(...) for full column specifications.

Joining dfs

``` r
tweets = 
  merge(biden_df, trump_df, all = TRUE)
```
