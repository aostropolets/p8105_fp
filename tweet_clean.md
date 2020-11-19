Tweet cleaning
================

Making datasets less than 100MB just wit `drop_na`

``` r
read_csv("./datasets/hashtag_donaldtrump.csv") %>% 
  drop_na() %>%
  slice(seq(0.5 * n())) %>% 
  write.csv("./datasets/trump1.csv")

read_csv("./datasets/hashtag_donaldtrump.csv") %>% 
  drop_na() %>%
  slice(-seq(0.5 * n())) %>% 
  write.csv("./datasets/trump2.csv")
  
read_csv("./datasets/hashtag_joebiden.csv") %>% 
  drop_na() %>% 
  slice(seq(0.5 * n())) %>% 
  write.csv("./datasets/biden1.csv")

read_csv("./datasets/hashtag_joebiden.csv") %>% 
  drop_na() %>% 
  slice(-seq(0.5 * n())) %>% 
  write.csv("./datasets/biden2.csv")
```

Importing datasets and making dates “tidy-er”

``` r
trump_df = 
  merge(
    read_csv("./datasets/trump1.csv"),
    read_csv("./datasets/trump2.csv"),
    all = TRUE
  ) %>%
  select(!X1) %>% 
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
  merge(
    read_csv("./datasets/biden1.csv"),
    read_csv("./datasets/biden2.csv"),
    all = TRUE
  ) %>%
  select(!X1) %>%  
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

Joining dfs and subsetting for US

``` r
tweets = 
  merge(biden_df, trump_df, all = TRUE)

# Subset with USA tweets only
tweets_usa =
  tweets %>% 
  filter(country == "United States of America")
```

Election stuff

``` r
election_df =
  read_csv("./datasets/president_county_candidate.csv") %>% 
  group_by(state, party) %>% 
  mutate(party_total = sum(total_votes)) %>% 
  ungroup() %>% 
  group_by(state) %>%
  mutate(state_winner = case_when(
                      party_total == max(party_total) ~ TRUE,
                      party_total != max(party_total) ~ FALSE),
         state_total = sum(total_votes)
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   state = col_character(),
    ##   county = col_character(),
    ##   candidate = col_character(),
    ##   party = col_character(),
    ##   total_votes = col_double(),
    ##   won = col_logical()
    ## )

``` r
state_election_df = 
  election_df %>% 
  filter(state_winner == TRUE) %>% 
  select(state, candidate, party, party_total, state_total) %>% 
  distinct()
```

joining df

``` r
main_tweets_usa = 
  left_join(tweets_usa, state_election_df, by = "state") %>% 
  rename(
    winner_candidate = candidate,
    winner_party = party
  )
```
