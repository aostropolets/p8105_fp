WordClouds
================

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
  merge(biden_df, trump_df, all = TRUE) %>% 
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

The `wordcount_df` function need a `df` that contains a **tweet**
column.

It generates a new df with a *word* and a *freq* column.

``` r
wordcount_df = function(df) {
  
  library(tm)

  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  df_text = 
    df %>% 
    slice_head(n = 1000) %>% 
    select(tweet) %>%
    mutate(
       tweet = gsub("#[[:alpha:]]*", "", tweet),
       tweet = gsub("@[[:alpha:]]*", "", tweet),
       tweet = gsub("https\\S*", "", tweet),
       tweet = gsub("http\\S*", "", tweet),
       tweet = gsub("@\\S*", "", tweet),
       tweet = gsub("amp", "", tweet),
       tweet = gsub("[\r\n]", "", tweet),
       tweet = gsub("[0-9]", "", tweet),
       tweet = gsub("[[:punct:]]", "", tweet),
       tweet = gsub("\\W*\\b\\w\\b\\W*", " ", tweet),
       tweet = str_to_lower(tweet)
    )
  
  docs_df =
    Corpus(VectorSource(df_text)) %>% 
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords, stopwords("english")) %>% 
    tm_map(removeWords, stopwords("spanish"))
  
  words_df = 
    TermDocumentMatrix(docs_df) %>% 
    as.matrix() %>% 
    rowSums() %>% 
    sort(decreasing = TRUE)
  
  word_count_df = data.frame(word = names(words_df),freq = words_df)

  return(word_count_df)
  
}
```

Wordlcouds

``` r
# Tweets with #Biden or #JoeBiden
main_tweets_usa %>% 
filter(hashtag == "Biden") %>% 
wordcount_df() %>% 
wordcloud2(size = 1.5, color = 'random-dark', ellipticity = 1)

# Tweets with #Trump or #DonaldTrump
main_tweets_usa %>% 
filter(hashtag == "Trump") %>% 
wordcount_df() %>% 
wordcloud2(size = 1.5, color = 'random-dark', ellipticity = 1, shuffle = FALSE)

# Tweets from states won by democrats in the shape of the US
main_tweets_usa %>% 
filter(winner_party == "DEM") %>% 
wordcount_df() %>%
slice(-5) %>%   ##the top words all look the same
wordcloud2(size = 1.5, color = 'random-dark', ellipticity = 1, shuffle = FALSE, figPath = "usa.jpg")

# Tweets from states won by republicans
main_tweets_usa %>% 
filter(winner_party == "REP") %>% 
wordcount_df() %>% 
slice(-5) %>% 
wordcloud2(size = 1.2, figPath = "usa.jpg", ellipticity = 8, minSize = 6)

#looking at specificthe words
word =  
  main_tweets_usa %>% 
  filter(str_detect(tweet, "fake")) %>% 
  wordcount_df()
```

Creating table to display most frequent word…

<table>

<caption>

\#Biden

</caption>

<thead>

<tr>

<th style="text-align:left;">

word

</th>

<th style="text-align:right;">

freq

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

biden

</td>

<td style="text-align:right;">

168

</td>

</tr>

<tr>

<td style="text-align:left;">

will

</td>

<td style="text-align:right;">

113

</td>

</tr>

<tr>

<td style="text-align:left;">

joe

</td>

<td style="text-align:right;">

103

</td>

</tr>

<tr>

<td style="text-align:left;">

trump

</td>

<td style="text-align:right;">

80

</td>

</tr>

<tr>

<td style="text-align:left;">

hunter

</td>

<td style="text-align:right;">

60

</td>

</tr>

<tr>

<td style="text-align:left;">

election

</td>

<td style="text-align:right;">

53

</td>

</tr>

<tr>

<td style="text-align:left;">

just

</td>

<td style="text-align:right;">

52

</td>

</tr>

<tr>

<td style="text-align:left;">

vote

</td>

<td style="text-align:right;">

50

</td>

</tr>

<tr>

<td style="text-align:left;">

twitter

</td>

<td style="text-align:right;">

46

</td>

</tr>

<tr>

<td style="text-align:left;">

decisions

</td>

<td style="text-align:right;">

45

</td>

</tr>

</tbody>

</table>

<table>

<caption>

\#Trump

</caption>

<thead>

<tr>

<th style="text-align:left;">

word

</th>

<th style="text-align:right;">

freq

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

vote

</td>

<td style="text-align:right;">

127

</td>

</tr>

<tr>

<td style="text-align:left;">

trump

</td>

<td style="text-align:right;">

115

</td>

</tr>

<tr>

<td style="text-align:left;">

please

</td>

<td style="text-align:right;">

99

</td>

</tr>

<tr>

<td style="text-align:left;">

will

</td>

<td style="text-align:right;">

99

</td>

</tr>

<tr>

<td style="text-align:left;">

antitrump

</td>

<td style="text-align:right;">

88

</td>

</tr>

<tr>

<td style="text-align:left;">

people

</td>

<td style="text-align:right;">

62

</td>

</tr>

<tr>

<td style="text-align:left;">

just

</td>

<td style="text-align:right;">

59

</td>

</tr>

<tr>

<td style="text-align:left;">

like

</td>

<td style="text-align:right;">

59

</td>

</tr>

<tr>

<td style="text-align:left;">

now

</td>

<td style="text-align:right;">

58

</td>

</tr>

<tr>

<td style="text-align:left;">

biden

</td>

<td style="text-align:right;">

50

</td>

</tr>

</tbody>

</table>

## WordClouds

\[<http://www.clc-ent.com/TBDE/Docs/wordCloud.pdf>\]
\[<https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a>\]
\[<http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need>\]

``` r
#Option1
wordcloud(words = biden_text$word, freq = biden_text$freq, min.freq = 2,           
          max.words = 250, random.order = FALSE, rot.per = 0.45,            
          colors = brewer.pal(8, "Dark2"), scale = c(3.5,0.25))

wordcloud(words = trump_text$word, freq = trump_text$freq, min.freq = 2,           
          max.words = 250, random.order = FALSE, rot.per = 0.45,            
          colors = brewer.pal(8, "RdBu"), scale = c(3.5,0.25))

#Option2
wordcloud2(data = biden_text, size = 1.5, color = 'random-dark', ellipticity = 1)

wordcloud2(data = trump_text, size = 1.5, color = 'random-dark', ellipticity = 1)
```

Useful stuff:

tidytext::unnest\_tokens() -\> unique words

<https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mining-twitter-data-intro-r/>

## WordClouds

\[<http://www.clc-ent.com/TBDE/Docs/wordCloud.pdf>\]
\[<https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a>\]
\[<http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need>\]

## Exporting as pdf or png… **not working**

\[<https://www.r-graph-gallery.com/196-the-wordcloud2-library.html#export>\]

``` r
library("htmlwidgets")
#install.packages("webshot")
library(webshot)
webshot::install_phantomjs()

# Make the cloud
biden_usa = 
  wordcloud2(word_count_biden, size = 1.2, figPath = "usa.jpg", ellipticity = 8, minSize = 6)

# save it in html
saveWidget(biden_usa,"tmp.html",selfcontained = FALSE)

# and in png or pdf
webshot("tmp.html","biden_usa.png", delay =5, vwidth = 480, vheight = 480)
```
