P8105 - Final Project Proposal
================

| Name             | Uni     |
| ---------------- | ------- |
| Chia-Wen Kao     | jck2183 |
| Anna Ostropolets | ao2671  |
| JR Chansakul     | ac4635  |
| Thiago de Araujo | tbd2117 |

### Title:

#### Predictors of 2020 US Elections - Twitter or The Polls?

### Motivation for this project

US polls have been the center of attention in recent presidential
elections due to the fact that the results have not accurately reflected
predictions ([US PresidentialElection Polls
Failure](https://news.northeastern.edu/2020/11/04/the-polls-were-still-way-off-in-the-2020-election-even-after-accounting-for-2016s-errors/)).
Pollsters suggest that the evidence of the 2016 and 2020 elections will
improve polling models.

On the other hand, the evolution of social media has been tied to
political behavior such as voting ([Twitter as Indicator for Political
Behavior](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0079449)).
Since the public is quickly losing the trust of polls for political
elections, researchers are now exploring whether social media activity
can be utilized to assess offline political behavior. We believe that
extracting online social networking environments can shed light on
communication patterns and political preferences of individuals.

We hypothesize that social media (within the US and potentially outside
of the US) can be beneficial in identifying elelectoral behaviors. We
want to use Twitter (specifically geocoding tweets referencing
presidential candidates) to identify the political affiliation to that
region.

-----

### Intended final product

  - A final report
  - A webpage containing descriptive statistics, visualizations related
    to polls distribution and trends, interactive map comparing twitter
    hashtag frequency of 2020 US presidential candidates by county/state
    and US polls
  - Interpretation of comparisons between twitter posts and polls in
    regards to elections

-----

### Anticipated data sources:

[US Election
Twitter](https://www.kaggle.com/manchunhui/us-election-2020-tweets)

[2020 Election
Forecast](https://github.com/fivethirtyeight/data/tree/master/election-forecasts-2020)

[2020 Election](https://www.kaggle.com/unanimad/us-election-2020)

[UN - World Happines Report, potentially
useful](https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv)

-----

### The planned analyses / visualizations / coding challenges

In this project, we will:

  - Compare tweets of \#Biden/\#Trump by county/state and US polls to
    see if tweets are more accurate of predicting presidential results.
    We will make a choropleth map to visualize states that tweeted
    \#Biden and \#Trump and compare it to the 2020 election outcome. We
    will do the same with the polls.

  - Explore possible correlations between Twitter election-related post
    and their features (e.g. tweet share, emoji) and election results.

#### Plot/Charts:

1)  Distribution of \#Biden/\#Trump tweets by state;  
2)  US polls prediction of winning candidate by state  
3)  Graph of Current Votes by state for US Election Results  
4)  Interactive visualisization of tweets features and content and their
    distribution by state

##### Challenges

1)  Cleaning up tweets free-text or identifying meaningful variables in
    free text
2)  Processing the poll data to allow polls/tweets comparison
3)  Identifying proper model for correlation
4)  Building an interactive visualization embedded into a web site
5)  Handling large volume of data

### Timeline

| Tentative Dates | Tasks                                |
| --------------- | ------------------------------------ |
| November 7      | Form Team and submit proposal        |
| November 8-12   | TAs to confirm project and datasets  |
| November 10-13  | Project review meeting: Zoom meeting |
| November 13-15  | Data Cleaning                        |
| November 16-19  | Exploratory Analysis and Plots       |
| November 19\*\* | Check-in Meeting                     |
| November 20-27  | Visualization, Interactive Map       |
| November 27-30  | Finalize Webpage/Begin Report        |
| December 1-4    | Work on Report and Record Screencast |
| December 4      | Submit Report, Webpage, Screencast   |
| December 5      | Submit Peer Assessment               |
| December 10     | In class discussion/presentations\!  |
