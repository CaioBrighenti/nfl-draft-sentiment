# NFL Draft Sentiment

This is a WIP project to stream comments from the [/r/NFL subreddit](http://www.reddit.com/r/NFL) and perform real-time sentiment analysis to gauge fan reactions during the 2020 NFL draft.

## Components

### Python data stream

The Python component of this project pulls new comments from /r/NFL in realtime using the [Python Reddit API Wrapper](https://praw.readthedocs.io/en/latest/) and automatically writes them into the ```data/comments.csv``` file. When a new comment is obtained, the script calls the [Stanford CoreNLP](https://stanfordnlp.github.io/CoreNLP/sentiment.html) sentiment annotator, which classifies the sentiment of the comment on a 0-4 scale. 0 represents highly negative, 2 neutral, and 4 highly positive. The script then records the timestamp, author flair (team), comment text, length, and sentiment.


### RShiny application

The RShiny component of this project relies on the ```reactiveFileReader()``` function to automatically read the comment data as the Python script updates it. Every 5 seconds, the data is reloaded and the Shiny server computes a rolling average of sentiment by team over the previous 30 seconds. This average is recorded in a table and visualized in two forms: a bar plot of current estimates by team, and a line chart showing the rolling average over time for each team. I intend to consider other metrics, such as proportion of of positive/negative comments as opposed to the sentiment metric outputted by CoreNLP.

![screenshot](https://raw.githubusercontent.com/CaioBrighenti/nfl-draft-sentiment/master/EVBQDbgWoAA5T8y2.PNG)

Given that the application only uses data from the last 30 seconds, there is no need to build up the dataset indefinitely, eventually causing slow downs due to the dataset size. To address this, I intend to build in a system that overwrites the accumulating .csv to remove unecessary application, while maintaining a complete copy elsewhere.

## To-Do

* Improve text processing, i.e. remove special characters and punctuation
* Remove commments from users with bandwagon flairs or NFC/AFC/NFL/None flairs
* ~~Implement sentiment analysis in Python script~~
* ~~Implement rolling average sentiment by team in RShiny~~
* ~~Implement bar plots showing fanbase sentiment in RShiny~~
* Test application on large-scale dataset
* Implement data cleanup to avoid indefinite accumulation
* Implement other metrics

## Built With

* [RShiny](https://shiny.rstudio.com/)
* [PRAW](https://praw.readthedocs.io/en/latest/)
* [CoreNLP](https://stanfordnlp.github.io/CoreNLP/)


## Authors

* **Caio Brighenti** - [CaioBrighenti2](https://twitter.com/CaioBrighenti2)

## Updates

On the day of the 2020 NFL Draft, I'll be streaming this RShiny application keeping track of fanbase sentiment throughout the draft on [my Twitter](https://twitter.com/CaioBrighenti2). I'll also likely Tweet out periodic updates, so that's the best place to keep track of this project. 
