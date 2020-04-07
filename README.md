# NFL Draft Sentiment

This is a WIP project to stream comments from the (/r/NFL subreddit)[http://www.reddit.com/r/NFL] and perform real-time sentiment analysis to gauge fan reactions during the 2020 NFL draft.

## Components

### Python data stream

The Python component of this project pulls new comments from /r/NFL in realtime using the (Python Reddit API Wrapper)[https://praw.readthedocs.io/en/latest/] and automatically writes them into the ```data/comments.csv``` file. This file will also implement the sentiment analysis component of the project using the Python [Natural Language Toolkit](https://www.nltk.org/) package.

### RShiny application

## Built With

* [RShiny](https://shiny.rstudio.com/)
* [PRAW](https://praw.readthedocs.io/en/latest/)
* [NLTK](https://www.nltk.org/)


## Authors

* **Caio Brighenti** - [CaioBrighenti2](https://twitter.com/CaioBrighenti2)

