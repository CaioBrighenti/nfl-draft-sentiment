# NFL Draft Sentiment

This is a WIP project to stream comments from the (/r/NFL subreddit)[http://www.reddit.com/r/NFL] and perform real-time sentiment analysis to gauge fan reactions during the 2020 NFL draft.

## Components

### Python data stream

The Python component of this project pulls new comments from /r/NFL in realtime using the [Python Reddit API Wrapper](https://praw.readthedocs.io/en/latest/) and automatically writes them into the ```data/comments.csv``` file. This file will also implement the sentiment analysis component of the project using the Python [Natural Language Toolkit](https://www.nltk.org/) package.

### RShiny application

The RShiny component of this project relies on the ```reactiveFileReader()``` function to automatically read the comment data as the Python script updates it. Currently, this application does a simple visualization showing histograms of comment body lengths faceted by team as a proof of concept. The complete code for the RShiny application is available in ```shiny/app.R```.

## Built With

* [RShiny](https://shiny.rstudio.com/)
* [PRAW](https://praw.readthedocs.io/en/latest/)
* [NLTK](https://www.nltk.org/)


## Authors

* **Caio Brighenti** - [CaioBrighenti2](https://twitter.com/CaioBrighenti2)

