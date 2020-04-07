# NFL Draft Sentiment

This is a WIP project to stream comments from the (/r/NFL subreddit)[http://www.reddit.com/r/NFL] and perform real-time sentiment analysis to gauge fan reactions during the 2020 NFL draft.

## Components

### Python data stream

The Python component of this project pulls new comments from /r/NFL in realtime using the [Python Reddit API Wrapper](https://praw.readthedocs.io/en/latest/) and automatically writes them into the ```data/comments.csv``` file. This file will also leverage the Python [Natural Language Toolkit](https://www.nltk.org/) package to calculate positive and negative sentiment for each comment, and record these in the .csv file.

### RShiny application

The RShiny component of this project relies on the ```reactiveFileReader()``` function to automatically read the comment data as the Python script updates it. Currently, this application does a simple visualization showing histograms of comment body lengths faceted by team as a proof of concept. The complete code for the RShiny application is available in ```shiny/app.R```.

In the future, the RShiny application will use the positive and negative sentiment values to compute a rolling average of sentiment for each team, and display these in faceted bar plots.

## Built With

* [RShiny](https://shiny.rstudio.com/)
* [PRAW](https://praw.readthedocs.io/en/latest/)
* [NLTK](https://www.nltk.org/)


## Authors

* **Caio Brighenti** - [CaioBrighenti2](https://twitter.com/CaioBrighenti2)

