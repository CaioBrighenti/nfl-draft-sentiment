# NFL Draft Sentiment

This is a project to stream comments from the [/r/NFL subreddit](http://www.reddit.com/r/NFL) as well as individual team subreddits during the 2020 NFL draft to perform real-time sentiment analysis, gauging fan reactions after each pick. Following the draft, all data collected will be posted here.

## Components

### Data Pipeline

The real-time data pipeline for this project is mainly implemented in ```stream_comments.py```. The Python script pulls new comments in from /r/NFL as well as each team subreddit in realtime using [Python Reddit API Wrapper](https://praw.readthedocs.io/en/latest/) and automatically writes them into the ```data/comments.csv``` file. When a new comment is obtained, the script calls the [Stanford CoreNLP](https://stanfordnlp.github.io/CoreNLP/sentiment.html) sentiment annotator, which classifies the sentiment of the comment on a 0-4 scale. 0 represents highly negative, 2 neutral, and 4 highly positive. The script then records the timestamp, author team, comment text, length, and sentiment. When a comment is from a team subreddit, the author's team is marked as that team. This obviously assumes that only fans of a certain team are commenting on that subreddit, which is a small limitation of this approach. When a comment is from /r/NFL, the script obtains the user's flair to identify the author's team. Comments without a flair are discarded.

### RShiny application

The RShiny component of this project relies on the ```reactiveFileReader()``` function to automatically read the comment data as the Python script updates it. Every 5 seconds, the data is reloaded and the Shiny server computes a rolling average of sentiment by team over the previous 2 minutes. This average is recorded in a table and visualized in two forms: a bar plot of current estimates by team, and a line chart showing the rolling average over time for each team for the last 10 minutes. In addition to the sentiment tracker by team, a second bar plot is shown counting the total number of comments over the last 2 minutes for each team, in order to measure activity and not just sentiment. 

The plots described above are always visible for all teams, but two gauge plots are also included focusing on the last and next team to pick, in order to more easily gauge reactions to picks. After each pick, these team who last picked is swapped out to include the next team up. These gauge plots are a "boo-meter," representing the current estimate of the fanbase sentiment. An example of what the application looks like can be seen below.

![screenshot](https://github.com/CaioBrighenti/nfl-draft-sentiment/blob/master/images/EVBQDbgWoAA5T8y3.PNG?raw=true)

### Data Cleanup

Given that the application only uses the last 2 minutes of comments to compute the rolling average at any given point, there is no need to continously re-load all comments every refresh period. To address this problem while minimizing writes to ```data/comments.csv``` (this is necessary to avoid conflicts between RShiny and the Python writer), the Python script will regularly enter a cleanup stage once 10,000 comments have been written. At this point, the script initiates a new ```.csv```, and for the next two minutes writes new comments into both ```comments.csv``` and the new ```.csv```. After these two minutes have passed, the new, shorter ```.csv``` can now become the main one. To do this, the Python script renames the original ```comments.csv``` file and places it in the ```data/chunks/``` folder, then renames the second ```.csv``` to ```comments.csv```. This makes for a seamless transition on the RShiny side of the application. The Python script then resets the counter and repeats the process after another 10,000 comments.

## Draft Day

During the draft on April 23rd-25th, I will be livestreaming the application at [twitch.tv/caiobrighenti](https://www.twitch.tv/caiobrighenti), and Tweeting out periodic updates at [@CaioBrighenti](https://twitter.com/CaioBrighenti). Given that the comment volume during the draft will likely be significantly larger than it currently is, there is the possibility that my application will require on-the-fly maintenance and performance adjustments. If the stream goes down, check my Twitter for updates and wait for it to come back online. I'll also be posting the complete dataset after the draft, and would love to see what other insights and ways to visualize it others can think of.

## To-Do

* ~~Remove commments from users with bandwagon flairs or NFC/AFC/NFL/None flairs~~
* ~~Implement sentiment analysis in Python script~~
* ~~Implement rolling average sentiment by team in RShiny~~
* ~~Implement bar plots showing fanbase sentiment in RShiny~~
* Test application on large-scale dataset
* Implement data cleanup to avoid indefinite accumulation
* ~~Add sidebar with last 15 comments~~
* ~~Add boo/cheer gauge plot for team currently picking~~
* ~~Implement system to keep track of and update draft order~~

## Built With

* [RShiny](https://shiny.rstudio.com/)
* [PRAW](https://praw.readthedocs.io/en/latest/)
* [CoreNLP](https://stanfordnlp.github.io/CoreNLP/)
* [VADER](https://github.com/cjhutto/vaderSentiment)

## Authors

* **Caio Brighenti** - [CaioBrighenti](https://twitter.com/CaioBrighenti)

## Acknowledgements

* [PFF_Moo](https://twitter.com/PFF_Moo) - For helping me with RShiny layout issues and getting team logos working
* [Lee Sharpe](https://twitter.com/LeeSharpeNFL) - For providing team logos used
* [StackOverflow](https://stackoverflow.com/questions/50042214/fill-a-polygon-with-gradient-scale-in-r) - Gauge plot code 100% lifted from this thread
