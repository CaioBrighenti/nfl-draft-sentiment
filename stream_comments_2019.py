import praw
import pandas

## INIT REDDIT INSTANCE
reddit = praw.Reddit(client_id='vh7hy1UfuwXEXg',
                     client_secret='as2NXgHFr_xYjzOI7AJox1ojW-o',
                     user_agent='python:sirbananas')

# load in submissions
df_sub = pandas.read_csv('C:/Users/Caio Laptop/Documents/Repositories/nfl-draft-sentiment/2019_submissions.csv')
for index, row in df_sub.iterrows():
    print(row)

#submission = reddit.submission(id='bhiw54')

#submission.comments.replace_more(limit=None)
#for comment in submission.comments.list():
#    print(comment.body)
