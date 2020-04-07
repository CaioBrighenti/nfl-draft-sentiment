import praw
import csv
import time
import re
from pycorenlp.corenlp import StanfordCoreNLP

## INIT REDDIT INSTANCE
reddit = praw.Reddit(client_id='',
                     client_secret='',
                     user_agent='')

## coreNLP sent. analysis
def getSentiment(text):
    ## connect to CoreNLP server
    host = "http://localhost"
    port = "9000"
    nlp = StanfordCoreNLP(host + ":" + port)

    # annotate text
    output = nlp.annotate(
    text,
    properties={
        "outputFormat": "json",
        "annotators": "sentiment"
    }
    )

    # grab sentiment
    total_sent = 0
    n = 0
    for sen in output['sentences']:
        total_sent = total_sent + int(sen["sentimentValue"])
        n = n + 1

    # avoid divide by 0
    if n != 0:
        return total_sent / n
    else:
        raise Exception("Comment length 0")


with open('D:/repositories/nfl-draft-sentiment/data/comments.csv', 'w', newline='') as csvfile:
    # initiate csv writer
    comment_writer = csv.writer(csvfile, delimiter='\t',
                            quotechar='|', quoting=csv.QUOTE_MINIMAL)
    # write header
    comment_writer.writerow(["timestamp", "team", "body", "length", "sentiment"])

# loop through comments
for comment in reddit.subreddit('nfl').stream.comments():
    # grab text and remove new lines
    comment_text = comment.body.replace("\n", "")
    comment_text = comment_text.replace("\t", "")
    comment_text = re.sub(r"[^a-zA-Z0-9 !?,.()]+", '', comment_text)
    comment_time = comment.created_utc #time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(comment.created_utc))
    comment_length = len(comment_text)

    # get sentiment from coreNLP
    try:
        comment_sent = getSentiment(comment.body)
    except:
        print("ERROR: CORENLP FAILED")

    # visualize and write
    print([comment_time, comment.author_flair_text, comment_text, comment_length, comment_sent])
    try:
        with open('D:/repositories/nfl-draft-sentiment/data/comments.csv', 'a', newline='') as csvfile:
            # initiate csv writer
            comment_writer = csv.writer(csvfile, delimiter='\t',
                                    quotechar='|', quoting=csv.QUOTE_MINIMAL)
            
            try:
                comment_writer.writerow([comment_time, comment.author_flair_text, comment_text, comment_length, comment_sent])
            except:
                print("UNSUPPORTED CHARACTER, REMOVING COMMENT")
    except:
        print("ERROR: CONFLICT IN USING FILE")
        
