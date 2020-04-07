import praw
import csv
import time
import re
reddit = praw.Reddit(client_id='',
                     client_secret='',
                     user_agent='')

with open('D:/repositories/nfl-draft-sentiment/data/comments.csv', 'w', newline='') as csvfile:
    # initiate csv writer
    comment_writer = csv.writer(csvfile, delimiter='\t',
                            quotechar='|', quoting=csv.QUOTE_MINIMAL)
    # write header
    comment_writer.writerow(["timestamp", "team", "body", "length"])

# loop through comments
for comment in reddit.subreddit('nfl').stream.comments():
    # grab text and remove new lines
    comment_text = comment.body.replace("\n", "")
    comment_text = comment_text.replace("\t", "")
    comment.text = re.sub(r"[^a-zA-Z0-9 !?,.()]+", '', comment_text)
    comment_time = time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(comment.created_utc))
    comment_length = len(comment_text)

    # visualize and write
    print([comment_time, comment.author_flair_text, comment_text, comment_length])
    with open('D:/repositories/nfl-draft-sentiment/data/comments.csv', 'a', newline='') as csvfile:
        # initiate csv writer
        comment_writer = csv.writer(csvfile, delimiter='\t',
                                quotechar='|', quoting=csv.QUOTE_MINIMAL)
        
        try:
            comment_writer.writerow([comment_time, comment.author_flair_text, comment_text, comment_length])
        except:
            print("UNSUPPORTED CHARACTER, REMOVING COMMENT")
        
