import praw
import csv
import time
import re
import os
from pycorenlp.corenlp import StanfordCoreNLP
from profanity_filter import ProfanityFilter
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr

## INIT GLOBAL VARS
chunk_size = 10000
num_chunks = 0

## INIT REDDIT INSTANCE
reddit = praw.Reddit()


## INIT PROFANITY FILTER
pf = ProfanityFilter()
pf.censor_whole_words = False

## INIT VADER SENTIMENT
vader = SentimentIntensityAnalyzer()

## INIT TEXTCLEAN R
textclean = importr('textclean', lib_loc = "C:/Users/caiob/Documents/R/win-library/3.6")
importr('stringi', lib_loc = "C:/Users/caiob/Documents/R/win-library/3.6")

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
    comment_writer.writerow(["timestamp", "team", "body", "length", "sentiment", "subreddit"])

# setup multireddit
team_subreddits = ["KansasCityChiefs","raiders","DenverBroncos","Chargers","Colts","Tennesseetitans","Texans","Jaguars","bengals","steelers",
"ravens","Browns","miamidolphins","nyjets","buffalobills","Patriots","cowboys","NYGiants","eagles","Redskins","CHIBears","GreenBayPackers",
"detroitlions","minnesotavikings","falcons","Saints","panthers","buccaneers","AZCardinals","49ers","LosAngelesRams","Seahawks"]
team_names = ["Chiefs","Raiders","Broncos","Chargers","Colts","Titans","Texans","Jaguars","Bengals","Steelers",
"Ravens","Browns","Dolphins","Jets","Bills","Patriots","Cowboys","Giants","Eagles","Redskins","Bears","Packers",
"Lions","Vikings","Falcons","Saints","Panthers","Buccaneers","Cardinals","49ers","Rams","Seahawks"]
teams_dict = dict(zip(team_subreddits,team_names))

while(True):
    # loop through comments
    n = 0
    for comment in reddit.subreddit("+".join(team_subreddits)+"+nfl").stream.comments():
        # grab text and remove new lines
        comment_text = comment.body.replace("\n", "")
        comment_text = comment_text.replace("\t", "")
        comment_text = re.sub(r"[^a-zA-Z0-9 !?,.()]+", '', comment_text)
        comment_time = comment.created_utc #time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(comment.created_utc))
        comment_length = len(comment_text)

        # remove elongations from text
        try:
            comment_text = textclean.replace_word_elongation(comment_text)[0]
        except:
            print("ERROR: WORD ELONGATION CLEANER FAILED")

        # grab subreddit team
        comment_subr = comment.subreddit.display_name
        if comment_subr == "nfl":
            comment_team = comment.author_flair_text
            if (comment_team not in team_names): continue
        else:
            comment_team = teams_dict[comment_subr]
            comment_subr = "team"

        # get sentiment
        try:
            #comment_sent = getSentiment(comment.body)
            comment_sent = vader.polarity_scores(comment_text)['compound']
            # identify just boos
        except:
            print("ERROR: SENTIMENT ANALYSIS FAILED")

        # sentiment heuristics
        if comment_text.lower() in ["let's go", "lets go", "fuck yes", "fuck yeah"]:
            comment_sent = .7
        elif comment_text.lower() in ["boo"]:
            comment_sent = -1


        # visualize and write
        print([comment_time, comment_team, comment_text, comment_length, comment_sent, comment_subr])

        # filter out profanity
        try:
            comment_text = pf.censor(comment_text)
        except:
            print("UNSUPPORTED CHARACTER IN PROFANITY CENSOR")
            continue

        
        ## keep track of chunks to cleanup after chunk size comments
        if n >= chunk_size:
            print('CHUNK SIZE HIT, STARTING NEW FILE')
            t = time.time()
            n = -1
            with open('D:/repositories/nfl-draft-sentiment/data/comments_temp.csv', 'w', newline='') as csvfile:
                # initiate csv writer
                comment_writer = csv.writer(csvfile, delimiter='\t',
                                        quotechar='|', quoting=csv.QUOTE_MINIMAL)
                # write header
                comment_writer.writerow(["timestamp", "team", "body", "length", "sentiment", "subreddit"])
        
        ## write to temp file
        if n == -1:
            try:
                with open('D:/repositories/nfl-draft-sentiment/data/comments_temp.csv', 'a', newline='') as csvfile:
                    # initiate csv writer
                    comment_writer = csv.writer(csvfile, delimiter='\t', quotechar='|', quoting=csv.QUOTE_MINIMAL)
                    comment_writer.writerow([comment_time, comment_team, comment_text, comment_length, comment_sent, comment_subr])
            except:
                print("ERROR: CONFLICT IN USING TEMP FILE OR UNSUPPORTED CHARACTER")

        # write file
        try:
            with open('D:/repositories/nfl-draft-sentiment/data/comments.csv', 'a', newline='') as csvfile:
                # initiate csv writer
                comment_writer = csv.writer(csvfile, delimiter='\t', quotechar='|', quoting=csv.QUOTE_MINIMAL)
                comment_writer.writerow([comment_time, comment_team, comment_text, comment_length, comment_sent, comment_subr])
                if n !=-1 : 
                    n = n + 1
        except:
            print("ERROR: CONFLICT IN USING FILE OR UNSUPPORTED CHARACTER")

        ## write to new file after 2 minutes
        if n == -1 and time.time() - t >= 15:
            print("CHUNK TIME PASSED, FIXING FILE")
            # swap files
            os.rename('D:/repositories/nfl-draft-sentiment/data/comments.csv',
            'D:/repositories/nfl-draft-sentiment/data/chunks/comments_' + str(num_chunks) + ".csv")
            os.rename('D:/repositories/nfl-draft-sentiment/data/comments_temp.csv','D:/repositories/nfl-draft-sentiment/data/comments.csv')

            # update vars
            n = 0
            num_chunks = num_chunks + 1

    
    # if 503 server overload, pause and try again
    time.sleep(3)
            
