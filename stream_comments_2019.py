import csv
import time
import re
from pycorenlp.corenlp import StanfordCoreNLP
from profanity_filter import ProfanityFilter
from textblob import TextBlob
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

# global variables
start_utc=1556236800 # April 25th, 2019, 8pm
init_time=time.time()

## INIT PROFANITY FILTER
pf = ProfanityFilter()
pf.censor_whole_words = False

## INIT VADER SENTIMENT
vader = SentimentIntensityAnalyzer()

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
        print(sen['sentiment'])
        total_sent = total_sent + int(sen["sentimentValue"])
        n = n + 1

    # avoid divide by 0
    if n != 0:
        return total_sent / n
    else:
        raise Exception("Comment length 0")

with open('D:/repositories/nfl-draft-sentiment/data/comments.csv', 'w+', newline='') as csvfile:
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

# open file with comments
with open("D:/repositories/nfl-draft-sentiment/data/2019_comments.csv","r") as f:
    first_row = True
    # loop through comments
    for line in f:
        # skip header
        if first_row: 
            first_row = False
            continue

        # split line into list
        line_list = line.split("\t")

        # grab text and remove new lines
        comment_text = line_list[3].strip("\n")
        comment_time = line_list[0]
        comment_length = len(comment_text)

        # wait until proper time
        te_comment = float(comment_time) - start_utc
        te_stream = time.time() - init_time
        if  te_stream < te_comment:
            print("Comment time after start: %f seconds" % te_comment)
            print("Actual time elapsed: %f seconds" % te_stream)
            print("Waiting for %f seconds" % (te_comment - te_stream))
            #time.sleep(1)
            time.sleep(te_comment - te_stream)
        
        # reset timestamp to current time
        comment_time = int(time.time())

        # grab subreddit team
        comment_subr = line_list[1]
        if comment_subr == "nfl":
            comment_team = line_list[2]
            if (comment_team not in team_names): continue
        else:
            comment_team = teams_dict[comment_subr]
            comment_subr = "team"

        # get sentiment from coreNLP
        try:
            #comment_sent = TextBlob(comment_text).sentiment.polarity
            #comment_sent = getSentiment(comment_text)
            comment_sent = vader.polarity_scores(comment_text)['compound']
        except:
            print("ERROR: SENTIMENT ANALYSIS FAILED")
            continue

        # visualize and write
        print([comment_time, comment_team, comment_text, comment_length, comment_sent, comment_subr])

        # filter out profanity
        comment_text = pf.censor(comment_text)

        try:
            with open('D:/repositories/nfl-draft-sentiment/data/comments.csv', 'a', newline='') as csvfile:
                # initiate csv writer
                comment_writer = csv.writer(csvfile, delimiter='\t',
                                        quotechar='|', quoting=csv.QUOTE_MINIMAL)
                
                try:
                    comment_writer.writerow([comment_time, comment_team, comment_text, comment_length, comment_sent, comment_subr])
                except:
                    print("UNSUPPORTED CHARACTER, REMOVING COMMENT")
        except:
            print("ERROR: CONFLICT IN USING FILE")
            
