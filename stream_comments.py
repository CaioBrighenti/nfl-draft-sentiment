import praw
import csv
import time
import re
from pycorenlp.corenlp import StanfordCoreNLP
from profanity_filter import ProfanityFilter


## INIT REDDIT INSTANCE
reddit = praw.Reddit()

## INIT PROFANITY FILTER
pf = ProfanityFilter()
pf.censor_whole_words = False

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


with open('C:/Users/Caio Laptop/Documents/Repositories/nfl-draft-sentiment/data/comments.csv', 'w', newline='') as csvfile:
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

# loop through comments
for comment in reddit.subreddit("+".join(team_subreddits)+"+nfl").stream.comments():
    # grab text and remove new lines
    comment_text = comment.body.replace("\n", "")
    comment_text = comment_text.replace("\t", "")
    comment_text = re.sub(r"[^a-zA-Z0-9 !?,.()]+", '', comment_text)
    comment_time = comment.created_utc #time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(comment.created_utc))
    comment_length = len(comment_text)

    # grab subreddit team
    comment_subr = comment.subreddit.display_name
    if comment_subr == "nfl":
        comment_team = comment.author_flair_text
        if (comment_team not in team_names): continue
    else:
        comment_team = teams_dict[comment_subr]
        comment_subr = "team"

    # get sentiment from coreNLP
    try:
        comment_sent = getSentiment(comment.body)
    except:
        print("ERROR: CORENLP FAILED")

     # visualize and write
    print([comment_time, comment_team, comment_text, comment_length, comment_sent, comment_subr])

    # filter out profanity
    comment_text = pf.censor(comment_text)

    try:
        with open('C:/Users/Caio Laptop/Documents/Repositories/nfl-draft-sentiment/data/comments.csv', 'a', newline='') as csvfile:
            # initiate csv writer
            comment_writer = csv.writer(csvfile, delimiter='\t',
                                    quotechar='|', quoting=csv.QUOTE_MINIMAL)
            
            try:
                comment_writer.writerow([comment_time, comment_team, comment_text, comment_length, comment_sent, comment_subr])
            except:
                print("UNSUPPORTED CHARACTER, REMOVING COMMENT")
    except:
        print("ERROR: CONFLICT IN USING FILE")
        
