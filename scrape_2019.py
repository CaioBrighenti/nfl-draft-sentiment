# code from: https://www.osrsbox.com/blog/2019/03/18/watercooler-scraping-an-entire-subreddit-2007scape/
import requests
import json
import re
import time
import csv
from tqdm import tqdm

PUSHSHIFT_REDDIT_URL = "http://api.pushshift.io/reddit"

def fetchObjects(**kwargs):
    # Default paramaters for API query
    params = {
        "sort_type":"created_utc",
        "sort":"asc",
        "size":1000
        }

    # Add additional paramters based on function arguments
    for key,value in kwargs.items():
        params[key] = value

    # Print API query paramaters
    print(params)

    # Set the type variable based on function input
    # The type can be "comment" or "submission", default is "comment"
    type = "comment"
    if 'type' in kwargs and kwargs['type'].lower() == "submission":
        type = "submission"
    
    # Perform an API request
    r = requests.get(PUSHSHIFT_REDDIT_URL + "/" + type + "/search/", params=params, timeout=30)

    # Check the status code, if successful, process the data
    if r.status_code == 200:
        response = json.loads(r.text)
        data = response['data']
        sorted_data_by_id = sorted(data, key=lambda x: int(x['id'],36))
        return sorted_data_by_id

def extract_reddit_data(**kwargs):
    # Grab start and end timestamps
    start_utc = kwargs['start_utc']
    end_utc = kwargs['end_utc']

    # Grab subreddit
    subreddit = kwargs['subreddit']

    # Init loop variables
    max_created_utc = start_utc
    max_id = 0

    # While loop for recursive function
    while 1:
        nothing_processed = True
        # Call the recursive function
        objects = fetchObjects(**kwargs,after=max_created_utc)
        
        # Loop the returned data, ordered by date
        for object in objects:
            id = int(object['id'],36)
            created_utc = object['created_utc']
            if id > max_id and created_utc < end_utc:
                nothing_processed = False
                max_id = id
                if created_utc > max_created_utc: max_created_utc = created_utc
                # Write row to csv writer
                print([created_utc, subreddit, object['id']])
                post_info = [str(created_utc), str(subreddit), str(object['id'])]
                with open("D:/repositories/nfl-draft-sentiment/2019_submissions.csv","a") as f:
                    f.write(",".join(post_info))
                    f.write("\n")
        
        # Exit if nothing happened
        if nothing_processed: return
        max_created_utc -= 1

        # Sleep a little before the next recursive function call
        time.sleep(.5)

with open("D:/repositories/nfl-draft-sentiment/2019_submissions.csv","w+") as f:
    f.write(",".join(["timestamp","subreddit","id"]))
    f.write("\n")

# loop through subreddits
subreddit_list = ["KansasCityChiefs","raiders","DenverBroncos","Chargers","Colts","Tennesseetitans","Texans","Jaguars","bengals","steelers",
"ravens","Browns","miamidolphins","nyjets","buffalobills","Patriots","cowboys","NYGiants","eagles","Redskins","CHIBears","GreenBayPackers",
"detroitlions","minnesotavikings","falcons","Saints","panthers","buccaneers","AZCardinals","49ers","LosAngelesRams","Seahawks","nfl"]
for subreddit in subreddit_list:
    extract_reddit_data(subreddit=subreddit,type="submission", start_utc=1556236800,end_utc=1556258400) # draft night

print("DONE SCRAPING POSTS, STARTING COMMENTS")

## INIT REDDIT INSTANCE
reddit = praw.Reddit()

# initialize csv of comments
with open("D:/repositories/nfl-draft-sentiment/data/2019_comments.csv","w+") as f:
    f.write("\t".join(["timestamp","subreddit","id","flair"]))
    f.write("\n")

# load in submissions
with open("D:/repositories/nfl-draft-sentiment/2019_submissions.csv","r") as f:
    # loop through submissions
    for line in tqdm(f):
        line = line.strip('\n')
        line_list = line.split(",")
        
        # grab components
        line_ts = line_list[0]
        line_subreddit = line_list[1]
        line_id = line_list[2]

        # grab submission from Reddit intsance
        try:
            submission = reddit.submission(id=line_id)
        except:
            print("ERROR: MISSING SUBMISSION AT ID " + line_id)
            continue

        # option to see all comments, continue if none
        try:
            submission.comments.replace_more(limit=None)
        except:
            print("ERROR: NO COMMENTS IN THIS SUBMISSION")
            continue
        
        # iterate through comments
        for comment in submission.comments.list():
            # clean up text
            comment_text = comment.body.replace("\n", "")
            comment_text = comment_text.replace("\t", "")
            comment_text = re.sub(r"[^a-zA-Z0-9 !?,.()]+", '', comment_text)
            comment_time = comment.created_utc

            # grab flair if needed
            if line_subreddit == "nfl":
                comment_flair = comment.author_flair_text
            else:
                comment_flair = "NA"

            # write to file
            #print([comment_time,line_subreddit,comment_flair,comment_text])
            with open("D:/repositories/nfl-draft-sentiment/data/2019_comments.csv","a") as f:
                try:
                    f.write("\t".join([str(comment_time),line_subreddit,comment_flair,comment_text]))
                    f.write("\n")
                except:
                    print("ERROR: NONETYPE IN COMMENT INFO")
                    print([comment_time,line_subreddit,comment_flair,comment_text])