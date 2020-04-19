# code from: https://www.osrsbox.com/blog/2019/03/18/watercooler-scraping-an-entire-subreddit-2007scape/
import requests
import json
import re
import time
import csv

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

    # Open a file and initiate csv writer
    file = open("C:/Users/Caio Laptop/Documents/Repositories/nfl-draft-sentiment/2019_submissions.csv","a")
    file_writer = csv.writer(file, delimiter=',')

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
                file_writer.writerow([created_utc, subreddit, object['id']])
        
        # Exit if nothing happened
        if nothing_processed: return
        max_created_utc -= 1

        # Sleep a little before the next recursive function call
        time.sleep(.5)

file = open("C:/Users/Caio Laptop/Documents/Repositories/nfl-draft-sentiment/2019_submissions.csv","a")
file_writer = csv.writer(file, delimiter=',',quotechar='|', quoting=csv.QUOTE_MINIMAL)
file_writer.writerow(["timestamp","subreddit","id"])

# loop through subreddits
subreddit_list = ["KansasCityChiefs","raiders","DenverBroncos","Chargers","Colts","Tennesseetitans","Texans","Jaguars","bengals","steelers",
"ravens","Browns","miamidolphins","nyjets","buffalobills","Patriots","cowboys","NYGiants","eagles","Redskins","CHIBears","GreenBayPackers",
"detroitlions","minnesotavikings","falcons","Saints","panthers","buccaneers","AZCardinals","49ers","LosAngelesRams","Seahawks","nfl"]
for subreddit in subreddit_list:
    extract_reddit_data(subreddit=subreddit,type="submission", start_utc=1556236800,end_utc=1556258400) # draft night