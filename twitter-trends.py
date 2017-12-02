#!/usr/bin/python

#-----------------------------------------------------------------------
# twitter-trends
#  - lists the current global trending topics
#-----------------------------------------------------------------------


import string
import sys
reload(sys)
sys.setdefaultencoding('utf-8')
from twitter import *

#printable = set(string.printable)
#-----------------------------------------------------------------------
# load our API credentials 
#-----------------------------------------------------------------------
config = {}
execfile("config.py", config)

#-----------------------------------------------------------------------
# create twitter API object
#-----------------------------------------------------------------------
twitter = Twitter(
		        auth = OAuth(config["access_key"], config["access_secret"], config["consumer_key"], config["consumer_secret"]))


#-----------------------------------------------------------------------
# retrieve global trends.
# other localised trends can be specified by looking up WOE IDs:
#   http://developer.yahoo.com/geo/geoplanet/
# twitter API docs: https://dev.twitter.com/rest/reference/get/trends/place
#-----------------------------------------------------------------------
# results = twitter.trends.place(_id = 23424977)
# results = twitter.trends.place(_id = 23424848)
results = twitter.trends.place(_id = 23424975)


print "IND Trends"

# for location in results:
# 	for trend in location["trends"]:
# 		print " - %s" % trend["name"]

for location in results:
	for trend in location["trends"]:
		query = twitter.search.tweets(q=trend["name"], count=10, lang='en')
		print trend["name"]
		for result in query["statuses"]:
			#print trend["name"]
			print "--------"
			refined=result["text"]
			refined = refined.encode("ascii", errors="ignore").decode()
			#''.join(filter(lambda x: x in string.printable, refined)
			#print "(%s) @%s %s" % (result["created_at"], result["user"]["screen_name"], result["text"])
			#print result["text"]
			print refined

#query = twitter.search.tweets(q = "lazy dog")
