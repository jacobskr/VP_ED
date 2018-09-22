# -*- coding: utf-8 -*-
"""
Created on Fri Sep 21 19:04:33 2018

@author: User
"""

import pandas as pd
import urllib.request
import json

# Your Bing Maps Key 
bingMapsKey = "AujkYNVsMInKkjbgHl3BrmAa_mWCO6lXqwLqp08P7lSPI_9HTPGdUFSQzHpulOyc"

# input information
origin = "2900 Stone Meadow CT Richmond VA 23228"
destination = "8909 Valley Green Williamsburg VA"

encodedOrig = urllib.parse.quote(origin, safe='')
encodedDest = urllib.parse.quote(destination, safe='')

routeUrl = "http://dev.virtualearth.net/REST/V1/Routes/Driving?wp.0=" + encodedOrig + "&wp.1=" + encodedDest + "&key=" + bingMapsKey

request = urllib.request.Request(routeUrl)
response = urllib.request.urlopen(request)

r = response.read().decode(encoding="utf-8")
result = json.loads(r)

itineraryItems = result["resourceSets"][0]["resources"][0]["routeLegs"][0]["itineraryItems"]

#Turn into pandas df
itineraryItemsdf = pd.DataFrame(itineraryItems)

#from pandas import ExcelWriter
#writer = ExcelWriter('Dir_Test.xlsx')
#dfitineraryItems.to_excel(writer,'Sheet1')
#writer.save()

# Distance in miles from kilometers and time in minutes from seconds
itineraryItemsdf.travelDistance.sum() * 0.621371
itineraryItemsdf.travelDuration.sum() / 60
