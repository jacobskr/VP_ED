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
longitude = -122.019943
latitude = 37.285989
destination = "1427 Alderbrook Ln San Jose CA 95129"

encodedDest = urllib.parse.quote(destination, safe='')

routeUrl = "http://dev.virtualearth.net/REST/V1/Routes/Driving?wp.0=" + str(latitude) + "," + str(longitude) + "&wp.1=" + encodedDest + "&key=" + bingMapsKey

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

itineraryItemsdf.travelDistance.sum()
itineraryItemsdf.travelDuration.sum() / 60   
