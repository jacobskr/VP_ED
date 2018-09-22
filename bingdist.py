# -*- coding: utf-8 -*-
"""
Created on Fri Sep 21 19:04:33 2018

@author: User
"""

import pandas as pd
import urllib.request
import json
import time
# Bing Maps Key (https://www.bingmapsportal.com/) in config file
try:
    import config as cf
except:
    print("No config file found with bingMapsKey, create one or type it in below:")
    cf.bingMapsKey = input('')


def BingDistance(xlfilein, xlfileout):
    #Time the run
    start_time = time.time()
    
    # Input information
    tripdf = pd.read_excel(xlfilein)
    
    # Create dictionary
    traveldict = {"Origin": [], "Destination": [], "Distance": [], "Duration": []}
    
    # Iterate over each origin/destination and append into traveldict
    i = 0
    
    for row in tripdf.itertuples(index=True, name='Pandas'):
        # Bing Maps API address input encoding
        encodedOrig = urllib.parse.quote(tripdf.Origin[i], safe='')
        encodedDest = urllib.parse.quote(tripdf.Destination[i], safe='')
        
        # Bing Maps API URL
        routeUrl = ("http://dev.virtualearth.net/REST/V1/Routes/Driving?wp.0="
                    + encodedOrig + "&wp.1=" + encodedDest + 
                    "&key=" + cf.bingMapsKey)
        
        # Bing Maps URL request and response
        request = urllib.request.Request(routeUrl)
        response = urllib.request.urlopen(request)
        
        # JSON and formatting
        r = response.read().decode(encoding="utf-8")
        result = json.loads(r)
        
        itineraryItems = result["resourceSets"][0]["resources"][0]["routeLegs"][0]["itineraryItems"]
       
        #Turn into pandas df
        itineraryItemsdf = pd.DataFrame(itineraryItems)
    
        # Distance in miles from kilometers and time in minutes from seconds
        dist = itineraryItemsdf.travelDistance.sum() * 0.621371
        dur = itineraryItemsdf.travelDuration.sum() / 60
        
        # Append dictionary
        traveldict["Origin"].append(tripdf.Origin[i])
        traveldict["Destination"].append(tripdf.Destination[i])
        traveldict["Distance"].append(dist)
        traveldict["Duration"].append(dur)
        
        print(i)
        i += 1
    
    travelpd = pd.DataFrame(traveldict)
    
    writer = pd.ExcelWriter(xlfileout)
    travelpd.to_excel(writer,'Sheet1', index = False)
    writer.save()
    
    print("--- %s seconds ---" % (time.time() - start_time))
