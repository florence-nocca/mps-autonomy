#!/usr/bin/python
# -*- coding: utf-8 -*-
import json
import codecs
import re
import csv

## Open and load data
datafile = open("italian_mps.json")
data = json.load(datafile)
italian_mps_data = codecs.open("datafiles/italian_mps.csv", "a+", "utf-8")

# Find the id of current legislature MPs
ids = []
for membership in data["memberships"]:
    term = membership["legislative_period_id"].encode('ascii','ignore')
    if term == "term/17":
        person_id = membership["person_id"].encode('ascii','ignore')
        area = re.sub("area/", "", membership["area_id"])
        parl_group = re.sub("party/", "", membership["on_behalf_of_id"])
        ids.append([person_id, area, parl_group])

# Get corresponding data        
for person in data["persons"]:
    identifier = person["id"]
    for membership in ids:
     if membership[0] == identifier:
         area = membership[1]
         parl_group = membership[2]
         name = person["name"]
         gender = person["gender"]
         birth_date = re.sub("-.*", "", person["birth_date"])
         if "contact_details" in person:
             contact = person["contact_details"]
             for element in contact:
                 if element["type"] == "twitter":
                     account = element["value"].lower()
             else:
                 account = "none"
         italian_mps_data.write('%s,%s,%s,%s,%s,%s\n' % (name,gender,birth_date,parl_group,area,account))

datafile.close()
