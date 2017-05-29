#!/usr/bin/python
# -*- coding: utf-8 -*-

# Packages
import json
from pprint import pprint
import uuid
import codecs

## Open and load data
datafile = open("spanish_mps.json")

data = json.load(datafile)

## Add missing MP to database
# # Make a random UUID to generate MP's id
# gen_id = str(uuid.uuid4())

# # Write biographical details
# mp_persons = {
#     u'family_name': u'Píriz Maya',
#     u'name': u'Víctor Valentín Píriz Maya',
#     u'sources': [
#         {u'url': u'http://www.congreso.es/portal/page/portal/Congreso/Congreso/Diputados/BusqForm?_piref73_1333155_73_1333154_1333154.next_page=/wc/fichaDiputado?idDiputado=352&idLegislatura=12'}
#     ],
#     u'gender': u'male',
#     u'image': u'http://www.congreso.es/wc/htdocs/web/img/diputados/352_12.jpg',
#     u'id': gen_id,
#     u'given_name': u'Víctor Valentín',
#     u'birth_date': u'1975-07-22',
#     u'sort_name': u'Píriz Maya, Víctor Valentín',
#     u'email': u'victor.piriz@congreso.es'}

# # MP's party/district 
# mp_memberships = {
#     "area_id": "area/badajoz",
#     "legislative_period_id": "term/12",
#     "on_behalf_of_id": "GP",
#     "organization_id": "a0ade8ef-8127-47eb-8cb5-9403d5dbeb4c",
#     "person_id": gen_id,
#     "role": "member"
#         }

# # Add informations to data
# data["persons"].append(mp_persons)
# data["memberships"].append(mp_memberships)

# ## Add missing twitter info
# for person in data["persons"]:
#     if "escudero" in person["family_name"].lower():
#         person['contact_details'] = [{u'type': u'twitter',
#                      u'value': u'beatrizescu'}]
        
# ## Push changes to json datafile
# with open('spanish_mps.json', 'w') as outfile:
#          json.dump(data, outfile)

# Write a csv containing MPs twitter
mps_accounts = codecs.open("datafiles/spanish_mps_accounts.csv", "a+", "utf-8")

# Find the id of current legislature MPs
ids = []
for membership in data["memberships"]:
    term = membership["legislative_period_id"].encode('ascii','ignore')
    if term == "term/11":
        person_id = membership["person_id"].encode('ascii','ignore')
        ids.append(person_id)

# Get corresponding data        
for person in data["persons"]:
    identifier = person["id"]
    if identifier in ids:
        name = person["name"]
        if "contact_details" in person:
            contact = person["contact_details"]
            for element in contact:
                if element["type"] == "twitter":
                    account = element["value"].lower()
        else:
            account = "none"
        mps_accounts.write('%s,%s\n' % (name,account))

datafile.close()
