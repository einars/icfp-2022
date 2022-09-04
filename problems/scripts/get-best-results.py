#!/usr/bin/env python

import requests
import json
import os

api_key = os.getenv('api_key')
if api_key == None:
    api_key = os.getenv('API_KEY')

if api_key == None:
    print()
    raise Exception("api_key env PLEASE!")



authorization = 'Bearer ' + api_key

d = requests.get('https://robovinci.xyz/api/submissions', headers={
                     'Authorization': authorization
                 })
j = json.loads(d.text)

besties = {}
max = 0
for e in j['submissions']:
    status = e['status']
    problem_id = e['problem_id']
    score = e['score']

    if status != 'SUCCEEDED': continue

    if problem_id > max:
        max = problem_id

    if problem_id not in besties:
        besties[problem_id] = score
    elif score < besties[problem_id]:
        besties[problem_id] = score

problem_ids = list(besties.keys())
problem_ids.sort()
total = 0
for problem_id in problem_ids:
    print("{}:\t{}".format(problem_id, besties[problem_id]))
    total += besties[problem_id]

print("Total:\t{}".format(total))

