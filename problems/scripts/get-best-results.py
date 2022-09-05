#!/usr/bin/env python

import requests
import json
import os

import sys

api_key = os.getenv('api_key')
if api_key == None:
    api_key = os.getenv('API_KEY')

if api_key == None:
    print()
    raise Exception("api_key env PLEASE!")


def prettier_time(t):
    return '{}.sep {}'.format(t[9], t[11:16])

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

    if problem_id not in besties or score < besties[problem_id]['score']:
        besties[problem_id] = {
            'id': e['id'],
            'score': score,
            'submitted_at': e['submitted_at']
        }

problem_ids = list(besties.keys())
problem_ids.sort()
total = 0
for problem_id in problem_ids:
    print("{}:\t{}\t{}".format(problem_id, prettier_time(besties[problem_id]['submitted_at']), besties[problem_id]['score']))
    total += besties[problem_id]['score']


print("Total:\t{}".format(total))

if len(sys.argv) > 1 and sys.argv[1] == '--fetch':
    output = '../../solutions'
    os.makedirs(output, exist_ok = True)
    sys.stderr.write('Fetching solutions...')
    sys.stdout.flush()
    for problem_id in problem_ids:
        b = besties[problem_id]
        d = requests.get('https://robovinci.xyz/api/submissions/' + str(b['id']), headers={ 'Authorization': authorization })
        j = json.loads(d.text)
        url =j['file_url']
        sol = requests.get(url)
        with open(output + '/' + str(problem_id) + '.txt', 'w') as f:
            f.write(sol.text)
        sys.stderr.write('.')
        sys.stderr.flush()


