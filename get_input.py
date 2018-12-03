import datetime
import requests
import sys

if len(sys.argv) > 1:
    today = int(sys.argv[1])
else:
    today = datetime.datetime.now().day

url = "https://adventofcode.com/2018/day/{0}/input".format(today)

with open("cookie", "r") as cookie_file:
    cookie = cookie_file.read().splitlines()[0]

cookies = {"session": cookie}

r = requests.get(url, cookies=cookies)
with open("input/{0}.txt".format(today), "w") as filename:
    filename.write(r.text)
