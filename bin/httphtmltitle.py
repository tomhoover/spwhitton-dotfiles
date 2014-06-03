#!/usr/bin/env python

# take url on the command line, output the HTML page's title if we can

import urllib2
import sys

url = sys.argv[1]
response = urllib2.urlopen(url)
html = response.read()

post_title_opening_tag = html.split('<title>')[1]
title = post_title_opening_tag.split('</title>')[0]

print title
