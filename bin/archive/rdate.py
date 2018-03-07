#!/usr/pkg/bin/python2.7
"""
Remembers or restores the original posting date of entry.

Filename may be given in commandline argument, if not so,
program asks for it. 

If dealing with file first time, program reads the modified-time
and stores it as a tag:

#published %Y-%m-%d %H:%M:%S

e.g.

#published 2007-05-15 15:30:28

right after entry title line. The current modified-time of file is
preserved.

If run on the same file again and if the modified-time has been
changed since the time of tag storage, script restores the saved
time (sets it as the current modified-time).

That's all.

Program is rather pyblosxom utility than plugin, so there is no
reason to move it to plugins dir.

There are many ways how to use the script. I personally use CVS as
a primary storage for blog entries. But there is problem - CVS does
not honor mtime. So, I run the rdate.py once on files before adding
them to CVS. Then i update blog from the repository, but, we know, 
cvs does it without correct mtimes. So, after blog update I run the
rdate.py on files once more to restore saved time from #published
tags. Simple bash scripts does this automatically for me, so I care
for blog entry times no more.

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify,
merge, publish, distribute, sublicense, and/or sell copies of the
Software, and to permit persons to whom the Software is furnished
to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

Copyright 2007 David Zejda
"""
__author__ = "David Zejda blogger at zejda dot net"
__version__ = "rememberdate.py,v 0.1 2007/05/15 12:00:00 zejdad"
__url__ = "http://www.zejda.net/"
__description__ = "Remembers or restores the original posting date of entry for PyBlosxom."

import os, datetime, time, sys

if len(sys.argv) > 1:
	filename = sys.argv[1]
else:
	filename = raw_input("filename? ")

if not filename:
	print "No filename given.  Quitting. :-|"
	sys.exit(1)
   
try:
	filestats = os.stat(filename)
except:
	print "File not found.  Quitting. :-( "
	sys.exit(1)
	
print "Targetted file:", filename
filestats = os.stat(filename)
atime, mtime = filestats[7:9]

fmt = "#published %Y-%m-%d %H:%M:%S"

fmtime = datetime.datetime.fromtimestamp(mtime)
fmtime = fmtime.strftime(fmt)
print " current mtime:", fmtime

try:
	try:
		f = open(filename, "r")
		lines = f.readlines();
	except Exception, e:
	    print "Failed to open file for reading.", e
	    print "Quitting :-("
	    sys.exit(1)
finally:
	f.close()

# restore
remembered = lines[1].rstrip()
if (len(lines) > 2) and ( remembered.find("#published") > -1):
	print "   saved mtime:", remembered
	if remembered == fmtime:
		print "Times are equal, quitting :-)"
	else:
		print "Trying to restore remembered mtime."
		ttuple = time.strptime(remembered, fmt)
		print "  parsed tuple:", ttuple
		epoch = time.mktime(ttuple)
		print " epoch seconds:", epoch
		
		os.utime(filename, (atime, epoch))
		print "Succesfully restored :-)"
	sys.exit(0)

# remember
try:
	try:
		f = open(filename, "w")
		i = 0
		for line in lines:
			i = i + 1
			if i == 2:
				f.write(fmtime);
				f.write("\n");
			f.write(line);
	except Exception, e:
	    print "Failed to open file for writing.", e
	    print "Quitting :-("
	    sys.exit(1)
finally:
	f.close()

os.utime(filename, (atime, mtime))

print "Successfully remembered :-)"

