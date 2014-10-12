#!/usr/bin/env python

import subprocess
import sys
import os
from readability.readability import Document
import html2text
import urllib
import time
import socket
import tempfile
import shutil

READINGDIR = "/home/swhitton/local/reading"
READINGORG = "/home/swhitton/doc/org/reading.org"

def main():
    url = sys.argv[1]
    page = urllib.urlopen(url)
    # encoding stuff from
    # http://cdn3.brettterpstra.com/downloads/Read2Text1.zip
    try:
        from feedparser import _getCharacterEncoding as enc
    except ImportError:
        enc = lambda x, y: ('utf-8', 1)

    unreadable_html = page.read()
    readable_html = Document(unreadable_html).summary().encode('ascii', 'ignore')
    readable_title = Document(unreadable_html).short_title().encode('ascii', 'ignore')

    encoding = enc(page.headers, readable_html)[0]
    if encoding == 'us-ascii': encoding = 'utf-8'
    data = readable_html.decode(encoding)
    data_title = readable_title.decode(encoding)

    h2t = html2text.HTML2Text()
    h2t.ignore_links = True
    markdown = h2t.handle(data)

    filename = (READINGDIR
                + "/"
                + "".join(x if x.isalnum() else "_" for x in readable_title)
                + str(int(time.time()))
                + ".md")

    try:
        os.mkdir(READINGDIR)
    except OSError:
        pass

    with open(filename, 'w') as markdown_file:
        markdown_file.write("## "
                            + data_title.encode('utf8')
                            + "\n\n"
                            + markdown.encode('utf8'))

    org = """
* TODO [[{url}][{title}]]
:PROPERTIES:
:markdown: [[file:{mdfile}]]
:machine:  {hostname}
:END:""".format(url=url,
                title=readable_title,
                mdfile=filename,
                hostname=socket.gethostname())

    with open(READINGORG, 'a') as org_file:
        org_file.write(org)

    workdir = tempfile.mkdtemp()
    os.chdir(workdir)
    subprocess.call(["pandoc", filename, "-o", "article.epub"])
    subprocess.call(["makemobi", "article.epub", readable_title, ""])
    subprocess.call(["sendtokindle", "article.mobi"])
    shutil.rmtree(workdir)

    dbf = open('/home/swhitton/.tmp-dbus-addr', 'r')
    dbv = dbf.readline()
    dbf.close()
    os.environ['DBUS_SESSION_BUS_ADDRESS'] = dbv
    os.environ['DISPLAY'] = "0:0"
    zenerr = open('/tmp/zenityerr', 'a')
    subprocess.Popen(['/usr/bin/notify-send',
                      '--hint=int:transient:1',
                      filename], stderr=zenerr, env=os.environ)
    zenerr.close()

if __name__ == "__main__":
    main()
