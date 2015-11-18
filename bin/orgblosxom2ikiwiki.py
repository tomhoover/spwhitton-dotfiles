#!/usr/bin/python
# coding=utf-8

import os
import random
import re
import shutil
import string
import subprocess
import sys
import xml.etree.ElementTree as ET
from PIL import Image           # apt-get install python-imaging

# to run: cd ~/src/wiki && rm -rf blog/entry && g co blog/entry && cd
# ~/lib/wikiannex && git clean -f && rm -rf
# blog/img/{jhcoip,oldtech,oliscrot} && cd $HOME &&
# orgblosxom2ikiwiki.py && rm
# ~/lib/wikiannex/blog/img/{jhcoip,oldtech,oliscrot}/*thumb*

# everything in Unicode please
reload(sys)
sys.setdefaultencoding('utf-8')

# input
POSTS = "/home/swhitton/local/big/blog"
COMMENTS = "/home/swhitton/local/big/comments"
# output
ENTRIES = "/home/swhitton/src/wiki/blog/entry"
IMAGES = "/home/swhitton/lib/wikiannex/blog/img"

def strip_smarts(text):
    return text.replace(u"“", "\"").replace(u"”", "\"").replace(u"’", "\'").replace(u"‘", "\'").replace(u"—", "---").replace(u"–", "--").replace(u"…", "...")

def fix_images(text):
    fixed = []

    for line in text.splitlines():
        match = re.match(r'\[!\[\]\(http://spw.sdf.org/blog/(.*)\)\]\(http://spw.sdf.org/blog/(.*)\)', line)
        if match:
            thumb = match.group(1)
            image = match.group(2)
            contents = os.listdir(os.path.join(POSTS, os.path.dirname(image)))
            exts = map(lambda x: os.path.splitext(x)[1], contents)
            if ".org" not in exts: # dedicated image dir
                link_path = os.path.join(os.path.dirname(image).rsplit("/", 1)[1], os.path.basename(image))
            else:
                link_path = os.path.basename(image)

            im = Image.open(os.path.join(POSTS, thumb))
            im_width, im_height = im.size
            dimensions = str(im_width) + "x" + str(im_height)
            fixed.append("[[!img blog/img/" + link_path + " size=" + dimensions + "]]")
        else:
            fixed.append(line)

    return "\n".join(fixed)

def fix_more(text):
    before, more, after = map(lambda s: s.strip(), text.partition("BREAK"))
    if "\nBREAK\n" in text:
        return "\n".join([before + "\n", "[[!more linktext=\"continue reading this entry\" pages=\"!blog/entry/*\" text=\"\"\"", after, "\"\"\"]]"])
    elif " BREAK " in text:
        return before + " [[!more linktext=\"continue reading this entry\" pages=\"!blog/entry/*\" text=\"\"\"" + after + "\n\"\"\"]]"
    else:
        return text

def convert_post(post):
    with open(post, 'r') as h:
        org = h.read()

    title = org.splitlines()[0].replace('#+HTML: ', '')
    title = strip_smarts(title)
    title = "[[!meta title=\"" + title + "\"]]"

    date = org.splitlines()[1].replace('#+HTML: #published ', '')
    date = "[[!meta date=\"" + date + "\"]]"

    tags = os.path.dirname(post).replace(POSTS, "")[1:].replace("/", " ")
    tags = "[[!tag  imported_PyBlosxom " + tags + "]]"

    # this file generates a pandoc error:
    # /home/swhitton/local/big/blog/linkdump/novdec14.org
    pandoc = subprocess.Popen(["pandoc", "-f", "org", "-t", "markdown_strict"],
                              stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    body, error = pandoc.communicate(input=org)

    body = strip_smarts(body)
    body = fix_images(body)
    body = fix_more(body)

    return "\n".join([date, title, tags, "", body])

def convert_comment(comment):
    # print "attempting to parse", comment
    tree = ET.parse(comment)
    root = tree.getroot()

    # reference:

    # [[!comment format=mdwn
    #  username="spwhitton@171b57686690088a367b4b10ddf73c4ca6f16601"
    #  nickname="spwhitton"
    #  avatar="http://cdn.libravatar.org/avatar/40da86a5d03e6fa62515a9d762601ed2"
    #  subject="And a second one, gravatar free"
    #  date="2015-11-11T00:18:46Z"
    #  content="""
    # Here it is
    # """]]

    slug = os.path.basename(root.find('parent').text)
    address = root.find('email')
    if address == None:
        username = root.find('author').text
    else:
        username = address.text.partition('@')[0]

    desc = root.find('description').text
    pandoc = subprocess.Popen(["pandoc", "-f", "html", "-t", "markdown_strict"],
                              stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    desc, error = pandoc.communicate(input=desc)

    comment = "\n".join([
        "[[!comment format=mdwn",
        " username=\"" + username  + "\"",
        " nickname=\"" + root.find('author').text + "\"",
        " date=\"" + root.find('w3cdate').text + "\"",
        " content=\"\"\"",
        desc + "\"\"\"]]"
    ])

    the_dir = os.path.join(ENTRIES, slug)
    if not os.path.exists(the_dir):
        os.mkdir(the_dir, 0755)
    rands = ''.join(random.choice(string.ascii_uppercase + string.digits) for _ in range(16))
    # ^ http://stackoverflow.com/a/2257449
    if not comment.endswith("\n"):
        comment = comment + "\n"
    with open(os.path.join(ENTRIES, slug, "comment_" + rands + "._comment"), 'w') as h:
        h.write(comment)

def main():
    for root, dirs, files in os.walk(POSTS):

        # skip all the templates stored in root of blog
        if root == POSTS or root.startswith("/home/swhitton/local/big/blog/.git"):
            continue

        # 1. If there's no .org in this dir and we're at the bottom of
        # a tree, then it's a dir for images only, so copy it
        # verbatim.  And we know from inspection with old Haskell
        # script that there are no conflicts other than inside these
        # image-only directories
        exts = map(lambda x: os.path.splitext(x)[1], files)
        if ".org" not in exts and not any(dirs):
            dest = os.path.join(IMAGES, os.path.basename(root))
            if not os.path.exists(dest):
                shutil.copytree(root, dest)
        # 2. now convert posts and images in the usual way
        else:
            for f in files:
                ext = os.path.splitext(f)[1]
                if ext == ".org":
                    # convert_post, unlike convert_comment, relies on
                    # us to decide where to save it
                    post = convert_post(os.path.join(root, f))
                    fname = os.path.join(ENTRIES, os.path.splitext(f)[0] + ".mdwn")
                    if os.path.exists(fname): # safety if inspection wrong
                        print "uh oh!  conflict!  " + fname + " exists!"
                        sys.exit()
                    else:
                        if not post.endswith("\n"):
                            post = post + "\n"
                        with open(fname, 'w') as h:
                            h.write(post)
                elif "thumb." not in f:
                    if os.path.exists(os.path.join(IMAGES, f)): # safety if inspection wrong
                        print "uh oh!  conflict!  " + os.path.join(IMAGES, f) + " exists!"
                        sys.exit()
                    else:
                        shutil.copy(os.path.join(root, f), IMAGES)

    for root, dirs, files in os.walk(COMMENTS):
        for f in files:
            if os.path.splitext(f)[1] == ".cmt":
                # convert_comment does the saving since the post to which
                # the comment is associated is stored within the comment
                convert_comment(os.path.join(root, f))

if __name__ == "__main__":
    main()
