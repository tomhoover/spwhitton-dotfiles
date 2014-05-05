#!/usr/bin/python

import tempfile
import subprocess
import shutil
import os
import sys

def main ():
    workdir = tempfile.mkdtemp()
    bgfiles = {'wotc': os.path.expanduser("~") + r'/lib/annex/rt/complete/Dungeons and Dragons/3.5 Core Rulebooks/Blank WoTC Stationary/D&D Blank Pages.pdf'}

    if not(len(sys.argv) == 3):
        print "wrong number of arguments"
        return 1
    # sys.argv[1] is the bgfiles key, sys.argv[2] is the file to be processed
    if sys.argv[2][-3:] == "tex":
        thepdf = sys.argv[2][:-3] + 'pdf'
    else:
        thepdf = sys.argv[2]

    subprocess.call(['pdftk', bgfiles[sys.argv[1]], 'cat', '1', 'output', workdir + '/odd_bg.pdf'])
    subprocess.call(['pdftk', bgfiles[sys.argv[1]], 'cat', '2', 'output', workdir + '/even_bg.pdf'])
    subprocess.call(['pdftk', thepdf, 'cat', '1-endodd', 'output', workdir + '/odd.pdf'])
    subprocess.call(['pdftk', thepdf, 'cat', '1-endeven', 'output', workdir + '/even.pdf'])
    subprocess.call(['pdftk', workdir + '/odd.pdf', 'background', workdir + '/odd_bg.pdf', 'output', workdir + '/odd_bged.pdf'])
    subprocess.call(['pdftk', workdir + '/even.pdf', 'background', workdir + '/even_bg.pdf', 'output', workdir + '/even_bged.pdf'])
    subprocess.call(['pdftk', 'A=' + workdir + '/odd_bged.pdf', 'B=' + workdir + '/even_bged.pdf', 'shuffle', 'A', 'B', 'output', thepdf])

    shutil.rmtree(workdir)

if __name__ == "__main__":
    main()

