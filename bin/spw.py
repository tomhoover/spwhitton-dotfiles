"""Sean's helper functions for python scripts in ~/bin"""

import termios
import fcntl
import subprocess
import sys
import os

def try_audible_notification(text):
    """Try to send a notification and play a sound.  Don't do anything if
    can't.

    """
    dev_null = open('/dev/null', 'w')
    try:
        subprocess.Popen(['/usr/bin/notify-send',
                          '--hint=int:transient:1',
                          text], stderr=dev_null, env=os.environ)
        audio_file = os.path.expanduser('~/lib/annex/doc/sounds/beep.wav')
        subprocess.call(['/usr/bin/aplay', audio_file], stderr=dev_null)
    except OSError:
        pass
    except subprocess.CalledProcessError:
        pass
    dev_null.close()

def print_same_line(line=''):
    """Print and then carriage return to stay on the same line"""
    # First clear to end of line: then if last print called this
    # function so cursor is at beginning of line, it'll clear out
    # previous printed string.  Needed in case current line is shorter
    # than previous one
    sys.stdout.write("\033[K")

    sys.stdout.write(line + '\r')
    sys.stdout.flush()

# TODO: better: http://stackoverflow.com/a/6599414
# avoids spinning in a tight loop making the CPU run wild
def getch():
    """Get a single char from the keyboard without curses library.  From
    the Python manual's Library and Extension FAQ

    """
    fileno = sys.stdin.fileno()

    oldterm = termios.tcgetattr(fileno)
    newattr = termios.tcgetattr(fileno)
    newattr[3] = newattr[3] & ~termios.ICANON & ~termios.ECHO
    termios.tcsetattr(fileno, termios.TCSANOW, newattr)

    oldflags = fcntl.fcntl(fileno, fcntl.F_GETFL)
    fcntl.fcntl(fileno, fcntl.F_SETFL, oldflags | os.O_NONBLOCK)

    try:
        while 1:
            try:
                the_char = sys.stdin.read(1)
                break
            except IOError:
                pass
    finally:
        termios.tcsetattr(fileno, termios.TCSAFLUSH, oldterm)
        fcntl.fcntl(fileno, fcntl.F_SETFL, oldflags)
    return the_char
