SHELL=/bin/sh
PATH=/home/spwhitton/local/bin:/home/spwhitton/bin:/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
HOME=/home/spwhitton
LOGNAME=spwhitton

1       60      mairix  nice ionice -c 3 sh -c mairix 2>/dev/null
