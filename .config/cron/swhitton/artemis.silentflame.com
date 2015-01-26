* * * * * srem cron
0 * * * * srem emacs

*/15 * * * * doccheckin >/dev/null
*/10 * * * * offlineimap -u Quiet
