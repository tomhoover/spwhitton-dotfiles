*/15 * * * * chronic doccheckin
*/30 * * * * offline || ( chronic mbsync fastmail && chronic notmuch new )
