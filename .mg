;;;; ---- preferences ----

make-backup-files
set-default-mode indent
# auto-execute mutt* auto-fill-mode

; following line doesn't work on Debian's mg
;set-default-mode notab

;;;; ---- bindings ----

global-set-key "\^w" backward-kill-word
global-set-key "\^x\RET" execute-extended-command
global-set-key "\^x\^m" execute-extended-command
global-set-key "\^x\^k" kill-region
