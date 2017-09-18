(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:weight normal :height 120 :width normal :foundry "raster" :family "Terminus"))))
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(fringe ((t (:background nil))))
 '(git-annex-dired-annexed-available ((t (:foreground "color-151"))))
 '(git-annex-dired-annexed-unavailable ((t (:foreground "color-167"))))
 '(org-hide ((t (:foreground "#3f3f3f" :slant normal :weight normal :height 110 :width normal :foundry "xos4" :family "Terminus"))))
 '(sr-directory-face ((t (:foreground "#94bff3" :weight bold))) t)
 '(sr-symlink-directory-face ((t (:foreground "#94bff3" :slant italic))) t)
 '(variable-pitch ((t (:height 1.4 :family "Bitstream Charter"))))
 '(visible-mark-face ((t (:background "#607080"))) t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-fold-env-spec-list (quote (("[comment]" ("comment")) ("[proof]" ("proof")))))
 '(ansi-color-names-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(auto-completion-syntax-alist (quote (accept . word)))
 '(cdlatex-math-modify-alist
   (quote
    ((98 "\\mathbb" "$\\mathbb" t nil nil)
     (66 "\\mathbf" "\\textbf" t nil nil))))
 '(cdlatex-math-symbol-alist (quote ((123 ("\\subseteq")))))
 '(custom-safe-themes
   (quote
    ("1373e3623ed5d758ef06dd19f2c8a736a69a15496c745a113d42230ab71d6b58" "799291799f87afb7a2a55bd63082c58fb58912bee0a6e3d5c1ce0e083ed046c9" "c8ac15f21cc5ac2563387e1186a474443c64e9c3dcac0ea55b61caf1bbba432f" "ff9e6deb9cfc908381c1267f407b8830bcad6028231a5f736246b9fc65e92b44" "cbef37d6304f12fb789f5d80c2b75ea01465e41073c30341dc84c6c0d1eb611d" "e56f1b1c1daec5dbddc50abd00fcd00f6ce4079f4a7f66052cf16d96412a09a9" "cdc7555f0b34ed32eb510be295b6b967526dd8060e5d04ff0dce719af789f8e5" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "b21bf64c01dc3a34bc56fff9310d2382aa47ba6bc3e0f4a7f5af857cd03a7ef7" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default)))
 '(ebib-additional-fields
   (quote
    (crossref url annote abstract keywords file timestamp translator booktitle)))
 '(ebib-index-display-fields (quote (title)))
 '(ebib-save-xrefs-first t)
 '(eimp-ignore-read-only-modes
   (quote
    (gnus-article-mode puzzle-mode tumme-display-image-mode tumme-thumbnail-mode w3m-mode image-mode)))
 '(emamux:use-nearest-pane t)
 '(fci-rule-color "#383838")
 '(footnote-narrow-to-footnotes-when-editing t)
 '(footnote-section-tag "")
 '(gnus-alias-add-identity-menu nil)
 '(gnus-alias-allow-forward-as-reply t)
 '(gnus-alias-default-identity "Nucifera")
 '(gnus-alias-identity-alist
   (quote
    (("Nucifera" "" "Sean Whitton <sean@silentflame.com>" "" nil nil nil)
     ("Nexus" "" "Sean Whitton <sean.whitton@balliol.ox.ac.uk>" "Balliol College, Oxford" nil nil nil))))
 '(gnus-alias-identity-rules
   (quote
    (("Nexus double match"
      ("to" "<.+@.+.ox.ac.uk" both)
      "Nexus"))))
 '(gnus-alias-override-user-mail-address t)
 '(gnus-alias-unknown-identity-rule "Nucifera")
 '(ido-ignore-directories
   (quote
    ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "auto$" "ltxpng$" "_region_\\.tex$")))
 '(inhibit-startup-echo-area-message "")
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(jabber-alert-message-hooks (quote (jabber-message-libnotify jabber-message-scroll)))
 '(jabber-alert-presence-hooks nil)
 '(mairix-file-path "~/local")
 '(mairix-search-file "mairixresults")
 '(notmuch-mua-user-agent-function (quote notmuch-mua-user-agent-full))
 '(openwith-associations
   (quote
    (("\\.pdf\\'" "evince"
      (file))
     ("\\.\\(ogg\\|mp3\\|flac\\)\\'" "vlc"
      (file))
     ("\\.\\(doc\\|docx\\|xls\\|xlsx\\)\\'" "soffice"
      (file))
     ("\\.\\(mkv\\|webm\\)\\'" "vlc"
      (file))
     ("\\.avi\\'" "vlc"
      (file))
     ("\\.mp4\\'" "vlc"
      (file))
     ("\\.ppt\\'" "soffice"
      (file))
     ("\\.\\(ppt\\|pptx\\|potx\\)\\'" "soffice"
      (file))
     ("\\.wmv\\'" "vlc"
      (file))
     ("\\.flv\\'" "vlc"
      (file))
     ("\\.hwp\\'" "hanword"
      (file))
     ("\\.\\(jpg\\|JPG\\|jpeg\\|png\\|gif\\)" "eog"
      (file)))))
 '(org-agenda-sticky t)
 '(org-agenda-time-grid
   (quote
    ((daily weekly today require-timed)
     #("----------------" 0 16
       (org-heading t))
     (800 1000 1200 1400 1600 1800 2000))))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-entities-user
   (quote
    (("boxright" "\\boxright" t "" "" "" "")
     ("space" "\\ " nil " " " " " " " "))))
 '(org-export-language-setup
   (quote
    (("en" "Author" "Date" "Table of Contents" "Notes")
     ("ca" "Autor" "Data" "&Iacute;ndex" "Peus de p&agrave;gina")
     ("cs" "Autor" "Datum" "Obsah" "Pozn\341mky pod carou")
     ("da" "Ophavsmand" "Dato" "Indhold" "Fodnoter")
     ("de" "Autor" "Datum" "Inhaltsverzeichnis" "Fu&szlig;noten")
     ("eo" "A&#365;toro" "Dato" "Enhavo" "Piednotoj")
     ("es" "Autor" "Fecha" "&Iacute;ndice" "Pies de p&aacute;gina")
     ("fi" "Tekij&auml;" "P&auml;iv&auml;m&auml;&auml;r&auml;" "Sis&auml;llysluettelo" "Alaviitteet")
     ("fr" "Auteur" "Date" "Sommaire" "Notes de bas de page")
     ("hu" "Szerz&otilde;" "D&aacute;tum" "Tartalomjegyz&eacute;k" "L&aacute;bjegyzet")
     ("is" "H&ouml;fundur" "Dagsetning" "Efnisyfirlit" "Aftanm&aacute;lsgreinar")
     ("it" "Autore" "Data" "Indice" "Note a pi&egrave; di pagina")
     ("ja" "&#33879;&#32773;" "&#26085;&#20184;" "&#30446;&#27425;" "&#33050;&#27880;")
     ("nl" "Auteur" "Datum" "Inhoudsopgave" "Voetnoten")
     ("no" "Forfatter" "Dato" "Innhold" "Fotnoter")
     ("nb" "Forfatter" "Dato" "Innhold" "Fotnoter")
     ("nn" "Forfattar" "Dato" "Innhald" "Fotnotar")
     ("pl" "Autor" "Data" "Spis tre&#x015b;ci" "Przypis")
     ("ru" "&#1040;&#1074;&#1090;&#1086;&#1088;" "&#1044;&#1072;&#1090;&#1072;" "&#1057;&#1086;&#1076;&#1077;&#1088;&#1078;&#1072;&#1085;&#1080;&#1077;" "&#1057;&#1085;&#1086;&#1089;&#1082;&#1080;")
     ("sv" "F&ouml;rfattare" "Datum" "Inneh&aring;ll" "Fotnoter")
     ("uk" "&#1040;&#1074;&#1090;&#1086;&#1088;" "&#1044;&#1072;&#1090;&#1072;" "&#1047;&#1084;&#1110;&#1089;&#1090;" "&#1055;&#1088;&#1080;&#1084;&#1110;&#1090;&#1082;&#1080;")
     ("zh-CN" "&#20316;&#32773;" "&#26085;&#26399;" "&#30446;&#24405;" "&#33050;&#27880;")
     ("zh-TW" "&#20316;&#32773;" "&#26085;&#26399;" "&#30446;&#37636;" "&#33139;&#35387;"))))
 '(org-odt-preferred-output-format "pdf")
 '(popwin:special-display-config
   (quote
    (("*Miniedit Help*" :noselect t)
     (help-mode :stick t)
     (completion-list-mode :width 80 :position right :noselect t)
     (compilation-mode :noselect t)
     (grep-mode :dedicated nil :stick nil :tail nil)
     (occur-mode :noselect nil)
     ("*Pp Macroexpand Output*" :noselect t)
     ("*Shell Command Output*")
     ("*vc-diff*")
     ("*vc-change-log*")
     (" *undo-tree*" :width 60 :position right)
     ("^\\*anything.*\\*$" :regexp t)
     ("*slime-apropos*")
     ("*slime-macroexpansion*")
     ("*slime-description*")
     ("*slime-compilation*" :noselect t)
     ("*slime-xref*")
     (sldb-mode :stick t)
     (slime-repl-mode)
     (slime-connection-list-mode))))
 '(projectile-mode-line
   (quote
    (:eval
     (format " Pr[%s]"
             (projectile-project-name)))))
 '(safe-local-variable-values
   (quote
    ((spwd20-party
      ("Zahrat" . 4)
      ("Anon" . 2))
     (bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)")
     (wc-word-goal . 750)
     (wc-modeline-format . "%w/%gw")
     (wc-modeline-format . %w/%gw)
     (eval highlight-regexp "^ *")
     (eval pandoc-set-write "latex")
     (eval spw/writing-toggle)
     (indent-tabs-mode . t\,))))
 '(send-mail-function (quote sendmail-send-it))
 '(smart-tab-disabled-major-modes (quote (org-mode term-mode latex-mode message-mode)))
 '(smart-tab-using-hippie-expand nil)
 '(tm/backspace-delete-column t)
 '(tm/use-goto-line t)
 '(tm/use-open-next-line nil)
 '(w3m-fill-column 0 t))
