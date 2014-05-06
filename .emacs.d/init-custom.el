(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(org-hide ((t (:foreground "#3f3f3f" :slant normal :weight normal :height 110 :width normal :foundry "xos4" :family "Terminus"))) t)
 '(sr-directory-face ((t (:foreground "#94bff3" :weight bold))) t)
 '(sr-symlink-directory-face ((t (:foreground "#94bff3" :slant italic))) t)
 '(visible-mark-face ((t (:background "#607080"))) t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-fold-env-spec-list (quote (("[comment]" ("comment")) ("[proof]" ("proof")))))
 '(ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(auto-completion-syntax-alist (quote (accept . word)))
 '(cdlatex-math-modify-alist (quote ((98 "\\mathbb" "$\\mathbb" t nil nil) (66 "\\mathbf" "\\textbf" t nil nil))))
 '(cdlatex-math-symbol-alist (quote ((123 ("\\subseteq")))))
 '(custom-safe-themes (quote ("cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default)))
 '(ebib-additional-fields (quote (crossref url annote abstract keywords file timestamp translator booktitle)))
 '(ebib-index-display-fields (quote (title)))
 '(ebib-save-xrefs-first t)
 '(emamux:use-nearest-pane t)
 '(fci-rule-color "#383838")
 '(footnote-narrow-to-footnotes-when-editing t)
 '(footnote-section-tag "")
 '(gnus-alias-add-identity-menu nil)
 '(gnus-alias-allow-forward-as-reply t)
 '(gnus-alias-default-identity "Nucifera")
 '(gnus-alias-identity-alist (quote (("Nucifera" "" "Sean Whitton <sean@silentflame.com>" "" nil nil nil) ("Nexus" "" "Sean Whitton <sean.whitton@balliol.ox.ac.uk>" "Balliol College, Oxford" nil nil nil))))
 '(gnus-alias-identity-rules (quote (("Nexus double match" ("to" "<.+@.+.ox.ac.uk" both) "Nexus"))))
 '(gnus-alias-override-user-mail-address t)
 '(gnus-alias-unknown-identity-rule "Nucifera")
 '(ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "auto$" "ltxpng$" "_region_\\.tex$")))
 '(inhibit-startup-echo-area-message "")
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(openwith-associations (quote (("\\.pdf\\'" "evince" (file)) ("\\.mp3\\'" "mplayer" (file)) ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file)) ("\\.\\(doc\\|docx\\)\\'" "soffice" (file)))))
 '(openwith-confirm-invocation t)
 '(org-agenda-files (quote ("~/doc/org/efl.org" "~/doc/org/korea.org" "~/doc/org/pathfinder2013.org" "~/doc/org/diary.org" "~/doc/org/korean.org" "~/doc/org/summer2012.org" "~/doc/org/Academic.org" "~/doc/org/Drafts.org" "~/doc/org/Sean.org" "~/doc/org/SilentFlame.org" "~/doc/org/TechNotes.org" "~/doc/org/refile.org")))
 '(org-agenda-sticky t)
 '(org-agenda-time-grid (quote ((daily weekly today require-timed) #("----------------" 0 16 (org-heading t)) (800 1000 1200 1400 1600 1800 2000))))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-entities-user (quote (("boxright" "\\boxright" t "" "" "" "") ("space" "\\ " nil " " " " " " " "))))
 '(org-export-language-setup (quote (("en" "Author" "Date" "Table of Contents" "Notes") ("ca" "Autor" "Data" "&Iacute;ndex" "Peus de p&agrave;gina") ("cs" "Autor" "Datum" "Obsah" "Pozn\341mky pod carou") ("da" "Ophavsmand" "Dato" "Indhold" "Fodnoter") ("de" "Autor" "Datum" "Inhaltsverzeichnis" "Fu&szlig;noten") ("eo" "A&#365;toro" "Dato" "Enhavo" "Piednotoj") ("es" "Autor" "Fecha" "&Iacute;ndice" "Pies de p&aacute;gina") ("fi" "Tekij&auml;" "P&auml;iv&auml;m&auml;&auml;r&auml;" "Sis&auml;llysluettelo" "Alaviitteet") ("fr" "Auteur" "Date" "Sommaire" "Notes de bas de page") ("hu" "Szerz&otilde;" "D&aacute;tum" "Tartalomjegyz&eacute;k" "L&aacute;bjegyzet") ("is" "H&ouml;fundur" "Dagsetning" "Efnisyfirlit" "Aftanm&aacute;lsgreinar") ("it" "Autore" "Data" "Indice" "Note a pi&egrave; di pagina") ("ja" "&#33879;&#32773;" "&#26085;&#20184;" "&#30446;&#27425;" "&#33050;&#27880;") ("nl" "Auteur" "Datum" "Inhoudsopgave" "Voetnoten") ("no" "Forfatter" "Dato" "Innhold" "Fotnoter") ("nb" "Forfatter" "Dato" "Innhold" "Fotnoter") ("nn" "Forfattar" "Dato" "Innhald" "Fotnotar") ("pl" "Autor" "Data" "Spis tre&#x015b;ci" "Przypis") ("ru" "&#1040;&#1074;&#1090;&#1086;&#1088;" "&#1044;&#1072;&#1090;&#1072;" "&#1057;&#1086;&#1076;&#1077;&#1088;&#1078;&#1072;&#1085;&#1080;&#1077;" "&#1057;&#1085;&#1086;&#1089;&#1082;&#1080;") ("sv" "F&ouml;rfattare" "Datum" "Inneh&aring;ll" "Fotnoter") ("uk" "&#1040;&#1074;&#1090;&#1086;&#1088;" "&#1044;&#1072;&#1090;&#1072;" "&#1047;&#1084;&#1110;&#1089;&#1090;" "&#1055;&#1088;&#1080;&#1084;&#1110;&#1090;&#1082;&#1080;") ("zh-CN" "&#20316;&#32773;" "&#26085;&#26399;" "&#30446;&#24405;" "&#33050;&#27880;") ("zh-TW" "&#20316;&#32773;" "&#26085;&#26399;" "&#30446;&#37636;" "&#33139;&#35387;"))))
 '(safe-local-variable-values (quote ((indent-tabs-mode . t\,))))
 '(smart-tab-disabled-major-modes (quote (org-mode term-mode latex-mode message-mode)))
 '(smart-tab-using-hippie-expand nil)
 '(tm/backspace-delete-column t)
 '(tm/use-goto-line t)
 '(tm/use-open-next-line nil)
 '(w3m-fill-column 0 t))
