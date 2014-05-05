\ProvidesClass{spwpaper}

\newcommand{\initialdouble}{\RequirePackage[doublespacing]{setspace}\setstretch{2}}
\newcommand{\startdouble}{\doublespacing\setstretch{2}}
\newcommand{\finishdouble}{\onehalfspacing}
\DeclareOption{onehalf}{\renewcommand{\initialdouble}{\RequirePackage{setspace}\onehalfspacing}\renewcommand{\startdouble}{}}
\DeclareOption{pseudodouble}{\renewcommand{\initialdouble}{\RequirePackage[doublespacing]{setspace}\onehalfspacing}\renewcommand{\startdouble}{\doublespacing}}
\DeclareOption{single}{\renewcommand{\initialdouble}{\RequirePackage{setspace}}\renewcommand{\startdouble}{}\renewcommand{\finishdouble}{}}

\newcommand{\notesandrefs}[1]{\noteshere
  \bibhere{#1}}
\newcommand{\doendnotes}{
  \renewcommand{\footnote}[1]{\endnote{##1}}}
\DeclareOption{footnotes}{\renewcommand{\notesandrefs}[1]{\bibhere{#1}}\renewcommand{\doendnotes}{}}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass[12pt,a4paper]{article}
\RequirePackage[a4paper,headheight=0.5in,margin=1in,nofoot]{geometry}
% \RequirePackage[headings]{fullpage}

\RequirePackage{enumitem}
\setenumerate[1]{resume,label={\arabic*.},ref={(\arabic*)},itemindent=0.30in,labelsep=1em,itemsep=-1em,before=\vspace{-0.5em},after=\vspace{-0.5em}}

\initialdouble % replaces below four lines to allow it to be disabled
               % in class options
%%% \RequirePackage[doublespacing]{setspace}
%%% % \doublespacing
%%% % \renewcommand{\baselinestretch}{2}
%%% \setstretch{2} % setspace *doesn't* get doublespacing right
\setlength\parindent{.40in}

\RequirePackage[protrusion=true,expansion=true]{microtype}
\RequirePackage{fancyhdr}
\RequirePackage{ifthen}

\DeclareSymbolFont{symbolsC}{U}{txsyc}{m}{n}
\DeclareMathSymbol{\strictif}{\mathrel}{symbolsC}{74}
\DeclareMathSymbol{\boxright}{\mathrel}{symbolsC}{128} % from
% http://www.latex-community.org/forum/viewtopic.php?f=48&t=6262
\RequirePackage{wasysym}

\RequirePackage{pslatex}
%\RequirePackage[scaled]{berasans}

\RequirePackage[sf,bf,compact,medium]{titlesec}
\RequirePackage{textcomp}
\RequirePackage[norule,splitrule,stable,multiple]{footmisc}
\RequirePackage[utf8]{inputenc}
\RequirePackage{ellipsis}
\RequirePackage{stmaryrd}

\RequirePackage{endnotes}
\doendnotes

\RequirePackage[all]{nowidow}

\pagestyle{fancy}

\fancyhf{}
\fancyhead[R]{\textsf{\thepage}}

\fancypagestyle{plain}{ %
  \fancyhf{} % remove everything
  \renewcommand{\headrulewidth}{0pt} % remove lines as well
  \renewcommand{\footrulewidth}{0pt}}

\renewcommand{\headrulewidth}{0pt}

\makeatletter\renewcommand{\maketitle}{\finishdouble\thispagestyle{plain}
  \begin{center}\textsf{\@title}

  
  \textsf{\@author}\ifthenelse{\equal{\@date}{}}{}{$\qquad\quad$}\textsf{\@date}

  % \hfill\large\textsf{\@author}\par\hfill\large\textsf{\@date}\normalsize
  % \vspace{8.5pt}
\end{center}\startdouble
}\makeatother

% following is the above with no blank line - as it should be, rather
% than requiring one to not end a para
% http://tex.stackexchange.com/questions/8657/prevent-a-paragraph-break-after-a-section-heading
\renewcommand{\section}[1]{%
  \par
  \ifthenelse{\equal{\thesection}{0}}{}{\bigskip}
  \pagebreak[2]%
  \refstepcounter{section}%
    \everypar={%
      {\setbox0=\lastbox}% Remove the indentation
%      \addcontentsline{toc}{section}{%
%        {\protect\makebox[0.3in][r]{\textsection \thesection.} \hspace*{3pt}}}%
%     \textbf{\textsection \thesection.} $\quad$% old version with \textsection symbol
      \thesection. $\quad$%
      \ifthenelse{\equal{#1}{}}{}{ \textsl{#1} $\quad$}% textsl here used to be textsc
%      \textsc{\thesubsection\space\space{#1} }%
      \everypar={}%
    }%
  \ignorespaces
}

% endnote and quotation formatting, some from Gregory Wheeler's
% philosophy.sty
\makeatletter
\renewcommand{\@makeenmark}{\hbox{$^{\tt\@theenmark}$}}

\newcommand{\noteshere}{%
  \begingroup
  \singlespacing
  \parindent 0pt
  \parskip 2ex
  \def\enotesize{\normalsize}
  \def\enoteformat{\rightskip=.75in \leftskip\z@ \parindent=0em
    \leavevmode\llap{\hbox{$^{\tt \@theenmark}$}}}%(tt) added, from AC's brain_damage.sty
  \theendnotes
  \endgroup}
\renewcommand{\enoteformat}{\singlespacing\rightskip\z@ \leftskip\z@ \parindent=.40in
  \leavevmode\llap{\hbox{$^{\@theenmark}$}}}
\renewcommand{\enoteheading}{\subsection*{\textsc{\notesname}
    \@mkboth{\uppercase{\notesname}}{\uppercase{\notesname}}}}
  % \leavevmode\par\vskip-\baselineskip}
\makeatother

\renewenvironment{quote}% No extra indent on first line 
{\list{}% 
  \small\item[]} 
{\endlist}

% bibliography management
% \RequirePackage{bibtex}
\RequirePackage[authoryear,sort,elide]{natbib}
% \RequirePackage{chicago}
\bibliographystyle{spwchicago}

\makeatletter \let\citeN\citealt \makeatother % fixes situations where
                                % citing multiple articles from one
                                % edited collection

\newcommand{\bibhere}[1]{%
  \begingroup
  \singlespacing
  \parindent 0pt
  \parskip 2ex
  \bibliography{#1}
  \endgroup}

\setlength{\itemindent}{5em}
\makeatletter
\def\thebibliography#1{\subsection*{\textsc{\refname}\@mkboth
    {\uppercase{\refname}}{\uppercase{\refname}}}\list
  {[\arabic{enumi}]}{\settowidth\labelwidth{[#1]}
    \leftmargin\labelwidth
    \advance\leftmargin\labelsep
    % \advance\leftmargin\bibindent
    \advance\leftmargin.35em
    \itemindent -\bibindent
    \listparindent \itemindent
    \parsep \z@
    \usecounter{enumi}}
  \def\newblock{}
  \sloppy
  \sfcode`\.=1000\relax}
\makeatother
%\setcitestyle{aysep={}}
\bibpunct{(}{)}{;}{a}{}{,}

%% get resizeable Quine Corner Quotes
\makeatletter
\DeclareFontFamily{OMX}{MnSymbolE}{}
\DeclareSymbolFont{MnLargeSymbols}{OMX}{MnSymbolE}{m}{n}
\SetSymbolFont{MnLargeSymbols}{bold}{OMX}{MnSymbolE}{b}{n}
\DeclareFontShape{OMX}{MnSymbolE}{m}{n}{
  <-6>  MnSymbolE5
  <6-7>  MnSymbolE6
  <7-8>  MnSymbolE7
  <8-9>  MnSymbolE8
  <9-10> MnSymbolE9
  <10-12> MnSymbolE10
  <12->   MnSymbolE12
}{}
\DeclareFontShape{OMX}{MnSymbolE}{b}{n}{
  <-6>  MnSymbolE-Bold5
  <6-7>  MnSymbolE-Bold6
  <7-8>  MnSymbolE-Bold7
  <8-9>  MnSymbolE-Bold8
  <9-10> MnSymbolE-Bold9
  <10-12> MnSymbolE-Bold10
  <12->   MnSymbolE-Bold12
}{}
\DeclareMathDelimiter{\ulcorner}
                     {\mathopen}{MnLargeSymbols}{'036}{MnLargeSymbols}{'036}
\DeclareMathDelimiter{\urcorner}
                     {\mathclose}{MnLargeSymbols}{'043}{MnLargeSymbols}{'043}
\makeatother
