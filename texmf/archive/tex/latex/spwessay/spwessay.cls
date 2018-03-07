\ProvidesClass{spwessay}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass[twoside,a4paper]{article}
%\setlength{\evensidemargin}{63pt}
%\setlength{\oddsidemargin}{\evensidemargin}
\usepackage[margin=1.75in]{geometry}

\RequirePackage[protrusion=true,expansion=true]{microtype}
\RequirePackage{fancyhdr}
\RequirePackage{ifthen}
\RequirePackage{amssymb} % for $\blacksquare$ at end

%\RequirePackage[charter]{mathdesign}
\RequirePackage{pslatex}
%\RequirePackage[scaled]{berasans}

\RequirePackage[sf,bf,compact,medium]{titlesec}
\RequirePackage{textcomp}
\RequirePackage[onehalf]{spwtitle}
\RequirePackage[norule,splitrule,stable,multiple]{footmisc}
\RequirePackage[utf8]{inputenc}
\RequirePackage{ellipsis}
\RequirePackage{stmaryrd}
\RequirePackage{amsmath}

%%% BEGIN BIBTEX STUFFn

\RequirePackage[%
%authorformat=smallcaps,%
titleformat=italic,%
titleformat=commasep,%
commabeforerest,%
ibidem=nostrict,% % used to be strictdoublepage
citefull=first,%
oxford,%
pages=format,% pages=test doesn't seem to work but is otherwise better to use
%idem,%
super,%
opcit,%
% human,%
bibformat=ibidem
]{jurabib}
\makeatletter
\jb@dotfalse
\makeatother

\AddTo\bibsenglish{%
  \def\edbyname{ed.}%
  \def\editorname{(ed.)}%
  \def\editorsname{(eds.)}%
  \def\incollinname{in}%
  \def\inname{in}%
  \def\transby{$\!\!$}%
  \def\bibchaptername{ch.}%
  \def\Bibchaptername{Ch.}%
  \def\Volumename{Vol.}%
  \def\volumename{vol.}%
}

\bibliographystyle{jox}

\renewcommand{\jbbtasep}{ \& }
\renewcommand{\jbbstasep}{ \& }
\renewcommand{\jbbtesep}{ \& }
\renewcommand{\jbbstesep}{ \& }
\renewcommand{\bibbtasep}{ \& }
\renewcommand{\bibbstasep}{ \& }
\renewcommand{\bibbtesep}{ \& }
\renewcommand{\bibbstesep}{ \& }

% to use this put this at end of essay: \nobibliography{/home/swhitton/doc/swhittonfhs}

%%% END BIBTEX STUFF

%%% BEGIN BIBLIOGRAPHY HACKING

\makeatletter
\renewenvironment{thebibliography}[1]
{\subsection*{\bibname}% <-- this line was changed from \chapter* to \section*
  \@mkboth{\MakeUppercase\bibname}{\MakeUppercase\bibname}%
  \list{\@biblabel{\@arabic\c@enumiv}}%
  {\settowidth\labelwidth{\@biblabel{#1}}%
    \leftmargin\labelwidth
    \advance\leftmargin\labelsep
    \@openbib@code
    \usecounter{enumiv}%
    \let\p@enumiv\@empty
    \renewcommand\theenumiv{\@arabic\c@enumiv}}%
  \sloppy
  \clubpenalty4000
  \@clubpenalty \clubpenalty
  \widowpenalty4000%
  \sfcode`\.\@m}
{\def\@noitemerr
  {\@latex@warning{Empty `thebibliography' environment}}%
  \endlist}
\makeatother

\newcommand{\bibname}{Bibliography}

%%% END BIBLIOGRAPHY HACKING

%%% BEGIN JURABIB HACKING

% % want translator's name in smallcaps
% % much commenting out here to fix some pdflatex errors, and since I
% % don't do anything in German
% \DeclareRobustCommand{\translator}[3]{%
%   \unskip\unskip\space%
%   \ifthenelse{\equal{#1}{}}{% translator missing
%     \ifjboxford\transfrom\else\Transfrom\fi\jbflanguage{#2}%
%   }{% translator given
%     % \ifthenelse{\equal{\bbl@main@language}{german}}{% main language german
%     % \ifthenelse{\equal{#2}{}}{% language missing
%     % {\"U}bers. \transby{} #1%
%     % }{% language given
%     %   \ifjboxford\transfrom\else\Transfrom\fi\jbflanguage{#2} ^^fcbers. \transby{} #1%
%     % }%
%     % }{%
%     \ifthenelse{\equal{#2}{}}{% language missing
%       \trans{} \transby{} \textsc{#1}%
%     }{% language given
%       \trans{} \transfrom{}\jbflanguage{#2} \transby{} \textsc{#1}%
%     }%
%     % }%
%   }% dot or not?
%   \ifjbchicago
%   \ifthenelse{\equal{#3}{1}}{% \(type=incollection \and editor given\) \or type=article
%     \bibatsep{}%
%   }{}%
%   \else
%   \ifthenelse{\equal{#3}{1}\and\not\boolean{jboxford}}{%
%     \bibatsep{}%
%   }{}%
%   \fi
% }%

%%% END JURABIB HACKING

\makeatletter
\newcommand*{\textoverline}[1]{$\overline{\hbox{#1}}\m@th$}
\makeatother

% from http://stackoverflow.com/questions/2522173/how-to-get-the-value-of-the-document-title-in-latex
\makeatletter\def\title#1{\gdef\@title{#1}\gdef\THETITLE{#1}}\makeatother
\makeatletter\def\author#1{\gdef\@author{#1}\gdef\THEAUTHOR{#1}}\makeatother
\makeatletter\def\date#1{\gdef\@date{#1}\gdef\THEDATE{#1}}\makeatother

\pagestyle{fancy}

\fancyhead{}
\fancyhead[LE]{\textsf{\thepage} \hfill \textsl{\textsf{\THEAUTHOR}} \hfill}
\fancyhead[RO]{\hfill \textsf{\THETITLE} \hfill \textsf{\thepage}}
\fancyfoot{}
%\fancyfoot[LE,RO]{\textsf{\thepage}}

\renewcommand{\headrulewidth}{0pt}

\fancypagestyle{plain}{ %
\fancyhf{} % remove everything
\renewcommand{\headrulewidth}{0pt} % remove lines as well
\renewcommand{\footrulewidth}{0pt}}

%\renewcommand{\section}[1]{\addtocounter{section}{1}\textbf{\textsection \thesection.} $\quad$\ifthenelse{\equal{#1}{}}{}{ \textsc{#1} $\quad$}}

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
