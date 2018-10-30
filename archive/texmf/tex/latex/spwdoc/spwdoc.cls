\ProvidesClass{spwdoc}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass[12pt,a4paper]{article}

\RequirePackage[protrusion=true,expansion=true]{microtype}
\RequirePackage{geometry}
\geometry{includeheadfoot,lmargin=1in,rmargin=1in,tmargin=.7in,bmargin=.7in}
\RequirePackage{fancyhdr}
\RequirePackage{ifthen}
\RequirePackage{amssymb}
%\RequirePackage[charter]{mathdesign}
%\RequirePackage[scaled]{berasans}
\RequirePackage[sf,bf,compact,medium]{titlesec}
\RequirePackage{textcomp}
\RequirePackage[onehalf]{spwtitle}
%\RequirePackage{ellipsis}
\RequirePackage{stmaryrd}
\RequirePackage{amsmath}

%%% BEGIN BIBTEX STUFF

\RequirePackage[%
authorformat=smallcaps,%
titleformat=italic,%
titleformat=commasep,%
commabeforerest,%
ibidem=nostrict,% % used to be strictdoublepage
citefull=first,%
oxford,%
pages=format,% pages=test doesn't seem to work but is otherwise better to use
idem,%
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

%%% BEGIN JURABIB HACKING

% want translator's name in smallcaps
% much commenting out here to fix some pdflatex errors, and since I
% don't do anything in German
\DeclareRobustCommand{\translator}[3]{%
  \unskip\unskip\space%
  \ifthenelse{\equal{#1}{}}{% translator missing
    \ifjboxford\transfrom\else\Transfrom\fi\jbflanguage{#2}%
  }{% translator given
    % \ifthenelse{\equal{\bbl@main@language}{german}}{% main language german
    % \ifthenelse{\equal{#2}{}}{% language missing
    % {\"U}bers. \transby{} #1%
    % }{% language given
    %   \ifjboxford\transfrom\else\Transfrom\fi\jbflanguage{#2} ^^fcbers. \transby{} #1%
    % }%
    % }{%
    \ifthenelse{\equal{#2}{}}{% language missing
      \trans{} \transby{} \textsc{#1}%
    }{% language given
      \trans{} \transfrom{}\jbflanguage{#2} \transby{} \textsc{#1}%
    }%
    % }%
  }% dot or not?
  \ifjbchicago
  \ifthenelse{\equal{#3}{1}}{% \(type=incollection \and editor given\) \or type=article
    \bibatsep{}%
  }{}%
  \else
  \ifthenelse{\equal{#3}{1}\and\not\boolean{jboxford}}{%
    \bibatsep{}%
  }{}%
  \fi
}%

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
\fancyhead[R]{\textsf{\thepage}}
\fancyfoot{}
\renewcommand{\headrulewidth}{0pt}
 
\fancypagestyle{plain}{ %
\fancyhf{} % remove everything
\renewcommand{\headrulewidth}{0pt} % remove lines as well
\renewcommand{\footrulewidth}{0pt}}
