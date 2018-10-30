% spwoutline.cls - export Org-mode outlines to LaTeX in an indented hierarchy
% Copyright (C) 2010-2012  Sean Whitton

% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.

% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

% Org-mode config to make use of this class:
% (add-to-list 'org-export-latex-classes
%              '("spwoutline"
%                "\\documentclass{spwoutline}"
%                ("\\section{%s}" . "\\section*{%s}")
%                ("\\subsection{%s}" . "\\subsection*{%s}")
%                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
% (setq org-export-latex-low-levels '("\\begin{lowitemize}\\setlength{\\parindent}{2em}" "\\end{lowitemize}" "\\item \\textbf{%s}\\indent %s")) ; four slashes used to be a \\par

%                                         ; used after things like e.g. to prevent a double space
% (setq org-entities-user '(("space" "\\ " nil " " " " " " " ")))

\ProvidesClass{spwoutline}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass[a4paper]{article}

\RequirePackage[protrusion=true,expansion=true]{microtype}
\RequirePackage{multicol}
\RequirePackage[a4paper]{geometry}
\geometry{a4paper,verbose,includehead,tmargin=30pt,headsep=15pt,bmargin=50pt,lmargin=60pt,rmargin=80pt}
\RequirePackage{fancyhdr} % this has to go after the geometry or the header appears too short
%\pagestyle{fancy}
%\RequirePackage{spwmaths}
\RequirePackage[charter]{mathdesign}
\RequirePackage[scaled]{berasans}
\RequirePackage[sf,bf,compact,medium]{titlesec}
%\setlength{\parindent}{0pt} % not sure why this was here and it was
% meaning no indent on top level - be willing to uncomment
\RequirePackage{ellipsis}
\RequirePackage{amsbsy} % boldsymbol
\RequirePackage[norule,splitrule,stable,multiple]{footmisc}

%%% BEGIN BIBTEX STUFF

\RequirePackage[%
authorformat=smallcaps,%
titleformat=italic,%
titleformat=commasep,%
commabeforerest,%
ibidem=strictdoublepage,%
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
    %   \ifthenelse{\equal{#2}{}}{% language missing
    %     {\"U}bers. \transby{} #1%
    %   }{% language given
    %     \ifjboxford\transfrom\else\Transfrom\fi\jbflanguage{#2} ^^fcbers. \transby{} #1%
    %   }%
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

\setlength{\headheight}{15pt}

\pagestyle{fancy}
%\renewcommand{\chaptermark}[1]{\markboth{#1}{}}
\renewcommand{\sectionmark}[1]{\markright{#1}{}}
\newcommand{\chaptermark}[1]{\markboth{#1}{}}

\RequirePackage[onehalf]{spwtitle}

\fancyhead{}
\fancyhead[LE,RO]{\textsf{\thepage}}
\makeatletter\fancyhead[CE]{\textsf{\nouppercase{\@title}}}\makeatother
\fancyhead[CO]{\textsf{\nouppercase{\rightmark}}}
\fancyfoot{}

\fancypagestyle{plain}{ %
\fancyhf{} % remove everything
\renewcommand{\headrulewidth}{0pt} % remove lines as well
\renewcommand{\footrulewidth}{0pt}}

\setlength{\columnsep}{25pt}
\setlength{\columnseprule}{.5pt}

\newenvironment{lowitemize}{\begin{list}{$\quad\boldsymbol{\star}\;$}{%
\setlength{\itemsep}{0pt}%
\setlength{\parsep}{3pt}%
\setlength{\leftmargin}{20pt}%
}}{\end{list}}
\renewenvironment{itemize}{\begin{list}{--}{%
\setlength{\itemsep}{0pt}%
\setlength{\parsep}{3pt}%
\setlength{\leftmargin}{40pt}%
}}{\end{list}}

\renewcommand{\subsubsection}[1]{\par\bigskip\textbf{#1}\bigskip\par}
%\renewcommand{\paragraph}[1]{\par\bigskip\textsc{#1}\bigskip\par}
