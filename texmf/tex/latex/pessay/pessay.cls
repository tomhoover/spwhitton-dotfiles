%% ---- Sean's Pandoc essay document class, derived from his spwpaper
%% ---- class.  Some features of the spwpaper class have not yet been
%% ---- implemented here, such as endnotes and some maths symbols.

\ProvidesClass{pessay}

%% ---- Commands to set up line spacing.

\newcommand{\initialdouble}{%
  \RequirePackage[doublespacing]{setspace}%
  \setstretch{2}%
}
\newcommand{\startdouble}{%
  \doublespacing%
  \setstretch{2}%
}
\newcommand{\finishdouble}{\onehalfspacing}

%% ---- Command to set up enumeration

\newcommand{\enumsettings}{%
  \setenumerate[1]{label={\arabic*.},ref={(\arabic*)},itemindent=0.30in,labelsep=1em,itemsep=-1em,before=\vspace{-0.5em},after=\vspace{-0.5em}}%
}

%% ---- Options for our document class.

%% One-half spacing

\DeclareOption{onehalf}{%
  \renewcommand{\initialdouble}{%
    \RequirePackage{setspace}%
    \onehalfspacing%
  }%
  \renewcommand{\startdouble}{}%
}

%% Pseudo double spacing

\DeclareOption{pseudodouble}{%
  \renewcommand{\initialdouble}{%
    \RequirePackage[doublespacing]{setspace}%
    \onehalfspacing%
  }%
  \renewcommand{\startdouble}{\doublespacing}%
}

%% Single spacing

\DeclareOption{single}{%
  \renewcommand{\initialdouble}{%
    \RequirePackage{setspace}%
  }%
  \renewcommand{\startdouble}{}%
  \renewcommand{\finishdouble}{}%
}

%% A4 paper instead of (default) letter paper

\newcommand{\papersettings}{%
  \LoadClass[12pt,letterpaper]{article}%
  \RequirePackage[letterpaper,headheight=0.5in,margin=1in,nofoot]{geometry}%
}

\DeclareOption{a4paper}{%
  \renewcommand{\papersettings}{%
    \LoadClass[12pt,a4paper]{article}%
    \RequirePackage[a4paper,headheight=0.5in,margin=1in,nofoot]{geometry}%
  }%
}

\newcommand{\notesandrefs}[1]{\noteshere
  \bibhere{#1}}
\newcommand{\doendnotes}{
  \renewcommand{\footnote}[1]{\endnote{##1}}}
\DeclareOption{footnotes}{\renewcommand{\notesandrefs}[1]{\bibhere{#1}}\renewcommand{\doendnotes}{}}

%% Enable enum numbering resume

\DeclareOption{resume}{%
  \renewcommand{\enumsettings}{%
    \setenumerate[1]{resume,label={\arabic*.},ref={(\arabic*)},itemindent=0.30in,labelsep=1em,itemsep=-1em,before=\vspace{-0.5em},after=\vspace{-0.5em}}%
  }%
}

%% ---- Pass options to the article class and then load it with
%% ---- \papersettings (defined just above)

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\papersettings

%% ---- Load all the other packages we'll need

\RequirePackage{amsmath}        % load it before loading fonts
\RequirePackage{enumitem}
\RequirePackage[british]{babel}
\RequirePackage{newtxmath}
\RequirePackage{newtxtext}      % would be good to use 'osf' option for old style figures: see newtxdoc.pdf
\RequirePackage[sc,sf,bf,compact,medium]{titlesec}
\RequirePackage{textcomp}
\RequirePackage[norule,splitrule,stable]{footmisc}
\RequirePackage{fnpct}
%% \RequirePackage[utf8]{inputenc}
%% \RequirePackage{ellipsis}
%% \RequirePackage{stmaryrd}
\RequirePackage{ifthen}
%% \RequirePackage[all]{nowidow}
\RequirePackage{fancyhdr}
\initialdouble                  % defined above; loads setspace package

%% ---- Useful enumerations

\enumsettings

%% ---- Sectioning

%% First define my custom sectioning command for essay sections

%% see http://tex.stackexchange.com/a/8755

\newcommand{\essaysection}[1]{%
  \par
  \ifthenelse{\equal{\thesection}{0}}{}{\bigskip}
  \pagebreak[2]%
  \refstepcounter{section}%
    \everypar={%
      {\setbox0=\lastbox}       % remove the indentation
      \thesection. $\quad$%
      \ifthenelse{\equal{#1}{}}{}{ \textsl{#1} $\quad$}
      \everypar={}%
    }%
  \ignorespaces
}

%% Now set up \section* to point to the old \section*, but \section to
%% point to my \essaysection.  This works well with pandoc-citeproc.

%% see http://tex.stackexchange.com/a/77906

\makeatletter
\let\latex@section\section
\def\section{\secdef\my@section{\latex@section*}}
\def\my@section[#1]#2{\essaysection{#2}}
\makeatother

%% ---- Headers and footers

\pagestyle{fancy}
\fancyhf{}
\fancyhead[R]{\textsf{\thepage}}
\fancypagestyle{plain}{ %
  \fancyhf{} % remove everything
  \renewcommand{\headrulewidth}{0pt} % remove lines as well
  \renewcommand{\footrulewidth}{0pt}}
\renewcommand{\headrulewidth}{0pt}

%% ---- Document title

\makeatletter\renewcommand{\maketitle}{\finishdouble\thispagestyle{plain}
  \begin{center}\textsf{\@title}

  
  \textsf{\@author}\ifthenelse{\equal{\@date}{}}{}{$\qquad\quad$}\textsf{\@date}

\end{center}\startdouble
}\makeatother

%% ---- Style inline quotations

\expandafter\def\expandafter\quote\expandafter{\quote\onehalfspacing\small}
