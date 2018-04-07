\ProvidesClass{spwdnd}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass[12pt,a4paper,twoside,twocolumn]{article}

\setlength{\columnsep}{1cm}

\RequirePackage[protrusion=true,expansion=true]{microtype}
\RequirePackage{geometry}
\geometry{includehead,rmargin=1.5in,lmargin=.4in,tmargin=.7in,bmargin=1.6in,headheight=.36in,footskip=1.96cm}
\RequirePackage{fancyhdr}
\RequirePackage{ifthen}
\RequirePackage{amssymb}
\RequirePackage[charter]{mathdesign}
\RequirePackage[scaled]{berasans}
\RequirePackage[sf,bf,compact,medium]{titlesec}
\RequirePackage{textcomp}
\RequirePackage[onehalf]{spwtitle}
\RequirePackage{ellipsis}
\RequirePackage{stmaryrd}
\RequirePackage{amsmath}

\RequirePackage{framed}
\RequirePackage{color}
\definecolor{shadecolor}{gray}{0.8}

%% allow lots and lots of images
\RequirePackage{etex}
\reserveinserts{18}
\RequirePackage{morefloats}

\pagestyle{fancy}
 
\fancyhf{}
\fancyhead[EL]{\hskip 0.75in \nouppercase{\leftmark}}
\fancyhead[OR]{\nouppercase{\leftmark} \hskip 0.7in}
\fancyfoot[EL]{\hskip -1.1cm \LARGE\textsf{\thepage}}
\fancyfoot[OR]{\LARGE\textsf{\thepage} \hskip -1.35cm}
\renewcommand{\headrulewidth}{0pt}
 
\fancypagestyle{plain}{ %
  \fancyhf{} % remove everything
  \fancyhead[OR]{\nouppercase{\leftmark} \hskip 0.7in}
  \fancyfoot[OR]{\LARGE\textsf{\thepage} \hskip -1.204cm}
\renewcommand{\headrulewidth}{0pt} % remove lines as well
\renewcommand{\footrulewidth}{0pt}}
