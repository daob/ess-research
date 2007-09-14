rm(list=ls())
thresholds <- read.table("c:/ess/berlin/data/doctors/thresholds.txt", header=T)
means <- read.table("c:/ess/berlin/data/doctors/means.txt", header=T)
questions <- read.table("c:/ess/berlin/data/doctors/questions.txt", header=T, as.is=c(1,2,3))
scales <- read.table("c:/ess/berlin/data/doctors/scales.txt", header=T, as.is=c(1,2,3))

stopifnot(length(unique(means$trait)) == length(unique(thresholds$trait)))
stopifnot(length(unique(means$method)) == length(unique(thresholds$method)))

traits <- unique(means$trait)
methods <- unique(means$method)
countries <- unique(means$country)

numtraits <-  length(traits)
nummethods <- length(methods)
numcountries <- length(countries)

numcategories <- 5
digits <- 1
str <- ""
iquestions <- 1
experiments <- "doctors"
numexperiments <- 1

for (ie in 1:numexperiments) {

for ( it in 1:numtraits ) {
  for( im in 1:nummethods ) {
    quest <- questions[questions$trait==traits[it] & questions$method==methods[im] , 1]
       str <- paste(str, "\n\n`", quest, "'\n\n", sep="" )       
  str <- paste(str, "\\begin{scriptsize}\\begin{tabular*}{\\textwidth}{lp{.12\\textwidth}")
  str <- paste(str, "lp{.10\\textwidth}cp{.10\\textwidth}cp{.10\\textwidth}cp{.10\\textwidth}cp{.10\\textwidth} }\n")
    str <- paste(str, "&  \\parbox{.10\\textwidth}{\\centering 1} & $\\tau_1$ &  \\parbox{.10\\textwidth}{\\centering 2} & $\\tau_2$ &  \\parbox{.10\\textwidth}{\\centering 3} & $\\tau_3$ &  \\parbox{.10\\textwidth}{\\centering 4} & $\\tau_4$ &  \\parbox{.10\\textwidth}{\\centering 5}\\\\\n")
    cats <- scales[scales$trait==traits[it] & scales$method==methods[im] , 1]

    for (ik in 1:numcategories) {
      if (ik > 1)     str <- paste(str, "&")
      str <- paste( str, "& \\parbox{.10\\textwidth}{\\centering \\textit{", cats[ik],"}}"  )
    }
    str <- paste(str, "\\\\\n")
  for( ic in 1:numcountries) {
       tau <- thresholds[thresholds$country==countries[ic] &  thresholds$trait==traits[it] &  thresholds$method==methods[im] ,1]
       nu <- means[means$country==countries[ic] &  means$trait==traits[it] &  means$method==methods[im], 1]
       tau <- tau - nu
       str <- paste( str, countries[ic] )
       for( i in 1:length(tau) ) {
         
         str <- paste( str, "&&", sprintf(paste("%.",digits,"f",sep=""), tau[i]) )
       }
       str <- paste(str, "\\\\\n")       
  } 
  str <- paste(str, "\\end{tabular*}\\end{scriptsize}\n\n\\vspace{12pt}")
}
}

write(str, paste("c:/ess/berlin/output/thresh_", experiments[ie], ".tex",sep=""))

}
