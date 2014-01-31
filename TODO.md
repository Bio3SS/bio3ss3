* get printing done in a sensible place (use tk_choose.dir or choose.dir depending on platform?  store original working directory??  completely separate print dialog?
* add textual info to plot, e.g. putting text in upper right corner can be done by `annotate(geom="text",x=Inf,y=Inf,label=<whatever>,hjust=1,vjust=1)`
    * print param info on plot
* more documentation of models
* add a stripped-down/basic version for students who want to start from scratch
* stochastic models? (realized-r vs density plots?)
* modularize more generally
* show stability info, equilibria?
* integrate other models (theta-logistic, logistic): different parameter sets vis ?conditionalPanel
* expand/collapse parameter table based on "allee' check box?: see ?conditionalPanel
* more generally,
* remove divOffset offset chunk

## Done
* 'print' button rather than checkbox: see https://github.com/rstudio/shiny/issues/167
* hash based on getenv("USER"), or macid name?
* redo reportSim based on N0=0?
* units in help file and on shiny controls
* is there an analytical solution of the exponential-fecundity model (NO)
