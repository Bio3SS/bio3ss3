* divOffset?
* 'print' button rather than checkbox: see https://github.com/rstudio/shiny/issues/167
* more documentation of models
* stochastic models? (realized-r vs density plots?)
* modularize more generally
* add textual info to plot, e.g. putting text in upper right corner can be done by `annotate(geom="text",x=Inf,y=Inf,label=<whatever>,hjust=1,vjust=1)`
    * print param info on plot
    * hash based on getenv("USER"), or macid name?
* redo reportSim based on N0=0?
* show stability info, equilibria?
* integrate other models (theta-logistic, logistic): different parameter sets vis ?conditionalPanel
* expand/collapse parameter table based on "allee' check box?: see ?conditionalPanel
* more generally, how do we manage the complexity of the interface?  Shiny is very space-limited ... 
