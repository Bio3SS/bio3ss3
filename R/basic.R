#' run a model in graphical interface
#'
#' run a model in the web browser with sliders etc.
#' 
#' @param modelName name of model to run
#' @export
#' @importFrom shiny runApp
## don't actually need this here ...
#' @importFrom tcltk tk_choose.dir

runModel <- function(modelName="model1") {
    modelDir <-  system.file("models",modelName,package="bio3ss3")
    if (nchar(modelDir)==0)
        stop("can't find model ",shQuote(modelName))
    message("Type ESCAPE (Windows) or Control-C (MacOS/Linux) in the R console or click the 'Stop' button (in RStudio) to quit")
    runApp(modelDir)
}


#' basic exponential-fecundity gradient function
#'
#' @rdname bd0
#' @param t time
#' @param y state variables
#' @param parms parameter values
#' @export
bdfun0 <- function(t,y,parms) {
    g <- with(as.list(c(y,parms)),    ## magic for using names of parameters
              N*(b0*exp(-N/bDD)-d0))  ## derivative
    list(g,NULL)                      ## more magic
}

#' basic exponential-fecundity simulation
#'
#' @param timeMax maximum time
#' @param steps number of time steps
#' @param N0 starting value
#' @param parms named parameter vector
#' @export
bd0 <- function(timeMax=20,steps=50,N0=1,parms=c(b0=1,d0=0.5,bDD=10)) {
    ode(c(N=N0),                                ## named starting cond
        times=seq(0,timeMax,length.out=steps),  ## reporting times
        bdfun0,                                 ## gradient function
        parms)                                  ## parameters
}
bd0()
