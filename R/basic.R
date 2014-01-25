#' run a model in graphical interface
#'
#' run a model in the web browser with sliders etc.
#' 
#' @param modelName name of model to run
runModel <- function(modelName="model1") {
    modelDir <-  system.file("models",modelName,package="bio3ss3")
    if (nchar(modelDir)==0)
        stop("can't find model ",shQuote(modelName))
    message("Type ESCAPE (Windows) or Control-C (MacOS/Linux) in the R console or click the 'Stop' button (in RStudio) to quit")
    runApp(modelDir)
}
