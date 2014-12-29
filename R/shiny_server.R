mkShinyServer <- function(fun,argList,
                          specials,debug=FALSE) {

    ## this is an attempt to encapsulate the common features
    ## of all of the different shinyServer() functions.  I'm probably
    ## going about it the wrong way ... I ought to try to modularize the
    ## bits rather than making it one monolithic function -- the result
    ## of doing it the latter way is that I need the specials() hack
    ## to incorporate things that can't easily be fitted into shiny
    function(input,output) {
        currentplot <- NULL
            
        output$plot <- renderPlot({

            ## retrieve all NULL values in argList from input values
            if (debug) cat("begin shiny server\n")
            fromInput <- names(argList)[(sapply(argList,is.null))]
            if (debug) cat("after fromInput\n")
            eval(specials)
            for (i in fromInput) {
                ## have to use [[-indexing
                if (debug) cat(i,"\n")
                argList[[i]] <- input[[i]]
            }
            if (debug) cat(names(argList),"\n")
            plots <- do.call(fun,argList)
            if (debug) cat("ran plots\n")
            print(currentplot <<- plots[[input$whichPlot]])
        })
        
        observe({
            if (input$printButton == 0)
                return()
            isolate({
                name <- paste0(input$filename, ".png")
                dir <- if (.Platform$OS.type=="windows") choose.dir() else {
                    tcltk::tk_choose.dir()
                }
                fp <- file.path(dir,name)
                cat("Printing to ",fp,"\n")
                ggsave(filename=fp,
                       plot=currentplot, type="cairo-png",width=5,height=5)
            })
        })
    }
}
