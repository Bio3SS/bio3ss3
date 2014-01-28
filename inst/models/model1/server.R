theme_set(theme_bw())


shinyServer(function(input, output) {

    currentplot <- NULL
    
    output$plot <- renderPlot({
        ## cat(input$fSize,"\n")
        ## with(reactiveValuesToList(input),cat(b0,bDD,bAllee,"\n"))
        ## BMB: could clean up slightly by using
        ## a modified version of reactiveValuesToList(input)
        ## as the input argument to bd ...
        plots <- bd(b0=input$b0,
                    bDD=input$bDD,
                    bAllee=input$bAllee,
                    d0=input$d0,
                    N0=input$N0,
                    timeMax=input$timeMax,
                    steps=input$timeSteps,
                    logScale=input$logScale,
                    reportDiff=input$reportDiff,
                    fontSize=input$fSize,
                    printPlots=FALSE)
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
            ggsave(file=fp,
                   plot=currentplot, type="cairo-png",width=5,height=5)
        })
    })
})
