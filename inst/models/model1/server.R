theme_set(theme_bw())

## server script
## test:
if (FALSE) {
    input <- list(N0=1,b0=2,bDD=10,logScale=FALSE,
                  reportDiff=FALSE,fontSize=1,
                  whichPlot="plot_pc_demog",
                  savePlot=FALSE)
}

shinyServer(function(input, output) {
    output$plot <- renderPlot({
        ## cat(input$fSize,"\n")
        ## with(reactiveValuesToList(input),cat(b0,bDD,bAllee,"\n"))
        plots <- bd(N0=input$N0,
                    b0=input$b0,
                    bDD=input$bDD,
                    bAllee=input$bAllee,
                    d0=input$d0,
                    logScale=input$logScale,
                    reportSim=(input$whichPlot=="plot_time"),
                    reportDiff=input$reportDiff,
                    fontSize=input$fSize,
                    returnPlotList=TRUE)
        if(input$savePlot) {
            name <- paste0(input$filename, ".png")
            ggsave(name, plots[[input$whichPlot]], type="cairo-png")
        }
        print(plots[[input$whichPlot]])
    })
})
