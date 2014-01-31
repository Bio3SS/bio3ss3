library(bio3ss3)
bd()  ## vanilla
bd(N0=1)  ## add dynamics plot
bd(bw=TRUE)  ## black and white
bd(printPlots=FALSE)  ## return list of plots instead
b1 <- bd(fontSize=1,printPlots=FALSE)
b2 <- bd(fontSize=2,printPlots=FALSE)
## bug: fontSize is only acting on second plot.  Why?


## unfortunately don't know how to test Shiny interface programmatically ...
