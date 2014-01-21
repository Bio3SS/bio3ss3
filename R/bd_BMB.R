## DESIGN ISSUES:

## * given a population model (logistic, exponential-fecundity with possible
##   Allee effects, theta-logistic; user-specified?
## * alternative plots:
##     per capita birth/death rates
##     per capita population growth rates (birth-death)
##     absolute birth/death rates
##     absolute population growth rates
##     pop size vs time (log or linear scale)
##     growth rate vs time?
## * base or ggplot graphs?
## * print graphs or return list
## * tweak graphics parameters: labels, sizes, min/max?
## * use analytic solution if available?

## Doesn't hurt the make project, helps people who are sourcing
require(deSolve)
require(ggplot2)
theme_set(theme_bw())

namedList <- function(...) {
    L <- list(...)
    snm <- sapply(substitute(list(...)),deparse)[-1]
    if (is.null(nm <- names(L))) nm <- snm
    if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
    setNames(L,nm)
}

# Device ask should be true if device is interactive
## options(device.ask.default=grDevices::dev.interactive(orNone = TRUE))
respPlot <- function(pop, b, d, lpos, ylab,
                     plab="Population size", title="",
                     plotDiff=FALSE,
                     logscale=FALSE,
                     legendSize=1,
                     fontSize=1,
                     plotType="ggplot"){
    ymin <- ifelse(logscale, min(c(b, d)), 0)
    ymax <- max(c(b,min(d)))
    logPar <- ifelse(logscale, "y", "")
    if (plotType=="base") {
        plot(pop, b,
             cex.axis = fontSize, cex.lab=fontSize,
             ylim = c(ymin, ymax),
             xlab = plab,
             ylab = ylab,
             type = "l", lwd=2, col="blue", main=title, log=logPar
             )
        lines(pop, d, lty=2, lwd=2)
        legend(lpos, cex=legendSize,
               legend = c("Birth rate", "Death rate"),
               col = c("blue", "black"),
               lty = c(1, 2)
               )
    } else {
        if (!plotDiff) {
            b <- rep(b,length.out=length(pop))
            d <- rep(d,length.out=length(pop))
            dd <- data.frame(pop=rep(pop,2),y=c(b,d),
                             lab=rep(c("birth rate","death rate"),
                             each=length(pop)))
            g0 <- ggplot(dd,aes(pop,y,colour=lab))
        } else {
            dd <- data.frame(pop,y=b-d)
            g0 <- ggplot(dd,aes(pop,y))+
                geom_hline(yintercept=0,lty=2)
        }
        g0 <- g0 + geom_line()+scale_colour_brewer(name="",palette="Set1")+
                    labs(x=plab,y=ylab,main=title)
        g0 <- g0 + theme_set(theme_bw(base_size=12*fontSize))
        if (logscale) g0 + scale_y_log10() else g0
    }
}

bdplots <- function(pop, b, d, reportTotal=FALSE,
                    reportDiff = FALSE,
                    title=NULL,
                    fontSize=1, legendSize, plab,
                    plotType="ggplot") {

    if (is.null(title))
        title <- if (!reportDiff) "Birth-death plot" else "Growth rate plot"
    ylab <- "Per capita rate (1/t)"
    lpos <- "topright"
    
    if(reportTotal) {
        ylab <- "Total rate (pop/t)"
        lpos <- "bottomright"
        b <- b*pop
        d <- d*pop
    }

    ## FIXME:: not allowed in shiny
    if (plotType=="base") par(cex=1.6)
    respPlot(pop, b, d, lpos, ylab, title, fontSize=fontSize,
             legendSize=legendSize, plab=plab, plotType=plotType,
             plotDiff=reportDiff)
}

## BMB: changed default divOffset to 0
rfun <- function(r0, DD, Allee, pop, birth=TRUE, divOffset=0, mmax=1000){
    mult <- 1 + 0*pop
    if (!is.null(DD)) mult <- mult*exp(pop/DD)
    if (!is.null(Allee))
        mult <- mult*exp((Allee+divOffset)/(pop+divOffset))
    mult <- mmax*mult/(mmax+mult)
    if (birth) {mult <- 1/mult}
    return(r0*mult)
}

ndot <- function(time, vars, parms){
	ndot <- with(as.list(c(vars, parms)),
		rfun(b0, bDD, bAllee, exp(n), TRUE) 
                     - rfun(d0, dDD, dAllee, exp(n), FALSE) 
	)
	list(c(ndot))
}

popSim <- function (N0, MaxTime, steps, parms){
    sim <- as.data.frame(lsoda(
        y = c(n=log(N0)),
        times = (0:steps)*MaxTime/steps,
        func = ndot,
        parms
	))
    sim$N <- exp(sim$n)
    return(sim)
}

#' basic one-species continuous-time population model
#'
#' show plots of demographic parameters or time dynamics
#' 
#' @param N0 initial population size for dynamics plot
#' @param MaxTime maximum time for dynamics plot
#' @param steps number of steps for dynamics plot
#' @param popMax maximum population for demographic parameter plot
#' @param b0 \emph{per capita} birth rate at zero density
#' @param bDD characteristic density for exponential decrease in per capita birth rate with increasing population density
#' @param bAllee characteristic scale for Allee effect in birth rate
#' @param d0 \emph{per capita} death rate at zero density
#' @param dDD characteristic density for exponential increase in \emph{per capita} death rate with increasing population density
#' @param dAllee characteristic scale for Allee effect in death rate
#' @param reportPcTotal whether to plot \emph{per capita} rates ("p"), total rates ("t"), both ("b"), or neither ("n")
#' @param reportSim whether to plot time dynamics
#' @param reportDiff whether to plot the overall growth rate (birth-death) rather than birth and death separately
#' @param popSteps ??
#' @param fontSize scaled font size
#' @param legendSize scaled legend size (base plots only)
#' @param title
#' @param tlab label for time axis
#' @param plab label for population size axis
#' @param returnPlotList return list of plots rather than printing plots?
#' @param plotType "ggplot2" or "base"
#' @param logScale make y-axis logarithmic (for time dynamics)?
bd <- function(N0=NULL, MaxTime=20, steps=100, popMax=100, popSteps=100,
               b0=1, bDD=NULL, bAllee=NULL, d0=0.5, dDD=NULL,
               dAllee=NULL,
               reportPcTotal="b",
               reportSim=FALSE,
               reportDiff=FALSE,
               fontSize=1,
               legendSize=1, title="",
               tlab = "Time (years)", plab="Population size",
               returnPlotList=FALSE,
               plotType="ggplot",
               logScale=FALSE) {

    plotList <- NULL
    pop <- 1:popSteps*(popMax/popSteps)

    b <- rfun(b0, bDD, bAllee, pop, TRUE)
    d <- rfun(d0, dDD, dAllee, pop, FALSE)

    if (reportPcTotal == "p" || reportPcTotal == "b") {
        plot_pc_demog <- bdplots(pop, b, d, reportTotal=FALSE,
                                 title, fontSize=fontSize, 
                                 legendSize=legendSize, plab=plab,
                                 plotType=plotType,
                                 reportDiff=reportDiff)
        if (plotType=="ggplot") {
            if (returnPlotList) plotList <- c(plotList,
                                              namedList(plot_pc_demog))
            else print(plot_pc_demog)
            
        }
    }
    
    if (reportPcTotal == "t" || reportPcTotal == "b") {
        plot_total_demog <- bdplots(pop, b, d, reportTotal=TRUE, title,
                                    fontSize=fontSize, 
                                    legendSize=legendSize, plab=plab,
                                    plotType=plotType,
                                    reportDiff=reportDiff)
        if (returnPlotList) plotList <- c(plotList,
                                          namedList(plot_total_demog))
        else print(plot_total_demog)
    }

    if(!is.null(N0)){
        sim <- bdsim(N0, MaxTime, steps, b0, bDD, bAllee, d0, dDD, dAllee)
        
        if (reportSim) {
            if (plotType=="base") {
                plot(sim$time, sim$N,
                     cex.lab=fontSize, cex.axis=fontSize,
                     main=title, xlab = tlab, ylab = "Population",
                     type = "l", ylim = c(0, max(sim$N)),
                     log = if (logScale) "y" else ""
                     )
            } else {
                plot_time <- ggplot(sim,aes(time,N))+geom_line()+
                    labs(xlab=tlab,ylab="Population",main=title)
                if (logScale) plot_time <- plot_time+scale_y_log10()
                if (returnPlotList) {
                    plotList <- c(plotList,
                                  namedList(plot_time))
                } else print(plot_time)
            }
        }
    }

    if (returnPlotList) return(plotList)

    ## return(data.frame(pop, b, d))
}

bdsim <- function(N0=1, MaxTime=20, steps=100, b0=1, bDD=NULL,
                  bAllee=NULL, d0=0.5, dDD=NULL, dAllee=NULL){
    parms = list(b0=b0, bDD=bDD, bAllee=bAllee, d0=d0, dDD=dDD, dAllee=dAllee)
    return(popSim(N0, MaxTime, steps, parms))
}


