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

## 
namedList <- function(...) {
    L <- list(...)
    snm <- sapply(substitute(list(...)),deparse)[-1]
    if (is.null(nm <- names(L))) nm <- snm
    if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
    setNames(L,nm)
}

respPlot <- function(pop, b, d, lpos, ylab,
                     plab="Population size", title="",
                     plotDiff=FALSE,
                     logscale=FALSE,
                     legendSize=1,
                     fontSize=1,
                     plotType="ggplot",
                     bw=FALSE){
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
        addHash("base")
    } else {
        if (!plotDiff) {
            ## plot birth & death separately
            b <- rep(b,length.out=length(pop))
            d <- rep(d,length.out=length(pop))
            dd <- data.frame(pop=rep(pop,2),y=c(b,d),
                             lab=rep(c("birth rate","death rate"),
                             each=length(pop)))
            g0 <- if (bw) ggplot(dd,aes(pop,y,linetype=lab))+
                scale_linetype_discrete(name="")
            else ggplot(dd,aes(pop,y,colour=lab))+ 
                scale_colour_brewer(name="",palette="Set1")
        
        } else {
            dd <- data.frame(pop,y=b-d)
            g0 <- ggplot(dd,aes(pop,y))+
                geom_hline(yintercept=0,lty=2)
        }
        g0 <- g0 + geom_line()+labs(x=plab,y=ylab,main=title)
        ## FIXME:: really shouldn't hard-code theme_bw ...
        g0 <- g0 + theme_bw(base_size=12*fontSize)+addHash("ggplot2")
        if (logscale) g0 + scale_y_log10() else g0
    }
}

bdplots <- function(pop, b, d, reportTotal=FALSE,
                    reportDiff = FALSE,
                    title=NULL,
                    fontSize=1, legendSize, plab,
                    plotType="ggplot",...) {

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

    ## FIXME:: not allowed in shiny (but 'base' won't work with shiny anyway)
    if (plotType=="base") par(cex=1.6)
    respPlot(pop, b, d, lpos, ylab, title, fontSize=fontSize,
             legendSize=legendSize, plab=plab, plotType=plotType,
             plotDiff=reportDiff,...)
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

#' @importFrom deSolve ode
popSim <- function (N0, timeMax, steps, parms){
    sim <- as.data.frame(ode(
        y = c(n=log(N0)),
        times = (0:steps)*timeMax/steps,
        func = ndot,
        parms
	))
    sim$N <- exp(sim$n)
    return(sim)
}

#' @importFrom digest digest
addHash <- function(plotType="ggplot2",add=getOption("bdAddHash",TRUE),
                    size=4) {
    hash <- digest(paste(Sys.time(),tempfile(),Sys.getenv("USER")),"crc32")
    if (plotType=="base") {
        if (add) {
            u <- par("usr")
            text(u[1],u[3],hash,adj=c(1,1))
        }
    } else {
        if (add) {
            ## cat("adding label\n")
            return(annotate(geom="text",label=hash,x=-Inf,y=-Inf,
                            hjust=-0.05,vjust=-0.05,size=size))
        } else return(element_blank())
    }
    "abc"  ## test: should be discarded anyway
}

#' basic one-species continuous-time population model
#'
#' show plots of demographic parameters or time dynamics
#' 
#' @param timeMax maximum time for dynamics plot [time]
#' @param steps number of steps for dynamics plot
#' @param popMax maximum population for demographic parameter plot [number]
#' @param popSteps number of steps for demographic parameter plot
#' @param b0 \emph{per capita} birth rate at zero density [1/time]
#' @param bDD characteristic density for exponential decrease in per capita birth rate with increasing population density [number]
#' @param bAllee characteristic scale for Allee effect in birth rate [number]
#' @param d0 \emph{per capita} death rate at zero density [1/time]
#' @param dDD characteristic density for exponential increase in \emph{per capita} death rate with increasing population density [number]
#' @param dAllee characteristic scale for Allee effect in death rate [number]
#' @param N0 initial population size for dynamics plot [number]: if \code{N0}=0, simulations of time dynamics will not be run nor plotted
#' @param reportPcTotal whether to plot \emph{per capita} rates ("p"), total rates ("t"), both ("b"), or neither ("n")
#' @param reportDiff whether to plot the overall growth rate (birth-death) rather than birth and death separately
#' @param fontSize scaled font size
#' @param legendSize scaled legend size (base plots only)
#' @param title plot title
#' @param tlab label for time axis
#' @param plab label for population size axis
#' @param printPlots print plots (alternatively, return a list of plots)?
#' @param plotType "ggplot2" or "base"
#' @param logScale make y-axis logarithmic (for time dynamics only)?
#' @param \dots additional arguments passed down to plotting functions, including \code{bw} for black-and-white plotting
#' @details The basic model considered here is an exponential-density-dependence model, i.e.
#' \deqn{\frac{dN}{dt} = N (b_0 \exp(-N/b_{DD}) - d_0 \exp(N/d_{DD}))}
#' (more details to be added later)
#' @examples
#' bd()     ## basic plot
#' ## set initial population size to something other than 0
#' ## in order to get a dynamics plot
#' bd(N0=1) 
#' @export
bd <- function(b0=1, bDD=NULL, bAllee=NULL,
               d0=0.5, dDD=NULL, dAllee=NULL,
               N0=0,
               logScale=FALSE,
               timeMax=20, steps=100,
               popMax=100, popSteps=100,
               reportPcTotal="b",
               reportDiff=FALSE,
               title="",
               tlab = "Time (years)", plab="Population size (number)",
               fontSize=1,
               legendSize=1,
               printPlots=TRUE,
               plotType="ggplot",
               ...
               ) {

    theme_set(theme_bw())

    ## Device ask should be true if device is interactive
    if (printPlots) {
        oldask <- par("ask")
        on.exit(par(ask=oldask))
        par(ask=grDevices::dev.interactive(orNone = TRUE))
    }

    plotList <- NULL

    ## construct pop vector
    pop <- 1:popSteps*(popMax/popSteps)

    b <- rfun(b0, bDD, bAllee, pop, TRUE)
    d <- rfun(d0, dDD, dAllee, pop, FALSE)

    if (reportPcTotal == "p" || reportPcTotal == "b") {
        plot_pc_demog <- bdplots(pop, b, d, reportTotal=FALSE,
                                 title, fontSize=fontSize, 
                                 legendSize=legendSize, plab=plab,
                                 plotType=plotType,
                                 reportDiff=reportDiff,...)
        if (plotType=="ggplot") {
            if (printPlots) {
                print(plot_pc_demog)
            } else { plotList <- c(plotList,
                                   namedList(plot_pc_demog))
                 }
        }
    }
    
    if (reportPcTotal == "t" || reportPcTotal == "b") {
        plot_total_demog <- bdplots(pop, b, d, reportTotal=TRUE, title,
                                    fontSize=fontSize, 
                                    legendSize=legendSize, plab=plab,
                                    plotType=plotType,
                                    reportDiff=reportDiff,
                                    ...)
        if (plotType=="ggplot") {
            if (printPlots) {
                print(plot_total_demog)
            } else {
                plotList <- c(plotList,
                              namedList(plot_total_demog))
            }
        }
    }

    if(N0>0) {
        sim <- bdsim(N0, timeMax, steps, b0, bDD, bAllee, d0, dDD, dAllee)
        
        if (plotType=="base") {
            ylim <- range(sim$N)
            if (!logScale) ylim[1] <- 0
            plot(sim$time, sim$N,
                 cex.lab=fontSize, cex.axis=fontSize,
                 main=title, xlab = tlab, ylab = "Population",
                 type = "l", ylim = ylim,
                 log = if (logScale) "y" else ""
                 )
            addHash("base")
        } else {
            plot_time <- ggplot(sim,aes(time,N))+geom_line()+
                labs(xlab=tlab,ylab="Population",main=title)+
                    addHash("ggplot2")
            if (logScale) {
                plot_time <- plot_time+scale_y_log10()
            } else {
                ## if NOT log-scaled, expand the limits to include zero
                plot_time <- plot_time+expand_limits(y=0)
            }
            if (printPlots) {
                print(plot_time)
            } else{
                plotList <- c(plotList,
                              namedList(plot_time))
            } 
        }
    }

    if (!printPlots) return(plotList)

    ## return(data.frame(pop, b, d))
}

bdsim <- function(N0=1, timeMax=20, steps=100, b0=1, bDD=NULL,
                  bAllee=NULL, d0=0.5, dDD=NULL, dAllee=NULL){
    parms = list(b0=b0, bDD=bDD, bAllee=bAllee, d0=d0, dDD=dDD, dAllee=dAllee)
    return(popSim(N0, timeMax, steps, parms))
}


