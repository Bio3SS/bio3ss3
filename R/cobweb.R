cobweb <- function(expr,start,N=50,
                   scol="black",slty=3,slwd=1,
                   svcol=scol,
                   rcol="gray",rlty=2,
                   add=FALSE,
                   interact=FALSE,
                   ylab=NULL,from,to,
                   xlim,ylim, ...) {
  ## magic taken from curve()
  sexpr <- substitute(expr)
  if (is.name(sexpr)) {
    fcall <- paste(sexpr, "(x)")
    expr <- parse(text = fcall)
    if (is.null(ylab)) 
      ylab <- fcall
  }
  else {
    if (!(is.call(sexpr) && match("x", all.vars(sexpr), nomatch = 0L))) 
      stop("'expr' must be a function or an expression containing 'x'")
    expr <- sexpr
    if (is.null(ylab)) 
      ylab <- deparse(sexpr)
  }
  x <- numeric(N)
  x[1] <- start
  for (i in 2:N) {
    x[i] <- eval(expr,envir=list(x=x[i-1]))
  }
  if (!add) {
    if (missing(from)) {
      if (!missing(xlim)) from <- xlim[1]
      else from <- min(x)
    }
    if (missing(to)) {
      if (!missing(xlim)) to <- xlim[2]
      else to <- max(x)
    }
    eval(substitute(curve(expr,from=from,to=to,xlim=xlim,ylim=ylim,...)))
    abline(a=0,b=1,lty=rlty,col=rcol)
  }
  for (i in 2:N) {
    segments(c(x[i-1],x[i-1]),
             c(x[i-1],x[i]),
             c(x[i-1],x[i]),
             c(x[i],x[i]),col=c(svcol,scol),lty=slty,lwd=slwd)
    if (interact) scan(n=1)
  }
  invisible(x)
}


## function closure to return a Ricker function with specified
##   f0 and scale parameters
## properties of Ricker parameterized in this way:
## N(t+1)=N(t)*f0*exp(-N(t)/scale)
##  1/f0 = exp(-N*/scale)
## N* = scale*log(f0)
## dN/N = f0*exp(-N/scale)-N/scale*f0*exp(-N/scale)
##  at eq = 1 - N/scale
##        = 1 - log(f0)
##  always < 1
##  < 0 if  log(f0)>1
##  < -1 if log(f0)>2

rickerF <- function(f0,scale) {
    function(x) { f0*x*exp(-x/scale) }
}
cobweb2 <- function(fun=rickerF(5,1),
                    start=1,N=50,ymax=5,...) {
    mapplot <- qplot(c(0,ymax), stat="function", fun=fun, geom="line")+
        labs(x="N(t)",y="N(t+1)")+geom_abline(a=0,b=1,lty=2)
    x <- numeric(N)
    x[1] <- start
    for (i in 2:N) {
        x[i] <- fun(x[i-1])
    }
    dynplot <- qplot(1:N,x, geom="line")+
        labs(x="time",y="N(t)")
    cseg1 <- data.frame(x0=x[1:(N-1)],
                        y0=x[1:(N-1)],
                        x1=x[1:(N-1)],
                        y1=x[2:N])
    cseg2 <- data.frame(x0=x[1:(N-1)],
                        y0=x[2:N],
                        x1=x[2:N],
                        y1=x[2:N])
    cobwebplot <- mapplot+annotate(geom="segment",
                                   x=x[1:(N-1)],
                                   xend=x[1:(N-1)],
                                   y=x[1:(N-1)],
                                   yend=x[2:N],
                                   colour="red")+
                                       annotate(geom="segment",
                                                x=x[1:(N-1)],
                                                xend=x[2:N],
                                                y=x[2:N],
                                                yend=x[2:N],
                                                colour="blue")+
                                                    annotate(geom="segment",
                                                             x=x[1],
                                                             xend=x[1],
                                                             y=0,
                                                             yend=x[1],
                                                             colour="red")
    list(cobweb=cobwebplot,map=mapplot,dyn=dynplot)
}

