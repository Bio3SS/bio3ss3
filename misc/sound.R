## sound package has been archived on CRAN.

seq2sound <- function(x,duration=0.1,minpitch=220,maxpitch=1760) {
  pitch <- (x-min(x))/diff(range(x))*(maxpitch-minpitch)+minpitch
  do.call(appendSample,lapply(pitch,Sine,dur=duration))
}

if (FALSE) {
  c1 <- cobweb(3.9*x*(1-x),start=0.3,N=200,ylim=c(0,1),from=0,to=1,
               scol="blue",svcol="red",interact=TRUE,)

  
  c1 <- cobweb(3.9*x*(1-x),start=0.3,N=200,ylim=c(0,1),from=0,to=1)
  t1 <- system.time(s <- seq2sound(c1[1:50],duration=0.2)) ## slow (13 secs for 200 steps)
  play(s)

  ##
  dr <- 0.01
  ntot <- 1000
  transient <- 700
  rvec <- seq(1,4,by=dr)
  res <- matrix(nrow=ntot,ncol=length(rvec))
  for (i in seq_along(rvec)) {
    x <- 0.5
    res[,i] <- replicate(ntot,x <<- rvec[i]*x*(1-x))
  }
  tres <- res[-(1:transient),]
  par(las=1,bty="l")
  plot(rvec[col(tres)],tres,pch=".",
       xlab="r",ylab="x")
}
