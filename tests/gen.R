library("bio3ss3")
p1 <- bd(bDD=1,b0=40,d0=1,discrete=TRUE,
         reportDiff=TRUE,popMax=15,N0=5,
         timeMax=1000,steps=1000,cobweb=TRUE,
         printPlots=FALSE)

## dev.new()
curve(15*x*exp(-x),from=0,to=15)
p0 <- list(b0=2, bDD=2, d0=1, dDD=NULL,
           bAllee=NULL, dAllee=NULL)
p0 <- list(b0=15, bDD=1, d0=1, dDD=NULL,
           bAllee=NULL, dAllee=NULL)
nt <- 100
tail(popSim(5,nt,nt, p0, discrete=FALSE))
head(popSim(5,nt,nt, p0, discrete=TRUE))
15*exp(-1)
15*exp(-1.98)
## ndot(0,c(n=5),p0)
