
pred <- function(...) {
    comp(...,type="pred")
}

comp <- function(r=c(1,1),K=c(1,1),alpha=c(0,0),
                 ## two-element vectors of parameters
                 N0=c(0.5,0.5),     ## two-element vector *or* nstartx2 matrix
                 type=c("comp","pred"),
                 r.V=1,K.V=Inf,b=1,c=1,d.P=0.5,alpha.P=0,h=0,
                 t.max=50,t.steps=200,
                 arrow.pos=0.05,debug=FALSE) {

    ## make R CMD check happy:
    N <- value <- variable <- X1 <- y <- lab <- NULL  

    type <- match.arg(type)
    if (debug) cat("in comp\n")
    if (type=="comp") {
        parList <- namedList(r,K,alpha)
        s.names <- c("N1","N2")
        grad <- cdot
    } else {
        parList <- namedList(r.V,K.V,b,c,d.P,h,alpha.P)
        s.names <- c("V","P")
        grad <- pdot
    }
    parList <- as.relistable(parList)
    tvec <- seq(0,t.max,length=t.steps)
    odefun <- function(N) {
        ode(setNames(N,s.names),  ## fix state names according to model type
            seq(0,t.max,length=t.steps),grad,unlist(parList))
    }
    if (!is.matrix(N0)) N0 <- rbind(N0)
    if (debug) cat("before ode runs\n")
    res <- setNames(adply(N0,1,odefun),c("X1","time",s.names))
    if (debug) cat("after ode runs\n")
    firstdat <- subset(res,time==0)
    lastdat <- subset(res,time==t.max)
    arrow.ind <- which.min(abs(tvec-t.max*arrow.pos))
    arrowdat <- subset(res,time %in% tvec[c(arrow.ind,arrow.ind+1)])
    arrowdat <- ddply(arrowdat,"X1",transform,time=rank(time))
    ## only works for a single arrow per trajectory right now
    arrow.mdat <- melt(arrowdat,id.var=c(1:2))
    arrow.sdat <- dcast(arrow.mdat,X1~variable+time)
    plot.aes <- do.call(aes_string,as.list(setNames(c(s.names,"X1"),c("x","y","group"))))
    pp_plot <- ggplot(res,plot.aes)+geom_path()
    arrow.aes <- do.call(aes_string,as.list(setNames(names(arrow.sdat)[-1],c("x","xend","y","yend"))))
    pp_plot <- pp_plot + geom_point(data=firstdat,pch=1)+
        geom_point(data=lastdat,pch=16)+
            geom_segment(data=arrow.sdat,
                         mapping=arrow.aes,
                         arrow = arrow(length = unit(0.25,"cm")))
    pp_plot <- pp_plot + expand_limits(x=0,y=0)
    time_plot <- ggplot(melt(res,id.var=1:2),
                        aes(time,value,colour=variable,lty=variable,
                            group=interaction(X1,variable)))+
                           geom_line()+
                               scale_linetype_discrete(name="species")+
                                   scale_colour_brewer(name="species",
                                                       palette="Set1")+
                                labs(y="Population density (ind/m^2)")
    time_plot <- time_plot + expand_limits(y=0)
    list(pp=pp_plot,time=time_plot)
}
pdot <- function(time,vars,parms) {
    ## browser()
    parList <- relist(parms)
    ndot <- with(c(as.list(vars),parList),
             {
                 funcresp <- b/(1+b*V*h)
                 c(V*(r.V*(1-V/K.V)-funcresp*P),
                   P*(c*funcresp*V-d.P-alpha.P*P))
             })
    list(ndot)
}
cdot <- function(time,vars,parms) {
    ## browser()
    parList <- relist(parms)
    ndot <- with(c(list(N=vars),parList),
                 r*N*(1-(N+alpha*rev(N))/K))
    list(ndot)
}


## c1 <- comp(r=c(1,1),K=c(200,100),alpha=c(1,1),
##      N0=c(1,1),arrow.pos=0.05)
## c1$time
## c1$pp

## c2 <- comp(r=c(1,1),K=c(200,100),alpha=c(1,1),
##      N0=matrix(c(1,1,2,1,1,2,3,1,1,3),byrow=TRUE,ncol=2),
##       arrow.pos=0.05)
## c2$pp

## c2$pp+scale_x_log10()+scale_y_log10()
