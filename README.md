bio3ss3
=======

Code for Bio 3SS3 at McMaster University.

### Installation

* Either via `devtools::install_github()`:
```
install.packages("devtools") ## if not already installed
library("devtools")
install_github("bbolker/bio3ss3")
library("bio3ss3")
```
* Or install from BB's repository:
```
install.packages("bio3ss3",
           repos=c("http://www.math.mcmaster.ca/bolker/R",
               getOption("repos","http://probability.ca/cran")))
library("bio3ss3")
```
* If all else fails, install the required packages yourself and load the R code directly (this won't give you the full capabilities of the package, but enough to get started with):
```
install.packages(c("ggplot2","digest","deSolve"))
library("ggplot2")
library("digest")
library("deSolve")
source("http://www.math.mcmaster.ca/bolker/R/misc/bd.R")
```


To make a (very) basic demographic-parameter plot, use `library("bio3ss3"); bd()`.  For more information, see `?bd`.


