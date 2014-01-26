bio3ss3
=======

Code for Bio 3SS3 at McMaster University.

### Installation

* Either via `devtools::install_github()`:
```
install.packages("devtools")
library("devtools")
install_github("bio3ss3",user="bbolker")
```
* Or install from BB's repository:
```
install.packages("bio3ss3",
           repos=c("http://www.math.mcmaster.ca/bolker/R",
               getOption("repos","http://probability.ca/cran")))
```

### Basic usage

To make a (very) basic demographic-parameter plot, use `library("bio3ss3"); bd()`.  For more information, see `?bd`.


