Homework1
================

Bootstrapping the SE of an MLE estimate. n i.i.d. R.V. X1,X2,..., Xn, each with probability fuction P(X=0|tetha)=(2/3)tetha P(X=1|tetha)=(1/3)tetha P(X=2|tetha)=(2/3)(1-tetha) P(X=3|tetha)=(1/3)(1-tetha)

tetha is unkown Use R to bootstrap an approximate SE of the maximum likelihood estimate.

Step 1) \[sample\] From question 1H we have that 10 independent obervations were taken from such a disribution: (3,0,2,1,3,2,1,0,2,1)&lt;= values of a sample of 10 i.i.d. R.V. X1,... X10

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
my_sample <- c(3,0,2,1,3,2,1,0,2,1) #which we will treat as the pop 
num_samples<- 1000
n_0<-0
n_1<-0
n<-length(my_sample)


find_MLE <- function(){
  resample <- my_sample %>% sample(replace=TRUE)
  for (i in resample) {
    if (i == 0){
     n_0 <- n_0+1} 
    else if ( i==1){
    n_1 <- n_1 +1}else {
    next
     }
    n_0
    n_1
  }
  (n_0+n_1)/n
}

#crate a vector of means 
MLES <-  replicate(num_samples, find_MLE())

#find SE of the sample mean
sd(MLES)
```

    ## [1] 0.1464986

``` r
print(sd(MLES))
```

    ## [1] 0.1464986
