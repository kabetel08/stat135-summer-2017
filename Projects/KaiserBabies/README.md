#  Kaiser Babies 

#### About

* The purpose of the project is to apply mathematical statistic and data analysis using R on an pre cleaned data file called Infants. The data can be obtained from the url found in the notes below. Some of its variables include: gestation period, birthweight, parity, age of mother, height, wight, marital status, and smoker status. 

* The project attempts to investigate several factors affecting infants’ health, such as:  the normality of mothers’ weight, the normality of the infants’ gestation period for mothers who smoke and those who have never smoked,  perform linear regression to understand the relationship between the infants’ gestation period and their birthweight, and compare the infants’ birthweight for mothers who smoke versus those have never smoked. 

* Some of the method used include: Confidence Intervals, nonparametric bootstrap, linear regression, two sided t-test, two sided nonparametric Mann-Witney test

* Some of the data visualizations include: histograms, probability plot (qqplot), botplot, residual plot, linear regression 


#### Setup
* R version: 3.4.4
* Operating system: OS X  El Capital; version 10.11.16
* packages: dyplr, ggplot2



#### Notes 

* Created KaiserBabies.csv located in the data directory with the purpose to illustrate the data I will be using without having to run any R script. using 

```
data <- load(url("http://www.stat.berkeley.edu/users/nolan/data/KaiserBabies.rda"))

write.table(Infants, file = "KaiserBabies.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
```

* The instructions for the project are located in `”Final Take Home. Rmd”`

* The final report can be found in report directory, which contains two files and an image directory.  (1) kaiserBabiesReport_files, (2) kaiserBabiesReport.Rmd, (3) kaiserBabiesReport.md. 

* kaiserBabiesReport_files contains all the plots and figures reproduced when running   kaiserBabiesReport.Rmd. 

* code directory contains the coded solutions for the project, with comments.
