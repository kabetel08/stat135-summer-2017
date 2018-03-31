#############
### SETUP ###
#############

#the function load, loads a dataset into your workspace. It outputs a vector of all dataframes in the data set. In this case there is just one data set called infants. 
data <- load(url("http://www.stat.berkeley.edu/users/nolan/data/KaiserBabies.rda"))
data
#you can see the variables in this data frame using the head command.
head(infants)

##################
### QUESTION 1 ###
##################

set.seed(7)
mysample=sample(na.omit(infants$wt),100)

####################
### QUESTION 1.A ###
####################
sample_avg_seed<- mean(mysample)
sd_sample_seed <- sd(mysample) 
n<- 100
SE<- sd_sample_seed/sqrt(n)
SE

lower_bound <-sample_avg_seed - (qnorm(.975))*SE
upper_bound <-sample_avg_seed + (qnorm(.975))*SE
CI<- c(lower_bound, upper_bound)

####################
### QUESTION 1.B ###
####################

install.packages("dplyr") 
library(dplyr)
pop<- na.omit(infants$wt)
num_samples <- 1000
sample_size <- 100
my_sample <- sample(pop,sample_size)

findE<- function(){
  resample <- sample(na.omit(infants$wt),100)
  sample_avg <- mean(resample)
  SE<-sd(resample)/10
  An<- c(sample_avg, SE)
}

S <-  replicate(num_samples, findE())
lbound<- S[1,] - (qnorm(.975))*S[2,]
ubound<- S[1,] + (qnorm(.975))*S[2,]

my_data <- data.frame(lbound,ubound)

trueAvg<- mean(na.omit(infants$wt))

TFAvgInCI<- (my_data[,2] > trueAvg & my_data[,1] < trueAvg )%>%summary(tot=sum(n))

#A
print(paste("We expect the true average to appear 950 times"))


#B
print(paste("The true average appears", TFAvgInCI[3], "times"))

####################
### QUESTION 1.C ###
####################

#using fuction from last problem 
S <-  replicate(num_samples, findE())
means<- S[1,]
sampleSD <- sd(means)
sampleSD

hist(means)

qqnorm(means)
 

##################
### QUESTION 2 ###
##################

#Starting with your original sample do the following.

####################
### QUESTION 2.A ###
####################

install.packages("ggplot2") 
library(ggplot2)
sample_size <- 100
num_samples<-1000
pop <- na.omit(infants$wt)
my_sample <- sample(pop,sample_size)

find_mean <- function(){
  resample <- my_sample %>% sample(replace=TRUE)
  mean(resample)
}

means_vec <-  replicate(num_samples, find_mean())
means_df<- data.frame(means_vec)


average_sample_distribution<- mean(means_vec)
average_sample_distribution
sd<- sd(means_vec)

plot<- means_df%>% ggplot(aes(x=means_vec)) + 
  geom_histogram(binwidth =1,col="black",fill="white") + 
  labs(title="Histogram of averages",x="averages",y="count") + theme(
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 25),
    axis.title=element_text(size=20)) +geom_vline(xintercept= average_sample_distribution, color= "red")


####################
### QUESTION 2.B ###
#################### 

bootstrapCI <- 2*average_sample_distribution-quantile(means_vec, c(.975,.025))

##################
### QUESTION 3 ###
##################

#We will now use the 1236 observations in the data set `infants` to perform a linear regression analysis.


####################
### QUESTION 3.A ###
#################### 
# (x=`gestation`) --> gestation period 
# (y=`bwt`) --> baby's birthweight 

x <- infants$gestation
y<- infants$bwt
df <- data.frame(x,y)
df<-na.omit(df)

### ORIGINAL MODEL ###
OriginalModel<- lm(df$y~df$x)
summary(OriginalModel)
OriginalModel$coefficients

OModelPlot<- df %>% ggplot(aes(x=df$x,y=df$y)) + geom_point() + geom_abline(intercept=-10.0641842 ,slope = 0.4283521, color="red")


# x is insignificant
# can improve our model by getting rid of the intercept

### NEW MODEL ###
NewModel <- lm(df$y~df$x-1)
summary(NewModel)
NewModel$coefficients
#equation of the regression line is y= 0.4283521x

NewMP<- OModelPlot + geom_abline(intercept=0,slope = 0.4283521, color="blue")


####################
### QUESTION 3.B ###
####################

#residuals

x <- infants$gestation
y<- infants$bwt
df <- data.frame(x,y)
df<-na.omit(df)


NewModel.lm.resid<- residuals(NewModel)
qqnorm(NewModel.lm.resid)
qqline(NewModel.lm.resid)

resplot<- plot(df$x, NewModel.lm.resid) +abline(h=0, col="red")


####################
### QUESTION 3.C ###
#################### 

#predict birthweight; baby's gestation period = 300

y= 0.4283521*(300)


##################
### QUESTION 4 ###
##################

#instructor aided in data cleaning 
# compare the mean gestation birth weight (`bwt`) of mothers who smoke (`smoke=Now`) versus mothers who don't smoke (`smoke=Never`).
#do a statistical analysis of whether birthweights are significantly different if mothers smoke or not.

library(dplyr)
smoke <- infants$smoke
gestation <- infants$gestation
df <- data.frame(smoke, gestation)
df <- df %>% filter(smoke=="Never" |  smoke =="Now")




####################
### QUESTION 4.A ###
####################  
# boxplot to examine `gestation` as a function of `smoke 

boxplot(formula=df$gestation~df$smoke)


####################
### QUESTION 4.B ###
####################   
#Normal Q-Q plot.

SGestation <- df %>% filter(smoke=="Now")
NSGestation <- df %>% filter(smoke=="Never")
SGestation <- SGestation$gestation
NSGestation <- NSGestation$gestation
A<-qqnorm(SGestation, col= "red")
B<-qqnorm(NSGestation, col= "blue")

qqplot(SGestation,NSGestation)

####################
### QUESTION 4.c ###
####################  
# t-test 
#nonparametric Mann-Whitney test

#Ho: mean difference in gestation period for smokes and non smokers is the same (=0)
#Ha:  mean difference in gestation period for smokes and non smokers is the same (=!0)
# we reject the Ho for small pvlaue 

t.test(SGestation,NSGestation,alternative="two.sided",mu=0,var.equal=FALSE,conf.level = 0.95)

wilcox.test(SGestation,NSGestation,alternative="two.sided",mu=0,conf.level=0.95)

