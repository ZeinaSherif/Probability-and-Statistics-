install.packages("dplyr", dependencies = TRUE) 
install.packages("ggplot2", dependencies = TRUE) 
install_github("StatsWithR/statsr", dependencies = TRUE)
library(statsr) 
library(dplyr) 
library(ggplot2)
library(MASS)
library(fitdistrplus)
library(hydroGOF)
library(tidyr)
titanic <- read.csv("C:\\Users\\User\\Desktop\\train.csv",header=TRUE,sep=",")
#Q1
summary(titanic)

#Q2
quantitative <- titanic %>% select_if(is.numeric)
quantitative
qualitative <-  titanic %>% select_if(Negate(is.numeric))
qualitative

#Q3
ageNA <- sum(is.na(titanic[,"Age"]))
ageNA

#Q4
titanic=na.omit(titanic)
titanic

#Q5
hist(titanic$Age)

#Q7
mean(titanic$Age)
sd(titanic$Age)

#Q8
sampl <- titanic %>%  sample_n(size = 50)
samplAge <- sampl$Age
samplAge
x = fitdist(samplAge,"norm")
x

#Q9
sample_means50 <- titanic %>%
  rep_sample_n(size=50,reps=50,replace = TRUE) %>%
  summarise(x_bar=mean(Age)) 
  ggplot(data = sample_means50 , aes(x=x_bar)) +
  geom_histogram(binwidth = 2)
  
#Q10  
sample_means100 <- titanic %>%
    rep_sample_n(size=50,reps=100,replace = TRUE) %>%
    summarise(x_bar=mean(Age)) 
    ggplot(data = sample_means100 , aes(x=x_bar)) +
    geom_histogram(binwidth = 2)
    
#Q11
sample_means1000 <- titanic %>%
      rep_sample_n(size=50,reps=1000,replace = TRUE) %>%
      summarise(x_bar=mean(Age)) 
      ggplot(data = sample_means1000 , aes(x=x_bar)) +
      geom_histogram(binwidth = 2)
      
#Q13
    sample_means_s20 <- titanic %>%
    rep_sample_n(size=20,reps=1500,replace = TRUE) %>%
    summarise(x_bar=mean(Age)) 
    ggplot(data = sample_means_s20 , aes(x=x_bar)) +
    geom_histogram(binwidth = 2)
    
#Q14      
    sample_means_s100 <- titanic %>%
      rep_sample_n(size=100,reps=1500,replace = TRUE) %>%
      summarise(x_bar=mean(Age)) 
      ggplot(data = sample_means_s100 , aes(x=x_bar)) +
      geom_histogram(binwidth = 2)
      
#Q15    
    sample_means_s200 <- titanic %>%
      rep_sample_n(size=200,reps=1500,replace = TRUE) %>%
      summarise(x_bar=mean(Age)) 
      ggplot(data = sample_means_s200 , aes(x=x_bar)) +
      geom_histogram(binwidth = 2)
      
#Q17
      sample_U1500 <- titanic %>%
        rep_sample_n(size=2,reps=1500,replace = TRUE) %>%
        summarise(x_variance=sd(Age)^2) 
      ggplot(data = sample_U1500 , aes(x=x_variance)) +
        geom_histogram(binwidth = 2)
      
#Q18
      sample_U1500 <- titanic %>%
        rep_sample_n(size=50,reps=1500,replace = TRUE) %>%
        summarise(x_variance=sd(Age)^2) 
      ggplot(data = sample_U1500 , aes(x=x_variance)) +
        geom_histogram(binwidth = 2)
#Q19
      sample50 <- titanic %>%  sample_n(size = 50)
      #MME
      x=rnorm(50,mean=mean(sample50$Age))
      meanEst=sum(x)/50
      meanEst
      bias_MME1=abs(meanEst-mean(titanic$Age))
      bias_MME1
      #MLE
      sample = rnorm(50,mean=mean(sample50$Age),sd=sd(sample50$Age))
      mle= optim(par = c(mu = 0.2, sigma = 1.5), fn = NLL, data = sample,
                 control = list(parscale = c(mu = 0.2, sigma = 1.5)))
      NLL = function(pars, data) {
        mu = pars[1]
        sigma=pars[2]
        NLL= -sum(dnorm(x = data, mean = mu,sd = sigma, log = TRUE))
      }
      mle$par
      bias_MLE1=as.numeric(mle$par[1])-mean(titanic$Age)
      bias_MLE1
#Q20
      sample200 <- titanic %>%  sample_n(size = 200)
      #MME
      x=rnorm(200,mean=mean(sample200$Age))
      meanEst2 =sum(x)/200
      meanEst2
      bias_MME2= abs(meanEst2-mean(titanic$Age))
      bias_MME2
      #MLE
      sample = rnorm(200,mean = mean(sample200$Age),sd = sd(sample200$Age))
      mle = optim(par = c(mu = 0.2, sigma = 1.5), fn = NLL, data = sample,
                  control = list(parscale = c(mu = 0.2, sigma = 1.5)))
      NLL = function(pars, data) {
        mu = pars[1]
        sigma=pars[2]
        NLL= -sum(dnorm(x = data, mean = mu,sd = sigma, log = TRUE))
      }
      mle$par
      bias_MLE2= as.numeric(mle$par[1])-mean(titanic$Age)
      bias_MLE2
#Q21
      age_male_2103112 <- titanic[titanic$Sex == "male",]
      age_male_2103112
      age_female_210583 <-  titanic[titanic$Sex == "female",]
      age_female_210583
      
      samplemale <- age_male_2103112 %>%
        rep_sample_n(size=50,reps=15000,replace = TRUE) %>%
        summarise(x_bar=mean(Age))
      samplefemale <- age_female_210583 %>%
        rep_sample_n(size=50,reps=15000,replace = TRUE) %>%
        summarise(x_bar = mean(Age))
      samplediff_means15000 <- (samplemale$x_bar - samplefemale$x_bar) 
      ggplot(data = samplemale-samplefemale , aes(x=samplediff_means15000)) +
        geom_histogram(binwidth = 2)
      
#Q22
      Survived_male <- age_male_2103112
      Survived_male 
      Survived_female <- age_female_210583
      Survived_female
      
      samplsurvmale  <- Survived_male %>%
        rep_sample_n(size=50,reps=15000,replace = TRUE) %>%
        summarise(sum(Survived=="1"))
      samplsurvfemale  <- Survived_female %>%
        rep_sample_n(size=50,reps=15000,replace = TRUE) %>%
        summarise(sum(Survived=="1"))
      samplediff_Survived15000 <- (samplsurvmale$`sum(Survived == "1")` - samplsurvfemale$`sum(Survived == "1")`) 
      ggplot(data = samplsurvmale-samplsurvfemale , aes(x=samplediff_Survived15000)) +
        geom_histogram(binwidth = 2)      
      
#Q23
      sampl23 <- titanic %>%  sample_n(size = 10)
      samplAge23 <- sampl23$Age
       mean23<- mean(samplAge23)
       mean23
      y=fitdist(sampl23$Age,"norm")
      confint(y,"mean",level = 0.95)

#Q24
      sampl24 <- titanic %>%  sample_n(size = 50)
      samplAge24 <- sampl24$Age
      mean24<- mean(samplAge24)
      mean24
      y=fitdist(sampl24$Age,"norm")
      confint(y,"mean",level = 0.95)

#Q25
      sampl25 <- titanic %>%  sample_n(size = 200)
      bsamplAge25 <- sampl25$Age
      mean(bsampAge25)
      var(bsampAge25)
      samplAge25 <- bsamplAge25 *5
      mean25 <- mean(samplAge25)      
      mean25
      var25 <- var(samplAge25)
      var25
      
#Q26
      sampl26 <- titanic %>%  sample_n(size = 200)
      bsamplAge26 <- sampl26$Age
      mean(bsampAge26)
      var(bsampAge26)
      samplAge26 <- bsamplAge26 +5
      mean26 <- mean(samplAge26)
      mean26
      var26 <- var(samplAge26)
      var26
      
      