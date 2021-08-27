##########################################################
# Name of file: 04a_Modelling.R
# Data release (if applicable):
# Original author(s): Chris Robertson chrisobertson@nhs.net
# Original date: 30 August 2020
# Latest update author (if not using version control) - Chris Robertson chrisobertson@nhs.net
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: reads in the cohort and merges in the risk groups 
#                         multivriate modelling with EAVE and SMR risk groups
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(plyr)
library(tidyverse)
library(survival)
library(survminer)
#Load data

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

z.response.vars <- c("tested","result","hosp_covid")
z_vars_use_a <- chartr(" ", "_", z_vars_use_a)



#if (exists("z.out")) rm(z.out)
df <-df.org2
df <- subset(df.org, ageYear>=5&ageYear<=11)
df <- subset(df.org, ageYear>=12&ageYear<=17)
#df <- subset(df, ageYear>=12&ageYear<=17)
names(df) <- chartr(" ","_",names(df))


z.rv <- "hosp_covid"
z.rv.time <- "Time.To.Hosp"

#z_vars_use_a2 <- z_vars_use_a[z_vars_use_a!="severe_mental_illness"]
#z_vars_use_a2 <- z_vars_use_a[z_vars_use_a!="blood_cancer"]

##cox model
z.fmla <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(ageYear) + Sex + simd2020_sc_quintile + no_hosp_2yrgp +",
                                         paste(z_vars_use_a, collapse= "+")))


z.fit <- coxph(z.fmla , data=df, weights = eave_weight)
summary(z.fit)

#ptemp <- termplot(z.fit, se=TRUE, plot=FALSE)
#ageterm <- ptemp$ageYear  # this will be a data frame
#center <- with(ageterm, y[x==5])
#ytemp <- ageterm$y + outer(ageterm$se, c(0, -1.96, 1.96),'*')
#matplot(ageterm$x, exp(ytemp - center), log='y',type='l', lty=c(1,2,2), col=1,
        #xlab="Age at Positive Test", ylab="Hospitalisation Hazard Ratio")
#abline(h=1, lty=2)


fun.cox <- function(z.var,z) {
  #z is a cox model fitted
  z.coefs <- summary(z)$coefficients
  z.est <- z.coefs[,"coef"]
  z.se <- z.coefs[,"se(coef)"]
  z.p <- z.coefs[,"p"]
  z.out <- cbind.data.frame(levels=dimnames(z.coefs)[[1]],HR=exp(z.est),LCL=exp(z.est-1.96*z.se),
                            UCL=exp(z.est+1.96*z.se),p=z.p,outcome=z.var)
  z.out$hrci <- paste(round(z.out$HR,2), " (", round(z.out$LCL,2), "-",round(z.out$UCL,2),")", sep="")

 return(z.out)
}

z.r <- fun.cox(z.rv,z.fit)
z.r
z.rr <- z.r 


#z.rr <- rbind(z.rr,z.r)



z.rv <- "result"
z.rv.time <- "Time.To.Test"

##cox model
z.fmla <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(ageYear) + Sex + simd2020_sc_quintile + no_hosp_2yrgp +",
                           paste(z_vars_use_a, collapse= "+")))
z.fit <- coxph(z.fmla , data=df, weights = eave_weight)
summary(z.fit)

z.r <- fun.cox("Tested pos",z.fit)
z.r
z.rr <- rbind(z.rr,z.r)


#ptemp <- termplot(z.fit, se=TRUE, plot=FALSE)
#ageterm <- ptemp$ageYear  # this will be a data frame
#center <- with(ageterm, y[x==5])
#ytemp <- ageterm$y + outer(ageterm$se, c(0, -1.96, 1.96),'*')
#matplot(ageterm$x, exp(ytemp - center), log='y',type='l', lty=c(1,2,2), col=1,
        #xlab="Age at Positive Test", ylab="Tested positive Hazard Ratio")
#abline(h=1, lty=2)


z.rv <- "tested"
z.rv.time <- "Time.To.Test"

##cox model
z.fmla <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(ageYear) + Sex + simd2020_sc_quintile + no_hosp_2yrgp +",
                           paste(z_vars_use_a, collapse= "+")))
z.fit <- coxph(z.fmla , data=df, weights = eave_weight)
summary(z.fit)

z.r <- fun.cox(z.rv,z.fit)
z.r

z.rr <- rbind(z.rr,z.r)




z.rr$agegp <- "5-17years"
z.rrr <- z.rr

z.rr$agegp <- "5-11years"
z.rrr <- rbind(z.rrr,z.rr)

z.rr$agegp <- "12-17years"
z.rrr <- rbind(z.rrr,z.rr)

write.csv(z.rrr,"cox.csv")

#ptemp <- termplot(z.fit, se=TRUE, plot=FALSE)
#ageterm <- ptemp$ageYear  # this will be a data frame
#center <- with(ageterm, y[x==5])
#ytemp <- ageterm$y + outer(ageterm$se, c(0, -1.96, 1.96),'*')
#matplot(ageterm$x, exp(ytemp - center), log='y',type='l', lty=c(1,2,2), col=1,
#        xlab="Age at Positive Test", ylab="Tested Hazard Ratio")
#abline(h=1, lty=2)





