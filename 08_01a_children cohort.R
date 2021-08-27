##########################################################
# Name of file: 08_01a_children.R
# Data release (if applicable):
# Original author(s): Chris Robertson chrisobertson@nhs.net
# Original date: 06 August 2020
# Latest update author (if not using version control) - Chris Robertson chrisobertson@nhs.net
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: reads in the cohort and merges in the Q Covid risk groups
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(plyr)
library(tidyverse)
library(survival)
#Load data

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop
project_path <- paste0(Location,"EAVE/GPanalysis/progs/JP/covid hosp in children")
setwd(project_path)




EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/Cohort_Demog_Endpoints_Times2021-07-28.rds"))
EAVE_cohort <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))

table(EAVE_cohort$death_covid, is.na(EAVE_cohort$NRS.Date.Death), exclude=NULL)
table(EAVE_cohort$icu_death, is.na(EAVE_cohort$date_icu_death), exclude=NULL)
table(EAVE_cohort$hosp_covid, is.na(EAVE_cohort$date_hosp_covid), exclude=NULL)

#z <- filter(EAVE_cohort, (icu_death==1 & is.na(date_icu_death)) )
#correct errors
EAVE_cohort <- EAVE_cohort %>% 
  mutate(death_covid = if_else(death_covid==1 & is.na(NRS.Date.Death), 0,death_covid)) %>% 
  mutate(icu_death = if_else((icu_death==1) & is.na(date_icu_death), 0, icu_death))

a_begin <- as.Date("2020-03-01")
#remove all who have died before the beginning
EAVE_cohort <- filter(EAVE_cohort, is.na(NRS.Date.Death) | (!is.na(NRS.Date.Death) & NRS.Date.Death > a_begin))

EAVE_Weights <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Weights.rds"))
EAVE_cohort  <- EAVE_cohort %>% left_join(EAVE_Weights, by="EAVE_LINKNO")
EAVE_cohort$eave_weight[is.na(EAVE_cohort$eave_weight)] <- mean(EAVE_cohort$eave_weight, na.rm=T)

#adjust inconsistencies in the endpoints and times - all hosp have an admission date
z_max_date_death <- max(EAVE_cohort$NRS.Date.Death, na.rm=T)
z_max_date_icu <- max(EAVE_cohort$date_icu_death, na.rm=T)
#EAVE_cohort <- EAVE_cohort %>% mutate(NRS.Date.Death = case_when(death_covid==1 & is.na(NRS.Date.Death) ~ SpecimenDate + 21,
#                                                       TRUE ~ NRS.Date.Death),
#                            date_icu_death = case_when(icu_death==1 & is.na(date_icu_death) ~ SpecimenDate + 14,
#                                                       TRUE ~ date_icu_death ) ) %>% 
#  mutate(NRS.Date.Death = case_when(NRS.Date.Death > z_max_date_death  ~ z_max_date_death,
#                                    TRUE ~ NRS.Date.Death),
#         date_icu_death = case_when(date_icu_death > z_max_date_icu ~ z_max_date_icu,
#                                    TRUE ~ date_icu_death ) )
EAVE_cohort <- EAVE_cohort %>% mutate(death_covid = case_when(death_covid==1 & is.na(NRS.Date.Death) ~ 0,
                                                              TRUE ~ death_covid),
                                      icu_death = case_when(icu_death==1 & is.na(date_icu_death) ~ 0,
                                                            TRUE ~ icu_death ) )

#z <- readRDS(paste0(Location,"EAVE/GPanalysis/data/ECOSS_deduped_linked.rds"))

#rg only has data on individuals who have at least one condition
rg <- readRDS( paste0(Location,"EAVE/GPanalysis/data/temp_allQcovid.rds"))
rg <- filter(rg,!duplicated(EAVE_LINKNO))

#Get the children Only
#df is the main analysis data frame
df <- EAVE_cohort %>% filter(ageYear >= 5 & ageYear < 18)


#individuals with no values in rg have no risk conditions
z <- df %>% left_join(rg, by="EAVE_LINKNO")
z <- z %>% mutate_at(vars(Q_DIAG_AF:Q_DIAG_CKD_LEVEL), ~replace(., is.na(.), 0))
z <- z %>% mutate_at(vars(Q_DIAG_AF:Q_DIAG_CKD_LEVEL), ~as.numeric(.))

df <- z

#get the names of all the Q covid risk groups
z_vars <- names(df)[grepl("^Q_", names(df))]

#rename the risk groups
name.map <- read.csv("condition_name_map.csv")
table(names(df)[names(df)%in%z_vars] == as.character(name.map$names))#all true
names(df)[names(df)%in%z_vars] <- as.character(name.map$full_name)
z_vars <- as.character(name.map$full_name)
#remove the risk groups - either not relevent to children or have data quality issues
z_vars_use <- z_vars[z_vars %in% c("BMI", "coronary heart disease", "COPD", 
"dementia","parkinson's disease","ethnicity", "accomodation")==FALSE]
zz <- z_vars_use[z_vars_use != "learning disability"]
#change the levels in z_vars_use - 0 is no 1 as yes
#for chronic kidney disease combine 3-5 to 1
zz <- z_vars_use[z_vars_use != "learning disability"]
df$'chronic kidney disease' <- ifelse(df$'chronic kidney disease'%in%c(3,4,5),1,df$'chronic kidney disease')
table(df$'chronic kidney disease')
my.fun <- function(x){as.factor(ifelse(x==1,"Yes","No"))}
z <- df %>% mutate_at(vars(zz), funs(my.fun))
#no learning disability, 1 means learning disability apart from Down’s syndrome and 2 means Down’s syndrome
z$`learning disability` <- factor(z$`learning disability`,level=0:2,
                                  labels=c("No","Yes - not Downs syndrome", "Yes - Downs syndrome"))
table(z$`learning disability`)
df <- z



#find the number with a value in each risk group
#z <- df %>% dplyr::select_at(z_vars)
#z <- z %>% pivot_longer(cols=everything())

#z.df <- z %>% group_by(name) %>% 
#  dplyr::summarise(N=sum(value>0))

#find the risk groups with at least 500 children
#drop bmi and ethnicity as they are not reliable
#z_vars_use <- z.df %>% filter(N>=500) %>% pull(name)
#z_vars_use <- z_vars_use[z_vars_use != "Q_BMI"]
#z_vars_use <- z_vars_use[z_vars_use != "Q_ETHNICITY"]

#get the numbers and rate per 100,000 for all the response variables
#don't use icu_death and covid_death as too few observations
#z_resp_vars <- c("tested","result","hosp_covid","icu_death","death_covid")
z_resp_vars <- c("tested","result","hosp_covid")
df.org <- df


#link in hospital history at baseline
z <- readRDS("/conf/EAVE/GPanalysis/data/smr01_JP_2021-08-26.rds")
hh.df <- z
#z <- subset(hh.df, ADMISSION_DATE >= as.Date("2018-03-01")&ADMISSION_DATE <as.Date("2020-03-01"))
#z.agg <- aggregate(z$CIS_MARKER, list(z$EAVE_LINKNO), function(x)length(unique(x)))#slow
#names(z.agg) <- c("EAVE_LINKNO","no_hosp_2yr")
#save the hosp history out
#saveRDS(z.agg, "hosp_hist.rds")
z.agg <- readRDS("hosp_hist.rds")

df <- left_join(df, z.agg)
summary(df$no_hosp_2yr)#majority have 1
df$no_hosp_2yr <- ifelse(is.na(df$no_hosp_2yr), 0, df$no_hosp_2yr)
df$no_hosp_2yrgp <- as.factor(ifelse(df$no_hosp_2yr>=1, "1+", 0))

df.org <- df

#link in the discharge date for the covid hosp
df <- df.org
z<- subset(hh.df, ADMISSION_DATE >= as.Date("2020-03-01"))
z$ADMISSION_DATE <- as.Date(substr(z$ADMISSION_DATE,1,10))
z$DISCHARGE_DATE <- as.Date(substr(z$DISCHARGE_DATE,1,10))
z.agg <- z %>% group_by( EAVE_LINKNO,CIS_MARKER) %>% 
  dplyr::summarise(doa = min(ADMISSION_DATE), dodis= max(DISCHARGE_DATE))

df <- left_join(df,z.agg[c("EAVE_LINKNO","doa","dodis")], by = c("date_hosp_covid" = "doa", "EAVE_LINKNO"="EAVE_LINKNO"))
df <- subset(df, !duplicated(EAVE_LINKNO))
table(is.na(subset(df, !is.na(date_hosp_covid))$dodis))#
summary(subset(df, !is.na(date_hosp_covid) & is.na(dodis))$date_hosp_covid) 

z <- readRDS("/conf/EAVE/GPanalysis/data/covid_hospitalisations.rds")
names(z)[2] <- "dodis2"
df <- left_join(df,z[c("EAVE_LINKNO","admission_date","dodis2")], by = c("date_hosp_covid" = "admission_date", "EAVE_LINKNO"="EAVE_LINKNO"))
table(is.na(subset(df, !is.na(date_hosp_covid))$dodis2))#

z <- readRDS("/conf/EAVE/GPanalysis/data/any_hospitalisation_post_01022020.rds")
names(z)[2] <- "dodis3"
df <- left_join(df,z[c("EAVE_LINKNO","admission_date","dodis3")], by = c("date_hosp_covid" = "admission_date", "EAVE_LINKNO"="EAVE_LINKNO"))
table(is.na(subset(df, !is.na(date_hosp_covid))$dodis3))#

df$dodis4 <- ifelse(is.na(df$dodis), as.character(df$dodis2), as.character(df$dodis))
df$dodis4 <- ifelse(is.na(df$dodis4), as.character(df$dodis3), as.character(df$dodis4))
table(is.na(subset(df, !is.na(date_hosp_covid))$dodis4))#
df$dodis4 <- as.Date(df$dodis4)
summary(df$dodis4)


df.org <- df


#table 1

z <- df %>% dplyr::select_at(c(z_vars_use, z_resp_vars, "eave_weight"))
z <- z %>% pivot_longer(cols=all_of(z_vars_use))
z.df <- z %>% group_by(name, value) %>% 
  dplyr::summarise(N = round(sum(eave_weight)),
                   across(all_of(z_resp_vars), ~ sum(.))) 
#z1 <- z.df %>% ungroup() %>%  mutate(across(all_of(z_resp_vars), ~round(./N*100000,1), .names="rate_{col}"))
z1 <- z.df %>%  mutate(rate_tested = round(tested/N*100000,1),
                       rate_positive = round(result/N*100000,1),
                       rate_hosp_covid = round(hosp_covid/N*100000,1) ) %>% ungroup() %>% as.data.frame()

z1$agegp <- "5-17 years old"
z.r <- z1

z_vars_use_a <- subset(z1, value!="No" & hosp_covid>=5)$name


df <- subset(df.org, ageYear>=5&ageYear<=11)
z <- df %>% dplyr::select_at(c(z_vars_use, z_resp_vars, "eave_weight"))
z <- z %>% pivot_longer(cols=all_of(z_vars_use))
z.df <- z %>% group_by(name, value) %>% 
  dplyr::summarise(N = round(sum(eave_weight)),
                   across(all_of(z_resp_vars), ~ sum(.))) 
#z1 <- z.df %>% ungroup() %>%  mutate(across(all_of(z_resp_vars), ~round(./N*100000,1), .names="rate_{col}"))
z1 <- z.df %>%  mutate(rate_tested = round(tested/N*100000,1),
                       rate_positive = round(result/N*100000,1),
                       rate_hosp_covid = round(hosp_covid/N*100000,1) ) %>% ungroup() %>% as.data.frame()
z1$agegp <- "5-11 years old"
z.r <- rbind(z.r,z1)


df <- subset(df.org, ageYear>=12&ageYear<=17)
z1$agegp <- "5-17 years old"
df <- subset(df.org, ageYear>=5&ageYear<=11)
z <- df %>% dplyr::select_at(c(z_vars_use, z_resp_vars, "eave_weight"))
z <- z %>% pivot_longer(cols=all_of(z_vars_use))
z.df <- z %>% group_by(name, value) %>% 
  dplyr::summarise(N = round(sum(eave_weight)),
                   across(all_of(z_resp_vars), ~ sum(.))) 
#z1 <- z.df %>% ungroup() %>%  mutate(across(all_of(z_resp_vars), ~round(./N*100000,1), .names="rate_{col}"))
z1 <- z.df %>%  mutate(rate_tested = round(tested/N*100000,1),
                       rate_positive = round(result/N*100000,1),
                       rate_hosp_covid = round(hosp_covid/N*100000,1) ) %>% ungroup() %>% as.data.frame()

z1$agegp <- "12-17 years old"
z.r <- rbind(z.r,z1)


write.csv(z.r, "table1.csv")

table(df$ageYear)
nrow(df)#764195
table(df$result)#44570
table(df$result)/nrow(df)*100#5.83%
table(df$hosp_covid)#449



#demog table
z <- df
z$agegp <- ifelse(z$ageYear>=12, "12-17","5-11")
z$agegp <- factor(z$agegp, levels=c("5-11","12-17"))
z$Sex <- as.factor(z$Sex)
table(z$agegp,exclude=NULL)
table(z$Sex,exclude=NULL)


for(i in 1:length(z_vars_use_a)){
  nn=aggregate(z$eave_weight, list(z[,z_vars_use_a[i]]),sum)$x[2]
  z.t <- data.frame(var="",levels="total",N=round(nn,0), condition=z_vars_use_a[i],
                    y.p = 100)
  nn1 <- aggregate(z$eave_weight, list(z$agegp,z[,z_vars_use_a[i]]),sum)$x[3:4]
  z.t <- rbind(z.t,data.frame(var="age",levels=levels(z$agegp),N=round(nn1,0), condition=z_vars_use_a[i],
                    y.p = round(nn1/nn*100,1)))
  nn1 <- aggregate(z$eave_weight, list(z$Sex,z[,z_vars_use_a[i]]),sum)$x[3:4]
  z.t <- rbind(z.t,data.frame(var="Sex",levels=levels(z$Sex),N=round(nn1,0), condition=z_vars_use_a[i],
                              y.p = round(nn1/nn*100,1)))
  nn1 <- aggregate(z$eave_weight, list(z$simd2020_sc_quintile,z[,z_vars_use_a[i]]),sum)$x[7:12]
  z.t <- rbind(z.t,data.frame(var="SIMD",levels=levels(z$simd2020_sc_quintile),
                              N=round(nn1,0), condition=z_vars_use_a[i],
                              y.p = round(nn1/nn*100,1)))
  nn1 <- aggregate(z$eave_weight, list(z$no_hosp_2yrgp,z[,z_vars_use_a[i]]),sum)$x[3:4]
  z.t <- rbind(z.t,data.frame(var="no_hosp_2yr",levels=levels(z$no_hosp_2yrgp),
                              N=round(nn1,0), condition=z_vars_use_a[i],
                              y.p = round(nn1/nn*100,1)))
  if(i==1) z.tt <- z.t else
    z.tt <- rbind(z.tt,z.t)
}

i=7

nn=aggregate(z$eave_weight, list(z[,z_vars_use_a[i]]),sum)$x[3]
z.t <- data.frame(var="",levels="total",N=round(nn,0), condition=z_vars_use_a[i],
                  y.p = 100)
nn1 <- aggregate(z$eave_weight, list(z$agegp,z[,z_vars_use_a[i]]),sum)$x[5:6]
z.t <- rbind(z.t,data.frame(var="age",levels=levels(z$agegp),N=round(nn1,0), condition=paste(z_vars_use_a[i], "- D"),
                            y.p = round(nn1/nn*100,1)))
nn1 <- aggregate(z$eave_weight, list(z$Sex,z[,z_vars_use_a[i]]),sum)$x[5:6]
z.t <- rbind(z.t,data.frame(var="Sex",levels=levels(z$Sex),N=round(nn1,0), condition=paste(z_vars_use_a[i], "- D"),
                            y.p = round(nn1/nn*100,1)))
nn1 <- aggregate(z$eave_weight, list(z$simd2020_sc_quintile,z[,z_vars_use_a[i]]),sum)$x[13:18]
z.t <- rbind(z.t,data.frame(var="SIMD",levels=levels(z$simd2020_sc_quintile),
                            N=round(nn1,0), condition=paste(z_vars_use_a[i], "- D"),
                            y.p = round(nn1/nn*100,1)))
nn1 <- aggregate(z$eave_weight, list(z$no_hosp_2yrgp,z[,z_vars_use_a[i]]),sum)$x[5:6]
z.t <- rbind(z.t,data.frame(var="no_hosp_2yr",levels=levels(z$no_hosp_2yrgp),N=round(nn1,0), condition=paste(z_vars_use_a[i], "- D"),
                            y.p = round(nn1/nn*100,1)))
  z.tt <- rbind(z.tt,z.t)


#overall
#aggregate(z$eave_weight, list(z$agegp), sum)
nn <- sum(z$eave_weight)
z.t <- data.frame(var="",levels="total",N=round(nn,0), condition="population",
                  y.p = 100)
z.t <- rbind(z.t,data.frame(var="age",levels=levels(z$agegp),N=round(aggregate(z$eave_weight, list(z$agegp), sum)$x,0), 
                  condition="population", y.p=round(aggregate(z$eave_weight, list(z$agegp), sum)$x/nn*100,1)))
z.t <- rbind(z.t,data.frame(var="Sex",levels=levels(z$Sex),N=round(aggregate(z$eave_weight, list(z$Sex), sum)$x,0), 
                  condition="population", y.p=round(aggregate(z$eave_weight, list(z$Sex), sum)$x/nn*100,1)))
z.t <- rbind(z.t,data.frame(var="SIMD",levels=levels(z$simd2020_sc_quintile),N=round(aggregate(z$eave_weight, list(z$simd2020_sc_quintile), sum)$x,0), 
                  condition="population", y.p=round(aggregate(z$eave_weight, list(z$simd2020_sc_quintile), sum)$x/nn*100,1)))
z.t <- rbind(z.t,data.frame(var="no_hosp_2yr",levels=levels(z$no_hosp_2yrgp),N=round(aggregate(z$eave_weight, list(z$no_hosp_2yrgp), sum)$x,0), 
                            condition="population", y.p=round(aggregate(z$eave_weight, list(z$no_hosp_2yrgp), sum)$x/nn*100,1)))

z.tt <- rbind(z.tt,z.t)

z.ttt <- data.frame(var=z.tt$var, levels=z.tt$levels, N=paste(z.tt$N," (",z.tt$y.p,")",sep=""), condition=z.tt$condition)

write.csv(z.ttt,"demog.csv")

##continous output from age
for(i in 1:length(z_vars_use_a)){
z.r <- data.frame(var="age",median=aggregate(z$ageYear, list(z[,z_vars_use_a[i]]), median)$x[2], 
                  q1 = aggregate(z$ageYear, list(z[,z_vars_use_a[i]]), function(x)quantile(x,0.25))$x[2],
                  q3 = aggregate(z$ageYear, list(z[,z_vars_use_a[i]]), function(x)quantile(x,0.75))$x[2],
                  condition=z_vars_use_a[i])
if(i==1) z.rr <- z.r else
  z.rr <- rbind(z.rr,z.r)
}
i=7
z.r <- data.frame(var="age",median=aggregate(z$ageYear, list(z[,z_vars_use_a[i]]), median)$x[3], 
                  q1 = aggregate(z$ageYear, list(z[,z_vars_use_a[i]]), function(x)quantile(x,0.25))$x[3],
                  q3 = aggregate(z$ageYear, list(z[,z_vars_use_a[i]]), function(x)quantile(x,0.75))$x[3],
                  condition=paste(z_vars_use_a[i],"- D"))
z.rr <- rbind(z.rr,z.r)
z.r <- data.frame(var="age",median=median(z$ageYear), 
                  q1 = quantile(z$ageYear,0.25),
                  q3 = quantile(z$ageYear,0.75),
                  condition="population")
z.rr <- rbind(z.rr,z.r)
z.rr$q3 <- round(z.rr$q3,0)
z.rrr <- data.frame(var="age", medianIQR = paste(z.rr$median, "(", z.rr$q1, ",",z.rr$q3, ")",sep=""), condition=z.rr$condition)






