#using df.org from 08_01a_children cohort.r

library(plyr)
library(tidyverse)
library(survival)
library(survminer)

z <- df.org
z$los <- as.numeric(z$dodis4-z$date_hosp_covid)
#table(z$los<=1)
#subset(z, !is.na(date_hosp_covid) & is.na(los))#check

#z <- subset(z, is.na(date_hosp_covid)|los>1)#check
#summary(z$los)

#table(is.na(subset(z, !is.na(date_hosp_covid))$los))# 

df<-z


z <- df %>% dplyr::select_at(c(z_vars_use, z_resp_vars, "eave_weight"))
z <- z %>% pivot_longer(cols=all_of(z_vars_use))
z.df <- z %>% group_by(name, value) %>% 
  dplyr::summarise(N = round(sum(eave_weight)),
                   across(all_of(z_resp_vars), ~ sum(.))) 
z1 <- z.df %>% ungroup() %>%  mutate(across(all_of(z_resp_vars), ~round(./N*100000,1), .names="rate_{col}"))
z1 <- z.df %>%  mutate(rate_tested = round(tested/N*100000,1),
                       rate_positive = round(result/N*100000,1),
                       rate_hosp_covid = round(hosp_covid/N*100000,1) ) %>% ungroup() %>% as.data.frame()

z1


z_vars_use_a <- chartr(" ", "_", z_vars_use_a)


names(df) <- chartr(" ","_",names(df))
z.rv <- "hosp_covid"
z.rv.time <- "Time.To.Hosp"





z.fmla <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(ageYear) + Sex + simd2020_sc_quintile + no_hosp_2yrgp +",
                           paste(z_vars_use_a, collapse= "+")))

z.fit <- coxph(z.fmla , data=df, weights = eave_weight)

z.r <- fun.cox(z.rv,z.fit)#fun.cox from 08_01c_children_Modelling.r
z.r