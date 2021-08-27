##########################################################
# Name of file: 08_01a_children_Risk_Group_Descriptions.R
# Data release (if applicable):
# Original author(s): Chris Robertson chrisobertson@nhs.net
# Original date: 06 August 2020
# Latest update author (if not using version control) - Chris Robertson chrisobertson@nhs.net
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: Run 08_01a_children.R to get the cohort (df)
#                         runs through tabulations and graphs
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(survival)
library(survminer)
#Load data
#Location <- "/conf/"  # Server
Location <- "//isdsf00d03/"  # Desktop

#df is the analysis data frame
df <- df.org

#names(df) <- chartr(" ","_",names(df))
#df <- subset(df.org, ageYear>=5&ageYear<=11)
#df <- subset(df.org, ageYear>=12&ageYear<=17)

setwd("Z:\\JP\\vaccine safety\\covid risk children results")

pdf("agesex dis and percent events over age.pdf", width = 12, height = 8)

for(z.var in z_vars_use_a){

#z.var <- z_vars_use_a[1]
#get percentages of risk group by age and sex
z.df <- select(df, ageYear, Sex, starts_with(z.var)&ends_with(z.var), eave_weight) %>% rename(Var=3) %>% 
  group_by(ageYear, Sex) %>% summarise(N=round(sum(eave_weight)), 
                                       R=length(Var[Var!="No"])) %>% 
  mutate(Percent = round(R/N*100,1)) 




#plot percentages
g0 <- ggplot(z.df, aes(x=ageYear, y=Percent, col=Sex)) + geom_point() + geom_smooth(span=0.4) +
  labs(x="Age", title=z.var) 
#g0

#
#z.response.vars <- c("tested","result","hosp_covid","icu_death","death_covid")
z.response.vars <- c("tested","result","hosp_covid")
z.df <- select(df, ageYear, Sex, starts_with(z.var)&ends_with(z.var), eave_weight, tested, result, hosp_covid, icu_death, death_covid) %>%
  rename(Var=3)
z1.df <- z.df %>% group_by(ageYear, Var) %>% summarise(N=round(sum(eave_weight)))
z2.df <- z.df %>% group_by(ageYear, Var) %>% summarise(across(all_of(z.response.vars), ~round(sum(.))))
z.df <- left_join(z1.df,z2.df) %>% 
  mutate(across(all_of(z.response.vars), ~ ./N*100 , .names = "P_{col}")) %>% 
  mutate(Var=factor(Var))

g1.tested <- ggplot(z.df, aes(x=ageYear, y=P_tested, col=Var)) + geom_point() + geom_smooth(span=0.5) +
  labs(x="Age", y="Percentage", title=paste0("Tested by ",z.var)) 
#g1.tested

g1.pos <- ggplot(z.df, aes(x=ageYear, y=P_result, col=Var)) + geom_point() + geom_smooth(span=0.5) +
  labs(x="Age", y="Percentage", title=paste0("Tested Positive by ",z.var)) 
#g1.pos

g1.hosp <- ggplot(z.df, aes(x=ageYear, y=P_hosp_covid, col=Var)) + geom_point() + geom_smooth(span=0.5) +
  labs(x="Age", y="Percentage", title=paste0("Covid hospitalisation by ",z.var)) 
#g1.hosp

grid.arrange(g0, g1.tested, g1.pos, g1.hosp, nrow=2, ncol=2)


}

dev.off()




#g1.severe <- ggplot(z.df, aes(x=ageYear, y=P_icu_death, col=Var)) + geom_point() + geom_smooth(span=0.5) +
#  labs(x="Age", y="Proportion", title=paste0("ICU or Covid Death by ",z.var)) 
#g1.severe

#g1.dth <- ggplot(z.df, aes(x=ageYear, y=P_death_covid, col=Var)) + geom_point() + geom_smooth(span=0.5) +
#  labs(x="Age", y="Proportion", title=paste0("Covid Death by ",z.var)) 
#g1.dth

#grid.arrange(g1.tested, g1.pos, g1.hosp, nrow=2, ncol=2)


#z.xlab <- paste0("Days from ", format.Date(a_begin, "%d-%b-%Y"))
z.xlab <- "Date"
cutpt1 <- as.numeric(as.Date("2020-07-31")-as.Date("2020-03-01"))
cutpt2 <- as.numeric(as.Date("2021-04-30")-as.Date("2020-03-01"))
cutpt3 <- as.numeric(as.Date("2021-01-04")-as.Date("2020-03-01"))
cutpt4 <- as.numeric(as.Date("2021-05-17")-as.Date("2020-03-01"))
#you can use this to speed up the model fitting if needed
#select all the events and a random sample of 100 controls per event
#change df to z.df in the coxph fitting below
#z.case <- filter(df, hosp_covid==1)
#z.control <- slice_sample(filter(df, hosp_covid==0), n=nrow(z.case)*100)
#z.df <- rbind(z.case,z.control)

#survival curve + hazard ratio

df <- df.org2
df <- subset(df.org, ageYear>=5&ageYear<=11)
df <- subset(df.org, ageYear>=12&ageYear<=17)

splots <- list()
k=1
z.var="asthma_pres_2yrgp1"

for(z.var in z_vars_use_a){
z <- survfit(Surv(Time.To.Hosp, hosp_covid) ~ get(z.var),weights = eave_weight, data=df)
z.ymax <- 1-min(z$surv)
z.time <- Sys.time()
#z.fit <- coxph(Surv(Time.To.Hosp, hosp_covid) ~  pspline(ageYear) + Sex + simd2020_sc_quintile + ur6_2016_name + get(z.var),weights = eave_weight,data=df)
print(Sys.time() - z.time)
#z.tt <- summary(z.fit)$conf.int
#z.t <- round(z.tt[nrow(z.tt),c(1,3,4)],2)
#z.t2 <- round(z.tt[nrow(z.tt)-1,c(1,3,4)],2)
#z.t2 <- rbind(c(1,NA,NA), z.t)
#dimnames(z.t2) <- list(NULL, c("HR.Death","LCL.Death","UCL.Death"))
#z.tab <- cbind(z.tab,z.t2)
#ym[k] <- z.ymax

g1 <-ggsurvplot(z, data = df, fun="event", risk.table = FALSE, legend.labs=levels(as.factor(df[,z.var])), censor=FALSE) +
  labs(x=z.xlab, y="Proportion Hospitalised", title=paste0("Time to Covid Hospitalisation - ",z.var)) 
#g1$plot <- g1$plot +ggplot2::annotate("text",x=50,y=z.ymax*0.9, label = paste0("HR = ", z.t[1], "\n(",z.t[2],", ",z.t[3],")" ))
# g1$plot <- g1$plot +ggplot2::annotate("text",x=50,y=z.ymax*0.9, label = paste0("HR1 = ", z.t[1], "(",z.t[2],", ",z.t[3],")" ,"\nHR2 = ", z.t2[1], "(",z.t2[2],", ",z.t2[3],")"))
g1$plot <- g1$plot +ggplot2::annotate("text",x=50,y=z.ymax*0.5, label = "1st wave")
g1$plot <- g1$plot +ggplot2::annotate("text",x=cutpt1+120,y=z.ymax*0.5, label = "2nd wave")
g1$plot <- g1$plot +ggplot2::annotate("text",x=cutpt2+50,y=z.ymax*0.5, label = "3rd wave")
g1$plot <-g1$plot + geom_vline(xintercept = c(cutpt1,cutpt2),colour="grey", linetype = "longdash")
g1$plot <-g1$plot + geom_rect(aes(xmin=cutpt3, xmax=cutpt4, ymin=0, ymax=z.ymax),alpha=0.005,fill=5)
g1$plot <-g1$plot + geom_rect(aes(xmin=cutpt4, xmax=510, ymin=0, ymax=z.ymax),alpha=0.005,fill=7)
g1$plot <- g1$plot +ggplot2::annotate("text",x=cutpt3+50,y=z.ymax*0.9, label = "alpha\n dominant",col="dark green")
g1$plot <- g1$plot +ggplot2::annotate("text",x=cutpt4+30,y=z.ymax*0.9, label = "delta\n dominant", col="dark orange")
g1$plot <- g1$plot + scale_x_continuous(breaks = seq(0,450,by=150), label=as.character(as.Date("2020-03-01")+seq(0,450,by=150)))
g1
#pdf(paste("cumprop events over time 5to17years",z.var,".pdf",sep=""), width = 8, height = 6)
#pdf("cumprop events over time 5to17years.pdf", width = 8, height = 6)
#print(g1)
#dev.off()
#ggsave(paste("cumprop events over time 5to17year",z.var,".pdf",sep=""), plot = print(g1))
#g1$plot +geom_rect(mapping=aes(xmin=0, xmax=cutpt1, ymin=0, ymax=z.ymax), fill='red', alpha=0.1)
#splots[[k]] <- g1
#k=k+1
}
#dev.off()
#splots[[1]]
z.var=z_vars_use_a[1]

par(mfrow=c(2,2))
for(z.var in z_vars_use_a[1:3]){
  z <- survfit(Surv(Time.To.Hosp, hosp_covid) ~ get(z.var), data=df)
  z.ymax <- 1-min(z$surv)
  z.time <- Sys.time()
  z.fit <- coxph(Surv(Time.To.Hosp, hosp_covid) ~  pspline(ageYear) + Sex + simd2020_sc_quintile + ur6_2016_name + get(z.var),weights = eave_weight,data=df)
  print(Sys.time() - z.time)
  z.t <- summary(z.fit)$conf.int
  z.t <- round(z.t[nrow(z.t),c(1,3,4)],2)
  #z.t2 <- rbind(c(1,NA,NA), z.t)
  #dimnames(z.t2) <- list(NULL, c("HR.Death","LCL.Death","UCL.Death"))
  #z.tab <- cbind(z.tab,z.t2)
  
  plot(z, fun="event",col=1:2,main=paste0("Time to Covid Hospitalisation - ",z.var),xlab=z.xlab,ylab="Proportion Hospitalised")
  legend("topleft", levels(df[,z.var]),lty=1,col=1:2)
  text(x=50,y=z.ymax*0.2, paste0("HR = ", z.t[1], "\n(",z.t[2],", ",z.t[3],")" )) 
  text(x=50,y=z.ymax*0.5, "1st wave") 
  text(x=cutpt1+120,y=z.ymax*0.5, "2nd wave") 
  text(x=cutpt2+50,y=z.ymax*0.5, "3rd wave") 
  text(x=cutpt3+50,y=z.ymax*0.9, "alpha\n dominant",col="dark blue") 
  text(x=cutpt4+30,y=z.ymax*0.9, "delta\n dominant",col="brown") 
  abline(v=c(cutpt1,cutpt2),col="grey",lty=3,lwd=2)
  rect(cutpt3,0,cutpt4,z.ymax,border = NA,col="light blue",density=50)
  rect(cutpt4,0,510,z.ymax,border = NA,col="orange",density=50)
}





for(z.var in z_vars_use_a){
  z <- survfit(Surv(Time.To.Test, result) ~ get(z.var), data=df)
  z.ymax <- 1-min(z$surv)
  z.time <- Sys.time()
  z.fit <- coxph(Surv(Time.To.Test, result) ~  pspline(ageYear) + Sex + simd2020_sc_quintile + ur6_2016_name + get(z.var),weights = eave_weight, data=df)
  print(Sys.time() - z.time)
  z.t <- summary(z.fit)$conf.int
  z.t <- round(z.t[nrow(z.t),c(1,3,4)],2)
  #z.t2 <- rbind(c(1,NA,NA), z.t)
  #dimnames(z.t2) <- list(NULL, c("HR.Death","LCL.Death","UCL.Death"))
  #z.tab <- cbind(z.tab,z.t2)
  
  g1 <-ggsurvplot(z, data = df, fun="event", risk.table = FALSE, legend.labs=levels(as.factor(df[,z.var])), censor=FALSE) +
    labs(x=z.xlab, y="Proportion tested positive", title=paste0("Time to tested positive - ",z.var)) 
  g1$plot <- g1$plot +ggplot2::annotate("text",x=50,y=z.ymax*0.9, label = paste0("HR = ", z.t[1], "\n(",z.t[2],", ",z.t[3],")" ))
  g1$plot <- g1$plot +ggplot2::annotate("text",x=50,y=z.ymax*0.5, label = "1st wave")
  g1$plot <- g1$plot +ggplot2::annotate("text",x=cutpt1+120,y=z.ymax*0.5, label = "2nd wave")
  g1$plot <- g1$plot +ggplot2::annotate("text",x=cutpt2+50,y=z.ymax*0.5, label = "3rd wave")
  g1$plot <-g1$plot + geom_vline(xintercept = c(cutpt1,cutpt2),colour="grey", linetype = "longdash")
  g1$plot <-g1$plot + geom_rect(aes(xmin=cutpt3, xmax=cutpt4, ymin=0, ymax=z.ymax),alpha=0.005,fill=5)
  g1$plot <-g1$plot + geom_rect(aes(xmin=cutpt4, xmax=510, ymin=0, ymax=z.ymax),alpha=0.005,fill=7)
  g1$plot <- g1$plot +ggplot2::annotate("text",x=cutpt3+50,y=z.ymax*0.9, label = "alpha\n dominant",col="dark green")
  g1$plot <- g1$plot +ggplot2::annotate("text",x=cutpt4+30,y=z.ymax*0.9, label = "delta\n dominant", col="dark orange")
  #ggsave(paste("plot",z.var,".jpg",sep=""), plot = print(gg), width = 3.3, height = 2.2, dpi = 1000)
  #splots[[k]] <- g1
 # k=k+1
}

for(z.var in z_vars_use_a){
  z <- survfit(Surv(Time.To.Test, tested) ~ get(z.var), data=df)
  z.ymax <- 1-min(z$surv)
  z.time <- Sys.time()
  z.fit <- coxph(Surv(Time.To.Test, tested) ~  pspline(ageYear) + Sex + simd2020_sc_quintile + ur6_2016_name + get(z.var),weights = eave_weight, data=df)
  print(Sys.time() - z.time)
  z.t <- summary(z.fit)$conf.int
  z.t <- round(z.t[nrow(z.t),c(1,3,4)],2)
  #z.t2 <- rbind(c(1,NA,NA), z.t)
  #dimnames(z.t2) <- list(NULL, c("HR.Death","LCL.Death","UCL.Death"))
  #z.tab <- cbind(z.tab,z.t2)
  
  g1 <-ggsurvplot(z, data = df, fun="event", risk.table = FALSE, legend.labs=levels(as.factor(df[,z.var])), censor=FALSE) +
    labs(x=z.xlab, y="Proportion tested", title=paste0("Time to tested - ",z.var)) 
  g1$plot <- g1$plot +ggplot2::annotate("text",x=50,y=z.ymax*0.9, label = paste0("HR = ", z.t[1], "\n(",z.t[2],", ",z.t[3],")" ))
  g1$plot <- g1$plot +ggplot2::annotate("text",x=50,y=z.ymax*0.5, label = "1st wave")
  g1$plot <- g1$plot +ggplot2::annotate("text",x=cutpt1+120,y=z.ymax*0.5, label = "2nd wave")
  g1$plot <- g1$plot +ggplot2::annotate("text",x=cutpt2+50,y=z.ymax*0.5, label = "3rd wave")
  g1$plot <-g1$plot + geom_vline(xintercept = c(cutpt1,cutpt2),colour="grey", linetype = "longdash")
  g1$plot <-g1$plot + geom_rect(aes(xmin=cutpt3, xmax=cutpt4, ymin=0, ymax=z.ymax),alpha=0.005,fill=5)
  g1$plot <-g1$plot + geom_rect(aes(xmin=cutpt4, xmax=510, ymin=0, ymax=z.ymax),alpha=0.005,fill=7)
  g1$plot <- g1$plot +ggplot2::annotate("text",x=cutpt3+50,y=z.ymax*0.9, label = "alpha\n dominant",col="dark green")
  g1$plot <- g1$plot +ggplot2::annotate("text",x=cutpt4+30,y=z.ymax*0.9, label = "delta\n dominant", col="dark orange")
  
  splots[[k]] <- g1
  k=k+1
}



pdf("cumprop events over time 5to17years.pdf", width = 16, height = 10)
arrange_ggsurvplots(
  splots,
  ncol = 2,
  nrow = 2
)
dev.off()

pdf("cumprop events over time 5to11years.pdf", width = 16, height = 10)
arrange_ggsurvplots(
  splots,
  ncol = 2,
  nrow = 2
)
dev.off()

pdf("cumprop events over time 12to17years.pdf", width = 16, height = 10)
arrange_ggsurvplots(
  splots,
  ncol = 2,
  nrow = 2
)
dev.off()