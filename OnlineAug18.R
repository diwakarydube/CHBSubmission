library(MASS)
library(car)
library(lme4)
library(nlme)
library(reshape)
library(ez)
library(MuMIn)
library(merTools)
library(arm)
library(MASS)
library(lmerTest)
library(jtools)
library(sjPlot)
library(multcomp)
library(ggpubr)
library(ggplot2)
library(ez); library(ggplot2); library(nlme); library(pastecs); library(reshape); library(WRS)

library(lme4)
library(lmerTest)
library(emmeans)
library(piecewiseSEM)
library(reghelper)
library(Rcpp)
#install.packages("extrafont")
library(extrafont)
library(effects)

#HYPOTHESIS

#h0-1: Granularity for detecting Surprise faces = Granularity for detecting Happy faces          (DATASET= df1v5v5)
#h0-2: Confidence for detecting Anger faces = Confidence for detecting Neutral faces             (DATASET= df1v5v5)
#h0-3: Time required for detecting Anger faces = Time required for detecting Neutral faces       (DATASET= df1v5v5)
#h0-4: Accuracy for detecting Surprise faces = Accuracy for detecting Happy faces                (DATASET= df1v5v5)

#Power Analysis
power.t.test(delta = 0.5,sig.level = 0.05,power = 0.8,type = "two.sample",alternative = "two")
power.anova.test(delta = 0.5,sig.level = 0.05,power = 0.8,type = "two.sample",alternative = "two")
??power.anova.test
power.anova.test(groups = 7, n = 140, between.var = 1, within.var = 3,sig.level = 0.05) 
#here n=140 because we 140 people have participated in the experiment
#Findings Power Analysis gives 100% 
??power.t.test

#Result: This project plans to recruit 53 adults in order to achieve a medium effect size. With an α 
#level of .05% and 80% power, the minimum sample size including a 10% dropout was estimated at N=53.
#But we were managed to get 140 participants and then the power was found to be 100%


#Load data format 1
df1v5 <- read.csv("ClearnFormat1Area_df1v5.csv", header = TRUE) #Only true value
df1v4 <- read.csv("ClearnFormat1Area_df1v4.csv", header = TRUE)  # both true and false value



#changing id to character and granularity to factor
df1v5$ID <- as.character(df1v5$ID)
df1v5$granularity_table <- as.numeric((df1v5$granularity_table))

#changing id to character and granularity to factor
df1v4$ID <- as.character(df1v4$ID)
df1v4$granularity_table <- as.numeric((df1v4$granularity_table))


#check missing values
table(df1v5$participated)
#It can be found that there are some participants who have participated in similar sort of experiment. so we need to
#drop those 4 participants

table(df1v5$disorder)

#It can be found that there are some participants who have disorder or autism. so we need to
#drop those these values as well

#add numeric value for accuracy (true/ false)
df1v4$Accuracy1 <- df1v4$Accuracy
df1v4$Accuracy1 <- as.integer(df1v4$Accuracy1)


#creating a new dataset with only TRUE value
table(df1v5$Accuracy)
df1v5 <- df1v5[!(df1v5$Accuracy=="FALSE"),]
table(df1v5$Accuracy)
#THE VERSION 3 DATSET HAS 7691 TRUE VALUES



#ggline(df, x = "default,", y = "age", color = "black", add = c("mean_ci"), main = "Default Vs Balance")
#Checking Parametric assumptions 
#shaprio will not run due to higher sample size > 5000
shapiro.test(log(df1v5$time_table))

#for df1v5 dataset
qqnorm(df1v5$time_table) # violated - transformation might work

qqnorm(log(df1v5$time_table), main = 'Reaction time (Log) - Online (web) Experiment') # fairly normal


qqnorm(df1v5$confidence_table) #Confidence is not a continuous variable 

# The graph suggest that the variance is not equally distributed even after sqrt transformation. One reason might be a lot of participants selected 
#100% confidence intervals while selecting certain expressions.Since anova is a robust test we can continue the analysis. 

#Although sphericity is an assumtiion 
#Using lme() has the benefit that we can forget about sphericity
#

#for v3 datset
qqnorm(df1v5$time_table) # violated - transformation might work

qqnorm(log(df1v5$time_table)) # fairly normal
#Qualitatively it can be seen that it looks fairly normal

df1v5$logtime_table <- log(df1v5$time_table) #added the log to the data frame

df1v4$logtime_table <- log(df1v4$time_table)



#levene for timetable
leveneTest(df1v5$logtime_table ~ df1v5$ID)
#violated as less than < 0.05
#Levene-Test gave significant result for all of them because there were a huge number of observations.
#Therefore, homogeneity was checked qualitatively using boxplot.
boxplot(df1v5$logtime_table ~ df1v5$trial_table, data = df1v5,
        main = "Reaction time (log) for each Valances",xlab = "Valance",
        ylab = "Reaction time (log)") # fairly normal

boxplot(df1v5$granularity_table ~ df1v5$trial_table, data = df1v5)
#levene for area (LAB)

#violated as less than < 0.05
#Levene-Test gave significant result for all of them because there were a huge number of observations.
#Therefore, homogeneity was checked qualitatively using boxplot.


#exploratory Analysis
boxplot(granularity_table ~ trial_table, data = df1v5) # fairly normal
boxplot(granularity_table ~ Accuracy, data = df1v4) # fairly normal
boxplot(granularity_table ~ gender, data = df1v5) # fairly normal
table(df1v5$gender)
table(df1v5$gender)

ggline(df1v5, x = "gender", y = "granularity_table", color = "Accuracy", add = c("mean_ci")) #females needed more granularity to identify
#From the graph, it can be seen that females required more granularity to detect emotions compared to males. 

ggline(df1v5, x = "gender", y = "logtime_table", color = "Accuracy", add = c("mean_ci")) 
#From the graph, it can be seen that females required slightly more time to detect emotions compared to males. 

ggline(df1v5, x = "gender", y = "confidence_table", color = "Accuracy", add = c("mean_ci")) #males were more confident

##Check with Eva
##ggline(df1v5v2, x = "gender", y = "logArea", color = "Accuracy", add = c("mean_ci")) #males were more confident

#3244/4515 ( whereas male has a 71.8% accuracy)
#4421/5565 Female has a 79.5% accuracy
#write df1v5 and df1v4 to csv
write.csv(df1v4,"CleanFormat1_df1v4log.csv")
write.csv(df1v5,"ClearnFormat1Area_df1v5log.csv")


#Mixed Model: lmer
#lmer format: lmermodel <- lmer(Accuracy~trial_table+granularity_table+(1|ID),data=df1v5)
BasemodelH01<-lmer(granularity_table~1+(1|ID),data=df1v5, REML = FALSE) #post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH01)

lmermodelH01<-lmer(granularity_table~trial_table+(1|ID),data=df1v5, REML = FALSE)
summary(lmermodelH01)
anova(BasemodelH01,lmermodelH01)

lmermodelH01a<-lmer(Accuracy1~gender*trial_table+(1|ID),data=df1v4, REML = FALSE)
summary(lmermodelH01a)
anova(BasemodelH01,lmermodelH01)

#The above shows the comparison of the baseline model and the model that includes trial_table as a predictor (lmermodelH01).  The AIC and BIC tell us about the 
#fit of the model. The fact that these values are smaller in the "lmermodelH01" model than the baseline tells us that
#the fit of the model has got better. Therefore,  granularity_table is a significant predictor. 

sjPlot::tab_model(lmermodelH01)
#We used LMER modelling with participant ID as the random variable, granularity_table (target variable) as the dependent variable 
#and trial_table as the independent variable, allowing us to 
#explore the impact of trial_table  on granularity_table for facial expression detection. The results of our model suggests 
#that granularity_level has a significant effect on trial_table and there is a main effect for happy 
#(b0 = 7.51, b1 = -2.74, t(7546.12) = -28.813, P < 0.05) and 
#surprise (b0 = 7.51, b2 = -1.04, t(7547.99) = -10.490, P < 0.05, R2m = 0.082, R2c = 0.32) on trial_table. 
library(ggeffects)
ddH01 <- plot_model(lmermodelH01, type = "eff",terms = c("trial_table"), xlab = "Valance" , ylab = "Confidence", title = "Granularity Level for each Valance")
ddH01 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ddH01 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
#post-hoc for lmermodelH01

posthoclmermodelH01<-glht(lmermodelH01, linfct = mcp(trial_table = "Tukey"))
summary(posthoclmermodelH01)
#Output shows the results of the post hoc tests. We can see that the granularity to detect surprise was significantly 
#more  compared to happy (p <.05) 

#Qualitatively checking for lmermodelH01
ggline(df1v5, x = "trial_table", y = "granularity_table", color = "trial_table", add = c("mean_ci"))

#qualititatively from the above graph we can see that Surprise requires more granurality compared to happy which is same as our post hoc 
#test. Therefore the H01 hypothesis is violated.

reEx<-REsim(lmermodelH01)             # mean, median and sd of the random effect estimates 

plotREsim(REsim(lmermodelH01))  # plot the interval estimates 
plotFEsim(FEsim(lmermodelH01))  # plot the interval estimates 

#link:  https://cran.r-project.org/web/packages/merTools/vignettes/merToolsIntro.html
#The following plot is of the estimated random effects for each trial and their granularity_table 
plot(lmermodelH01) 

p1 <- plotREsim(reEx)
p1

plot_model(lmermodelH01)

#With respect to intercept anger, happy takes the lowest granularity than surprise
summary(lmermodelH01)

library(reghelper)
simple_slopes(lmermodelH01)
graph_model(lmermodelH01, y=trial_table, x=granularity_table, lines=granularity_table)
library(jtools)
summ(lmermodelH01)

#graph in paper
plot_model(lmermodelH01, type = "eff",terms = c("trial_table")) +
  labs(x = "Valence" , y = "Granularity", title = "Granularity Level for each valence")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A'))
install.packages("effects")

plot_model(lmermodelH01, terms = c("trial_table"), labs(x = "Valence" , y = "Granularity", title = "Granularity Level for each valence")+
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   text = element_text(family = 'A')))
##############updates for paper start##########03122023########
windowsFonts(A = windowsFont("Times New Roman"))
plot_model(lmermodelH01, type = "eff",terms = c("trial_table")) +
  labs(x = "Emotion type" , y = "Granularity", title = "Granularity for each emotion type")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A', size = 13))
##############updates for paper end############03122023####
#HYPOTHEIS-2 (CONFIDENCE)

#Mixed Model: lmer
#lmer format: lmermodel <- lmer(Accuracy~trial_table+granularity_table+(1|ID),data=df1v5)
BasemodelH02<-lmer(confidence_table~1+(1|ID),data=df1v5, REML = FALSE) #post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH02)

lmermodelH02<-lmer(confidence_table~trial_table+(1|ID),data=df1v5, REML = FALSE)
summary(lmermodelH02)
anova(BasemodelH02,lmermodelH02)

#The above shows the comparison of the baseline model and the model that includes trial_table as a predictor (lmermodelH02).  The AIC and BIC tell us about the 
#fit of the model. The fact that these values are smaller in the "lmermodelH02" model than the baseline tells us that
#the fit of the model has got better. Therefore,  confidence_table is a significant predictor. 

sjPlot::tab_model(lmermodelH02)
#We used LMER modelling with participant ID as the random variable, granularity_table (target variable) as the dependent variable 
#and trial_table as the independent variable, allowing us to 
#explore the impact of trial_table  on granularity_table for facial expression detection. The results of our model suggests 
#that granularity_level has a significant effect on trial_table and there is a main effect for happy 
#(b0 = 7.51, b1 = -2.74, t(7546.12) = -28.813, P < 0.05) and 
#surprise (b0 = 7.51, b2 = -1.04, t(7547.99) = -10.490, P < 0.05, R2m = 0.082, R2c = 0.32) on trial_table. 

#post-hoc for lmermodelH01

posthoclmermodelH02<-glht(lmermodelH02, linfct = mcp(trial_table = "Tukey"))
summary(posthoclmermodelH02)
#Output shows the results of the post hoc tests. We can see that the confidence to detect surprise was significantly 
#more  compared to happy (p <.05) 

#Qualitatively checking for lmermodelH01
ggline(df1v5, x = "trial_table", y = "confidence_table", color = "trial_table", add = c("mean_ci"))
ggline(df1v5, x = "trial_table", y = "confidence_table", color = "trial_table", add = c("mean_ci"), 
       main = "Difference in Confidence against Valances", xlab = "Valance"
       , ylab = "Mean - Confidence Score")


#qualititatively from the above graph we can see that Surprise requires more confidence compared to happy which is same as our post hoc 
#test. Therefore the H01 hypothesis is violated.

reEx<-REsim(lmermodelH02)             # mean, median and sd of the random effect estimates 

plotREsim(REsim(lmermodelH02))  # plot the interval estimates 

#link:  https://cran.r-project.org/web/packages/merTools/vignettes/merToolsIntro.html
#The following plot is of the estimated random effects for each trial and their granularity_table 
plot(lmermodelH02) 
#dev.off()
p1 <- plotREsim(reEx)
p1

ddH02 <- plot_model(lmermodelH02, type = "eff",terms = c("trial_table"),axis.title = "" , xlab = "Valance" , ylab = "Confidence", title = "Confidence Score for each Valance")
ddH02 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(family = 'A'))
ddH02 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))


plot_model(lmermodelH02)
ddH02 <- plot_model(lmermodelH02, type = "eff",terms = c("trial_table"),axis.title = "" , xlab = "Valance" , ylab = "Confidence", title = "Confidence Score for each Valance")
ddH02 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))
#With respect to intercept anger, happy takes the lowest granularity than surprise
summary(lmermodelH01)

windowsFonts(A = windowsFont("Times New Roman"))
??windowsFonts
#graph in paper
plot_model(lmermodelH02, type = "eff",terms = c("trial_table")) +
  labs(x = "Valence" , y = "Confidence score", title = "Confidence score for each valence")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A'))
install.packages("effects")
#######03122023#######updates to the graph start
plot_model(lmermodelH02, type = "eff",terms = c("trial_table")) +
  labs(x = "Emotion type" , y = "Confidence score", title = "Confidence for each emotion predicted by the model")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A', size = 13))
install.packages("effects")
################03122023#########updates to graph end

#HYPOTHESIS-3: TIME

#Mixed Model: lmer
#lmer format: lmermodel <- lmer(Accuracy~trial_table+granularity_table+(1|ID),data=df1v5)
BasemodelH03<-lmer(logtime_table~1+(1|ID),data=df1v5, REML = FALSE) #post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH03)

lmermodelH03<-lmer(logtime_table~trial_table+(1|ID),data=df1v5, REML = FALSE)
summary(lmermodelH03)
anova(BasemodelH03,lmermodelH03)

BasemodelH03a<-lmer(time_table~1+(1|ID),data=df1v5, REML = FALSE) #post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH03a)

lmermodelH03a<-lmer(time_table~trial_table+(1|ID),data=df1v5, REML = FALSE)
summary(lmermodelH03a)
anova(BasemodelH03a,lmermodelH03a)

#The above shows the comparison of the baseline model and the model that includes trial_table as a predictor (lmermodelH01).  The AIC and BIC tell us about the 
#fit of the model. The fact that these values are smaller in the "lmermodelH01" model than the baseline tells us that
#the fit of the model has got better. Therefore,  granularity_table is a significant predictor. 

sjPlot::tab_model(lmermodelH03)
sjPlot::tab_model(lmermodelH03a)
#We used LMER modelling with participant ID as the random variable, granularity_table (target variable) as the dependent variable 
#and trial_table as the independent variable, allowing us to 
#explore the impact of trial_table  on granularity_table for facial expression detection. The results of our model suggests 
#that granularity_level has a significant effect on trial_table and there is a main effect for happy 
#(b0 = 7.51, b1 = -2.74, t(7546.12) = -28.813, P < 0.05) and 
#surprise (b0 = 7.51, b2 = -1.04, t(7547.99) = -10.490, P < 0.05, R2m = 0.082, R2c = 0.32) on trial_table. 

#post-hoc for lmermodelH01

posthoclmermodelH03<-glht(lmermodelH03, linfct = mcp(trial_table = "Tukey"))
summary(posthoclmermodelH03)
#Output shows the results of the post hoc tests. We can see that the granularity to detect surprise was significantly 
#more  compared to happy (p <.05) 

#Qualitatively checking for lmermodelH01
ggline(df1v5, x = "trial_table", y = "logtime_table", color = "trial_table", add = c("mean_ci"))

#qualititatively from the above graph we can see that Surprise requires more granurality compared to happy which is same as our post hoc 
#test. Therefore the H01 hypothesis is violated.
ddH03 <- plot_model(lmermodelH03, type = "eff",terms = c("trial_table"), xlab = "Valance" , ylab = "Confidence", title = "Reaction Time (log) Score for each Valance")
ddH03 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ddH03 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
reEx<-REsim(lmermodelH03)             # mean, median and sd of the random effect estimates 

plotREsim(REsim(lmermodelH03))  # plot the interval estimates 

#link:  https://cran.r-project.org/web/packages/merTools/vignettes/merToolsIntro.html
#The following plot is of the estimated random effects for each trial and their granularity_table 
plot(lmermodelH03) 

p1 <- plotREsim(reEx)
p1

plot_model(lmermodelH03)

#With respect to intercept anger, happy takes the lowest granularity than surprise
summary(lmermodelH01)

#graph in paper
plot_model(lmermodelH03, type = "eff",terms = c("trial_table")) +
  labs(x = "Valence" , y = "Reaction time (log)", title = "Reaction time (log) Score for each valence")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A'))

###################03122023##########updates to paper start
windowsFonts(A = windowsFont("Times New Roman"))

plot_model(lmermodelH03, type = "eff",terms = c("trial_table")) +
  labs(x = "Emotion type" , y = "Reaction time (log)", title = "Log(Reaction time (RT)) for each emotion type")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A', size = 13))

##################03122023##########updates to paper end

#hypothesis 4: ACCURACY

#Mixed Model: lmer
#lmer format: lmermodel <- lmer(Accuracy~trial_table+granularity_table+(1|ID),data=df1v5)
BasemodelH04<-lmer(Accuracy1~1+(1|ID),data=df1v4, REML = FALSE) #post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH04)

lmermodelH04<-lmer(Accuracy1~trial_table+(1|ID),data=df1v4, REML = FALSE)
summary(lmermodelH04)
anova(BasemodelH04,lmermodelH04)
summary.lmer(lmermodelH04)
#The above shows the comparison of the baseline model and the model that includes trial_table as a predictor (lmermodelH01).  The AIC and BIC tell us about the 
#fit of the model. The fact that these values are smaller in the "lmermodelH01" model than the baseline tells us that
#the fit of the model has got better. Therefore,  granularity_table is a significant predictor. 

sjPlot::tab_model(lmermodelH04)
#We used LMER modelling with participant ID as the random variable, granularity_table (target variable) as the dependent variable 
#and trial_table as the independent variable, allowing us to 
#explore the impact of trial_table  on granularity_table for facial expression detection. The results of our model suggests 
#that granularity_level has a significant effect on trial_table and there is a main effect for happy 
#(b0 = 7.51, b1 = -2.74, t(7546.12) = -28.813, P < 0.05) and 
#surprise (b0 = 7.51, b2 = -1.04, t(7547.99) = -10.490, P < 0.05, R2m = 0.082, R2c = 0.32) on trial_table. 
#plot_model(lmermodelH04, type = "pred",terms = c("trial_table") +
#             labs(x = "Valance" , y = "Accuracy", title = "Predicted Accuracy by Model for each Valance"))


#post-hoc for lmermodelH01
plot_model(lmermodelH04, type = "pred",terms = c("trial_table")) +
  labs(x = "Accuracy (Success)" , y = "Valence", title = "Predicted Accuracy for Valences")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

windowsFonts(A = windowsFont("Times New Roman"))
??windowsFonts

#plot_model(lmermodelH04, type = "pred",terms = c("trial_table")) +
  labs(x = "Accuracy (Success)" , y = "Valence", title = "Predicted Accuracy for Valences")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        +theme(text = element_text(family = 'A')))
#graph in paper
plot_model(lmermodelH04, type = "pred",terms = c("trial_table")) +
  labs(x = "Accuracy (Success)" , y = "Valence", title = "Predicted accuracy for valence")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          text = element_text(family = 'A'))

pdf("Fig13 Accuracy of prediction for all valences.pdf", family = "Times")
##################03122023##########updates to paper###
#graph in paper
plot_model(lmermodelH04, type = "pred",terms = c("trial_table")) +
  labs(x = "Emotion type" , y = "Accuracy", title = "Accuracy of prediction for all emotion types")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A', size = 13))

pdf("Fig13 Accuracy of prediction for all valences.pdf", family = "Times")

###################03122023##########updates to paper###



??plot_model
posthoclmermodelH04<-glht(lmermodelH04, linfct = mcp(trial_table = "Tukey"))
summary(posthoclmermodelH04)
#Output shows the results of the post hoc tests. We can see that the granularity to detect surprise was significantly 
#more  compared to happy (p <.05) 

#Qualitatively checking for lmermodelH01
ggline(df1v5, x = "trial_table", y = "Accuracy1", color = "trial_table", add = c("mean_ci"))

#qualititatively from the above graph we can see that Surprise requires more granurality compared to happy which is same as our post hoc 
#test. Therefore the H01 hypothesis is violated.
table(df1v5$trial_table)
reEx<-REsim(lmermodelH04)             # mean, median and sd of the random effect estimates 
res <- t.test(Accuracy1 ~ trial_table, data = df1v5)
ttestdf1v5a 
plotREsim(REsim(lmermodelH04))  # plot the interval estimates 

#link:  https://cran.r-project.org/web/packages/merTools/vignettes/merToolsIntro.html
#The following plot is of the estimated random effects for each trial and their granularity_table 
plot(lmermodelH04) 

plotREsim(reEx)
p1

plot_model(lmermodelH04)

#With respect to intercept anger, happy takes the lowest granularity than surprise
summary(lmermodelH04)

df1v2 <- read.csv("ClearnFormat1Area_df1v2.csv", header = TRUE)
df1v2a <- read.csv("ClearnFormat1Area_df1v2a.csv", header = TRUE)
table(df1v2a$Accuracy)
table(df1v2$Accuracy)

df1v2a$ID <- as.character(df1v2a$ID)
df1v2a$granularity_table <- as.numeric((df1v2a$granularity_table))

df1v2$ID <- as.character(df1v2$ID)
df1v2$granularity_table <- as.numeric((df1v2$granularity_table))

###testing
###############
fitLM <- lm(Accuracy1 ~ granularity_table, data = df1v5)
plot(fitLM)
plot_model(fitLM, type = "pred", terms = c("granularity"))
summary(fitLM)

##
lmermodelH04<-lmer(Accuracy1~granularity_table+(1|ID),data=df1v4, REML = FALSE)
summary(lmermodelH04)
anova(BasemodelH04,lmermodelH04)
summary.lmer(lmermodelH04)
plot_model(lmermodelH04, type = "pred",terms = c("granularity_table")) +
  labs(x = "Accuracy (Success)" , y = "Valance", title = "Predicted Accuracy for Valances")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggline(df1v5, x = "granularity_table", y = "Accuracy", color = "trial_table", add = c("mean_ci"))
dfgran <- table(df1v5$granularity_table)
table(df1v4$granularity_table, df1v4$Accuracy) + (df1v4$Accuracy=="TRUE"/(df1v4$Accuracy=="TRUE"+df1v4$Accuracy=="FALSE"))
dd = (df1v4$Accuracy = "TRUE"/(df1v4$Accuracy = "TRUE"+df1v4$Accuracy = "FALSE"))

df1v4$Accuracy1 <- as.numeric(df1v4$Accuracy1)

fitanova12 <- aov(df1v4$Accuracy1 ~ df1v4$granularity_table)
summary(fitanova12)
TukeyHSD(fitanova12)
