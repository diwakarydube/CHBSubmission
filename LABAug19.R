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

library(lmer)
library(lmerTest)
library(emmeans)
library(piecewiseSEM)
library(reghelper)


#HYPOTHESIS

#h0-1: Granularity for detecting Surprise faces = Granularity for detecting Happy faces          (DATASET= df1v2av5)
#h0-2: Confidence for detecting Anger faces = Confidence for detecting Neutral faces             (DATASET= df1v2av5)
#h0-3: Time required for detecting Anger faces = Time required for detecting Neutral faces       (DATASET= df1v2av5)
#h0-4: Accuracy for detecting Surprise faces = Accuracy for detecting Happy faces                (DATASET= df1v2av5)
#h0-5: Area for detecting Surprise faces = Area for detecting Happy faces 
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
df1v2a <- read.csv("ClearnFormat1Area_df1v2a.csv", header = TRUE) #Only true value
df1v2 <- read.csv("ClearnFormat1Area_df1v2.csv", header = TRUE)  # both true and false value



#changing id to character and granularity to factor
df1v2a$ID <- as.character(df1v2a$ID)
df1v2a$granularity_table <- as.numeric((df1v2a$granularity_table))

#changing id to character and granularity to factor
df1v2$ID <- as.character(df1v2$ID)
df1v2$granularity_table <- as.numeric((df1v2$granularity_table))


#check missing values
table(df1v2a$participated)
#It can be found that there are some participants who have participated in similar sort of experiment. so we need to
#drop those 4 participants

table(df1v2a$disorder)

#It can be found that there are some participants who have disorder or autism. so we need to
#drop those these values as well

#add numeric value for accuracy (true/ false)
df1v2$Accuracy1 <- df1v2$Accuracy
df1v2$Accuracy1 <- as.integer(df1v2$Accuracy1)

df1v2aFear  <- df1v2a[!(df1v2a$trial_table=="Sad"),]
df1v2aFear1  <- df1v2aFear[!(df1v2aFear$trial_table=="Disgust"),]
df1v2aFear2  <- df1v2aFear1[!(df1v2aFear1$trial_table=="Surprise"),]
df1v2aFear3  <- df1v2aFear2[!(df1v2aFear2$trial_table=="Happy"),]
df1v2aFear4 <- df1v2aFear3[!(df1v2aFear3$trial_table=="Anger"),]
df1v2aFear5  <- df1v2aFear4[!(df1v2aFear4$trial_table=="Neutral"),]
#df1v2aFear6  <- df1v2aFear5[!(df1v2aFear5$trial_table=="Neutral"),]
#creating a new dataset with only TRUE value
table(df1v2a$Accuracy)
df1v2a <- df1v2a[!(df1v2a$Accuracy=="FALSE"),]
table(df1v2a$Accuracy)
#THE VERSION 3 DATSET HAS 7691 TRUE VALUES

#write df1v2a and df1v2av2 to csv
#write.csv(df1v2a,"CleanFormat1_df1v2a.csv")
#write.csv(df1v2av2,"ClearnFormat1Area_df1v2av2.csv")
#write.csv(df1v2a,"ClearnFormat1Area_df1v2a.csv")


#Checking Parametric assumptions 

qqnorm(df1v2a$Area,main = "Pupil Diameter") #does not look normal
qqnorm(log(df1v2a$Area),main = "Pupil Diameter (Log)") #fairly normal after log transformation
##Qualitatively it can be seen that it looks fairly normal after log transformation

df1v2a$logArea <- log(df1v2a$Area)


#levene for timetable
leveneTest(df1v2a$logtime_table ~ df1v2a$ID)
#violated as less than < 0.05
#Levene-Test gave significant result for all of them because there were a huge number of observations.
#Therefore, homogeneity was checked qualitatively using boxplot.
boxplot(df1v2a$logtime_table ~ df1v2a$trial_table, data = df1v2a,
        main = "Reaction time (log) for each valences",xlab = "valence",
        ylab = "Reaction time (log)") # fairly normal
boxplot(df1v2a$logArea~df1v2a$trial_table,data=df1v2a,
        main = "Pupil Diameter (log) for each valences",xlab = "valence",
        ylab = "Pupil Diameter (log)")

boxplot(InstructorFeedback ~ instr, data = df, 
        main = "Instructor vs Insturctor Feedback",xlab = "Instructor",
        ylab = "Instructor Feedback") 

#levene for area (LAB)
leveneTest(df1v3$logArea ~ df1v3$ID)
#violated as less than < 0.05
#Levene-Test gave significant result for all of them because there were a huge number of observations.
#Therefore, homogeneity was checked qualitatively using boxplot.
boxplot(df1v2$logArea ~ df1v2$trial_table, data = df1v2) # fairly normal

boxplot(df1v1$logArea ~ df1v1$trial_table, data = df1v1) # fairly normal




#shaprio will not run due to higher sample size > 5000
shapiro.test(log(df1v2a$time_table))

#for df1v2a dataset
qqnorm(df1v2a$time_table) # violated - transformation might work

qqnorm(log(df1v2a$time_table), main = 'Reaction time (Log) - Lab Experiment') # fairly normal
qqnorm(df1v2a$cubeconfidence)
df1v2a$cubeconfidence <- df1v2a$confidence_table^(1/3) 

hist(df1v2a$confidence_table)
qqnorm(df1v2a$confidence_table) #Confidence is not a continuous variable 
df1v2a$sqrtconfidence_table <- sqrt(df1v2a$confidence_table)
qqnorm(df1v2a$sqrtconfidence_table)
# The graph suggest that the variance is not equally distributed even after sqrt transformation. One reason might be a lot of participants selected 
#100% confidence intervals while selecting certain expressions.Since anova is a robust test we can continue the analysis. 

#Although sphericity is an assumtiion 
#Using lme() has the benefit that we can forget about sphericity
#

#for v3 datset
qqnorm(df1v2a$time_table) # violated - transformation might work

qqnorm(log(df1v2a$time_table)) # fairly normal
#Qualitatively it can be seen that it looks fairly normal

df1v2a$logtime_table <- log(df1v2a$time_table) #added the log to the data frame

df1v2$logtime_table <- log(df1v2$time_table)



#levene for timetable
leveneTest(df1v2a$logtime_table ~ df1v2a$ID)
#violated as less than < 0.05
#Levene-Test gave significant result for all of them because there were a huge number of observations.
#Therefore, homogeneity was checked qualitatively using boxplot.
boxplot(df1v2a$logtime_table ~ df1v2a$trial_table, data = df1v2a) # fairly normal

boxplot(df1v2a$granularity_table ~ df1v2a$trial_table, data = df1v2a)
#levene for area (LAB)

#violated as less than < 0.05
#Levene-Test gave significant result for all of them because there were a huge number of observations.
#Therefore, homogeneity was checked qualitatively using boxplot.


#exploratory Analysis
boxplot(granularity_table ~ trial_table, data = df1v2a) # fairly normal
boxplot(granularity_table ~ Accuracy, data = df1v2) # fairly normal
boxplot(granularity_table ~ gender, data = df1v2a) # fairly normal
table(df1v2a$gender)
table(df1v2a$gender)

ggline(df1v2a, x = "gender", y = "granularity_table", color = "Accuracy", add = c("mean_ci")) #females needed more granularity to identify
#From the graph, it can be seen that females required more granularity to detect emotions compared to males. 

ggline(df1v2a, x = "gender", y = "logtime_table", color = "Accuracy", add = c("mean_ci")) 
#From the graph, it can be seen that females required slightly more time to detect emotions compared to males. 

ggline(df1v2a, x = "gender", y = "confidence_table", color = "Accuracy", add = c("mean_ci")) #males were more confident

##Check with Eva
##ggline(df1v2av2, x = "gender", y = "logArea", color = "Accuracy", add = c("mean_ci")) #males were more confident

#3244/4515 ( whereas male has a 71.8% accuracy)
#4421/5565 Female has a 79.5% accuracy


#Mixed Model: lmer
#lmer format: lmermodel <- lmer(Accuracy~trial_table+granularity_table+(1|ID),data=df1v2a)
BasemodelH01<-lmer(granularity_table~1+(1|ID),data=df1v2a, REML = FALSE) #post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH01)

lmermodelH01<-lmer(granularity_table~trial_table+(1|ID),data=df1v2a, REML = FALSE)
summary(lmermodelH01)
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
ddH01 <- plot_model(lmermodelH01, type = "eff",terms = c("trial_table"), xlab = "valence" , ylab = "Confidence", title = "Granularity Level for each valence")
ddH01 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ddH01 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
#post-hoc for lmermodelH01

posthoclmermodelH01<-glht(lmermodelH01, linfct = mcp(trial_table = "Tukey"))
summary(posthoclmermodelH01)
#Output shows the results of the post hoc tests. We can see that the granularity to detect surprise was significantly 
#more  compared to happy (p <.05) 

#REFERENCE: Output  shows the results of the post hoc tests. We can see that the GRANULARITY LEVEL TO DETECT retch was significantly 
#longer after eating a stick insect compared to a kangaroo testicle (p = .004) and a fish eye (p = .003) but not 
#compared to a witchetty grub. The time to retch after eating a kangaroo testicle was not significantly different 
#to after eating a fish eyeball or witchetty grub (both ps > .05). Finally, 
#the time to retch was not significantly different after eating a fish eyeball compared to a witchetty grub (p > .05).

#Qualitatively checking for lmermodelH01
ggline(df1v2a, x = "trial_table", y = "granularity_table", color = "trial_table", add = c("mean_ci"))

#qualititatively from the above graph we can see that Surprise requires more granurality compared to happy which is same as our post hoc 
#test. Therefore the H01 hypothesis is violated.

reEx<-REsim(lmermodelH01)             # mean, median and sd of the random effect estimates 

plotREsim(REsim(lmermodelH01))  # plot the interval estimates 

#link:  https://cran.r-project.org/web/packages/merTools/vignettes/merToolsIntro.html
#The following plot is of the estimated random effects for each trial and their granularity_table 
plot(lmermodelH01) 

p1 <- plotREsim(reEx)
p1

plot_model(lmermodelH01)

#With respect to intercept anger, happy takes the lowest granularity than surprise
summary(lmermodelH01)

plot_model(lmermodelH01, type = "eff",terms = c("trial_table")) +
  labs(x = "Valence" , y = "Granularity", title = "Granularity Level for each valence")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A'))


#HYPOTHEIS-2 (CONFIDENCE)

#Mixed Model: lmer
#lmer format: lmermodel <- lmer(Accuracy~trial_table+granularity_table+(1|ID),data=df1v2a)
BasemodelH02<-lmer(confidence_table~1+(1|ID),data=df1v2a, REML = FALSE) #post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH02)

lmermodelH02<-lmer(confidence_table~trial_table+(1|ID),data=df1v2a, REML = FALSE)
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

#graph in paper
plot_model(lmermodelH02, type = "pred",terms = c("trial_table")) +
   labs(x = "Valence" , y = "Confidence in percentage", title = "Predicted confidence by model for each valence")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    text = element_text(family = 'A'))
#Qualitatively checking for lmermodelH01

ddtestplot + labs(x = "valence" , y = "Accuracy", title = "Predicted Accuracy by Model for each valence")

ddtestplot <- plot_model(lmermodelH04, type = "eff",terms = c("trial_table"), xlab = "valence" , ylab = "Confidence", title = "Reaction Time (log) Score for each valence")
ddtestplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(x = "valence" , y = "Confidence in percentage", title = "Predicted Confidence by Model for each valence")
#Qualitatively checking for lmermodelH01
ggline(df1v2a, x = "trial_table", y = "confidence_table", color = "trial_table", add = c("mean_ci"))

#qualititatively from the above graph we can see that Surprise requires more confidence compared to happy which is same as our post hoc 
#test. Therefore the H01 hypothesis is violated.

reEx<-REsim(lmermodelH02)             # mean, median and sd of the random effect estimates 

plotREsim(REsim(lmermodelH02))  # plot the interval estimates 

#link:  https://cran.r-project.org/web/packages/merTools/vignettes/merToolsIntro.html
#The following plot is of the estimated random effects for each trial and their granularity_table 
plot(lmermodelH02) 

p1 <- plotREsim(reEx)
p1

plot_model(lmermodelH02)

#With respect to intercept anger, happy takes the lowest granularity than surprise
summary(lmermodelH01)

#HYPOTHESIS-3: TIME

#Mixed Model: lmer
#lmer format: lmermodel <- lmer(Accuracy~trial_table+granularity_table+(1|ID),data=df1v2a)
BasemodelH03<-lmer(logtime_table~1+(1|ID),data=df1v2a, REML = FALSE) #post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH03)

lmermodelH03<-lmer(logtime_table~trial_table+(1|ID),data=df1v2a, REML = FALSE)
summary(lmermodelH03)
anova(BasemodelH03,lmermodelH03)

BasemodelH03a<-lmer(time_table~1+(1|ID),data=df1v2a, REML = FALSE) #post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH03a)

lmermodelH03a<-lmer(time_table~trial_table+(1|ID),data=df1v2a, REML = FALSE)
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

posthoclmermodelH03a<-glht(lmermodelH03a, linfct = mcp(trial_table = "Tukey"))
summary(posthoclmermodelH03a)
#Output shows the results of the post hoc tests. We can see that the granularity to detect surprise was significantly 
#more  compared to happy (p <.05) 

#Qualitatively checking for lmermodelH01
ggline(df1v2a, x = "trial_table", y = "logtime_table", color = "trial_table", add = c("mean_ci"))

#qualititatively from the above graph we can see that Surprise requires more granurality compared to happy which is same as our post hoc 
#test. Therefore the H01 hypothesis is violated.

reEx<-REsim(lmermodelH03)             # mean, median and sd of the random effect estimates 

plotREsim(REsim(lmermodelH03))  # plot the interval estimates 

#link:  https://cran.r-project.org/web/packages/merTools/vignettes/merToolsIntro.html
#The following plot is of the estimated random effects for each trial and their granularity_table 
plot(lmermodelH03) 

p1 <- plotREsim(reEx)
p1

plot_model(lmermodelH03)
plot_model(lmermodelH03, type = "pred",terms = c("trial_table")) +
  labs(x = "Valence" , y = "Reaction time (log)", title = "Predicted reaction time (log) by model for each valence")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A'))
#With respect to intercept anger, happy takes the lowest granularity than surprise
summary(lmermodelH01)

#hypothesis 4: ACCURACY

#Mixed Model: lmer
#lmer format: lmermodel <- lmer(Accuracy~trial_table+granularity_table+(1|ID),data=df1v2a)
BasemodelH04<-lmer(Accuracy1~1+(1|ID),data=df1v2,REML = FALSE) #post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH04)

lmermodelH04<-lmer(Accuracy1~trial_table+(1|ID),data=df1v2,REML = FALSE)
summary(lmermodelH04)
anova(BasemodelH04,lmermodelH04)

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

#post-hoc for lmermodelH01

posthoclmermodelH04<-glht(lmermodelH04, linfct = mcp(trial_table = "Tukey"))
summary(posthoclmermodelH04)
#Output shows the results of the post hoc tests. We can see that the granularity to detect surprise was significantly 
#more  compared to happy (p <.05) 

#Qualitatively checking for lmermodelH01
ggline(df1v2a, x = "trial_table", y = "Accuracy1", color = "trial_table", add = c("mean_ci"))

#qualititatively from the above graph we can see that Surprise requires more granurality compared to happy which is same as our post hoc 
#test. Therefore the H01 hypothesis is violated.

reEx<-REsim(lmermodelH04)             # mean, median and sd of the random effect estimates 

plotREsim(REsim(lmermodelH04))  # plot the interval estimates 
ddtestplot <- plot_model(lmermodelH04, type = "pred",terms = c("trial_table"),axis.title = "" , xlab = "valence" , ylab = "Accuracy", title = "Predicted Accuracy by Model for each valence")
ddtestplot + labs(x = "valence" , y = "Accuracy", title = "Predicted Accuracy by Model for each valence")

ddtestplot <- plot_model(lmermodelH04, type = "eff",terms = c("trial_table"), xlab = "valence" , ylab = "Confidence", title = "Reaction Time (log) Score for each valence")
ddtestplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(x = "valence" , y = "Accuracy in percentage", title = "Predicted Accuracy by Model for each valence")
ddtestplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))

#link:  https://cran.r-project.org/web/packages/merTools/vignettes/merToolsIntro.html
#The following plot is of the estimated random effects for each trial and their granularity_table 
plot(lmermodelH04) 

p1 <- plotREsim(reEx)
p1

plot_model(lmermodelH04)

#With respect to intercept anger, happy takes the lowest granularity than surprise
summary(lmermodelH04)

windowsFonts(A = windowsFont("Times New Roman"))
#used in graph
plot_model(lmermodelH04, type = "pred",terms = c("trial_table")) +
  labs(x = "Valence" , y = "Accuracy in percentage", title = "Predicted accuracy by model for each valence")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A'))

#test
#ddtestplot <- plot_model(lmermodelH04, type = "eff",terms = c("trial_table"), xlab = "valence" , ylab = "Accuracy in percentage", title = "Predicted accuracy by model for each valence")
#ddtestplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(x = "Valence" , y = "Accuracy in percentage", title = "Predicted accuracy by model for each valence"),
#+theme(text = element_text(family = 'A')
#ddtestplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(family = 'A'))
#                   panel.background = element_blank(), axis.line = element_line(colour = "black"))

#HYPOTHESIS-5: AREA

#Mixed Model: lmer
#lmer format: lmermodel <- lmer(Accuracy~trial_table+granularity_table+(1|ID),data=df1v2a)
BasemodelH05<-lmer(logArea~1+(1|ID),data=df1v2a, REML = FALSE)#post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH05)

lmermodelH05<-lmer(logArea~trial_table+(1|ID),data=df1v2a, REML = FALSE)
summary(lmermodelH05)
anova(BasemodelH05,lmermodelH05)

BasemodelH05a<-lmer(Area~1+(1|ID),data=df1v2a, REML = FALSE)#post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH05a)

lmermodelH05aa<-lmer(Area~trial_table+(1|ID),data=df1v2a, REML = FALSE)
summary(lmermodelH05aa)
anova(BasemodelH05a,lmermodelH05aa)

#The above shows the comparison of the baseline model and the model that includes trial_table as a predictor (lmermodelH01).  The AIC and BIC tell us about the 
#fit of the model. The fact that these values are smaller in the "lmermodelH01" model than the baseline tells us that
#the fit of the model has got better. Therefore,  granularity_table is a significant predictor. 

sjPlot::tab_model(lmermodelH05aa)
#We used LMER modelling with participant ID as the random variable, granularity_table (target variable) as the dependent variable 
#and trial_table as the independent variable, allowing us to 
#explore the impact of trial_table  on granularity_table for facial expression detection. The results of our model suggests 
#that granularity_level has a significant effect on trial_table and there is a main effect for happy 
#(b0 = 7.51, b1 = -2.74, t(7546.12) = -28.813, P < 0.05) and 
#surprise (b0 = 7.51, b2 = -1.04, t(7547.99) = -10.490, P < 0.05, R2m = 0.082, R2c = 0.32) on trial_table. 

#post-hoc for lmermodelH01

posthoclmermodelH05<-glht(lmermodelH05, linfct = mcp(trial_table = "Tukey"))
summary(posthoclmermodelH05)
#Output shows the results of the post hoc tests. We can see that the granularity to detect surprise was significantly 
#more  compared to happy (p <.05) 

#Qualitatively checking for lmermodelH01
ggline(df1v2a, x = "trial_table", y = "logArea", color = "trial_table", add = c("mean_ci"))
ggline(df1v2, x = "trial_table", y = "Area", color = "trial_table", add = c("mean_ci"))

#qualititatively from the above graph we can see that Surprise requires more granurality compared to happy which is same as our post hoc 
#test. Therefore the H01 hypothesis is violated.

reEx<-REsim(lmermodelH05)             # mean, median and sd of the random effect estimates 

plotREsim(REsim(lmermodelH05))  # plot the interval estimates 

#link:  https://cran.r-project.org/web/packages/merTools/vignettes/merToolsIntro.html
#The following plot is of the estimated random effects for each trial and their granularity_table 
plot(lmermodelH05) 

p1 <- plotREsim(reEx)
p1

plot_model(lmermodelH05)

#With respect to intercept anger, happy takes the lowest granularity than surprise
summary(lmermodelH01)


#HYPOTHESIS-6: AREA vs RT

#Mixed Model: lmer
#lmer format: lmermodel <- lmer(Accuracy~trial_table+granularity_table+(1|ID),data=df1v2a)
BasemodelH06<-lmer(logArea~1+(1|ID),data=df1v2a, REML = FALSE)#post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH06)

BasemodelH06a<-lmer(Area~1+(1|ID),data=df1v2a, REML = FALSE)#post hoc i.e.,baseline model where 1 is the intercept
summary(BasemodelH06a)

lmermodelH06<-lmer(logArea~trial_table*logtime_table+(1|ID),data=df1v2a, REML = FALSE)
summary(lmermodelH06)
anova(BasemodelH06,lmermodelH06)

lmermodelH06aa<-lmer(Area~trial_table*time_table+(1|ID),data=df1v2a, REML = FALSE)
summary(lmermodelH06aa)
anova(BasemodelH06a,lmermodelH06aa)

#The above shows the comparison of the baseline model and the model that includes trial_table as a predictor (lmermodelH01).  The AIC and BIC tell us about the 
#fit of the model. The fact that these values are smaller in the "lmermodelH01" model than the baseline tells us that
#the fit of the model has got better. Therefore,  granularity_table is a significant predictor. 

sjPlot::tab_model(lmermodelH06)
sjPlot::tab_model(lmermodelH06aa)
#We used LMER modelling with participant ID as the random variable, granularity_table (target variable) as the dependent variable 
#and trial_table as the independent variable, allowing us to 
#explore the impact of trial_table  on granularity_table for facial expression detection. The results of our model suggests 
#that granularity_level has a significant effect on trial_table and there is a main effect for happy 
#(b0 = 7.51, b1 = -2.74, t(7546.12) = -28.813, P < 0.05) and 
#surprise (b0 = 7.51, b2 = -1.04, t(7547.99) = -10.490, P < 0.05, R2m = 0.082, R2c = 0.32) on trial_table. 

#post-hoc for lmermodelH01

posthoclmermodelH06<-glht(lmermodelH06, linfct = mcp(trial_table = "Tukey"))
summary(posthoclmermodelH06)
#Output shows the results of the post hoc tests. We can see that the granularity to detect surprise was significantly 
#more  compared to happy (p <.05) 

#Qualitatively checking for lmermodelH01
ggline(df1v2a, x = "logtime_table", y = "logArea", color = "trial_table", add = c("mean_ci"))
ggline(df1v2, x = "trial_table", y = "Area", color = "trial_table", add = c("mean_ci"))

#qualititatively from the above graph we can see that Surprise requires more granurality compared to happy which is same as our post hoc 
#test. Therefore the H01 hypothesis is violated.

reEx<-REsim(lmermodelH05)             # mean, median and sd of the random effect estimates 

plotREsim(REsim(lmermodelH05))  # plot the interval estimates 

#link:  https://cran.r-project.org/web/packages/merTools/vignettes/merToolsIntro.html
#The following plot is of the estimated random effects for each trial and their granularity_table 
plot(lmermodelH05) 

p1 <- plotREsim(reEx)
p1

plot_model(lmermodelH06, type = "pred")
install.packages("glmmTMB")
library(glmmTMB)
plot_model(lmermodelH06, type = "pred",terms = c("logarea","logtime_table")) +
  labs(x = "Reaction Time (log)" , y = "Pupil Area (log)", title = "Interaction between Reaction time and Pupil Area")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
interaction.plot(x.factor = df1v2a$trial_table, #x-axis variable
                 trace.factor = df1v2a$logtime_table, #variable for lines
                 response = df1v2a$logArea, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Weight Loss",
                 xlab = "Exercise Intensity",
                 col = c("pink", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Gender")
#With respect to intercept anger, happy takes the lowest granularity than surprise
summary(lmermodelH01)

fitLMint <- lm(logArea ~ trial_table*logtime_table, data = df1v2a)
plot(fitLMint)
plot_model(fitLMint, type = "int",terms = c("logarea","logtime_table"))


fitLMint_1 <- lm(confidence_table ~ logtime_table*logArea, data = df1v2a)
plot(fitLMint)
plot_model(fitLMint_1, type = "int",terms = c("logarea","logtime_table"))

fitLMint_2 <- lm(logArea ~ trial_table*logtime_table, data = df1v2a)
plot(fitLMint)
plot_model(fitLMint_1, type = "int",terms = c("logarea","logtime_table"))

plotREsim(FEsim(lmermodelH06))

qqnorm(lmermodelH06, ~ranef(., level=2))
qqnorm(resid(lmermodelH01))
Plot.Model.F.Linearity<-plot(resid(lmermodelH01),trial_table)

dd <- Plot.Model.F.Linearity(lmermodelH01)


lmermodelH07<-lmer(logArea~granularity_table*logtime_table+(1|ID),data=df1v2a, REML = FALSE)
summary(lmermodelH07)
#graph in paper figure 19
windowsFonts(A = windowsFont("Times New Roman"))
plot_model(lmermodelH07, type = "slope",terms = c("logarea","logtime_table")) +
  labs(x = "Reaction time (log)" , y = "Pupil diameter (log)", title = "Predicted reaction time (log) by model for all valences")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A'))
ggcoef(lmermodelH07)


+
  labs(x = "valence" , y = "Reaction time (log)", title = "Predicted Reaction Time (log) by Model for each valence")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
library(lme4)
sjp.lmer(lmermodelH07, type = "re", vars = NULL, ri.nr = NULL,
         group.estimates = NULL, remove.estimates = NULL, emph.grp = NULL,
         sample.n = NULL, poly.term = NULL, sort.est = NULL, title = NULL,
         legend.title = NULL, axis.labels = NULL, axis.title = NULL,
         geom.size = NULL, geom.colors = "Set1", show.values = TRUE,
         show.p = TRUE, show.ci = FALSE, show.legend = FALSE,
         show.loess = FALSE, show.loess.ci = FALSE, show.intercept = FALSE,
         string.interc = "(Intercept)", p.kr = TRUE, show.scatter = TRUE,
         point.alpha = 0.2, point.color = NULL, jitter.ci = FALSE,
         fade.ns = FALSE, axis.lim = NULL, digits = 2, vline.type = 2,
         vline.color = "grey70", facet.grid = TRUE, free.scale = FALSE,
         y.offset = 0.1, prnt.plot = TRUE, ...)


####
#fear only
df1v2aFear5$logtime_table <- log(df1v2aFear5$time_table) #added the log to the data frame

df1v2aFear5$logArea <- log(df1v2aFear5$Area)

lmermodelH07Fear<-lmer(logArea~granularity_table*logtime_table+(1|ID),data=df1v2aFear5, REML = FALSE)
summary(lmermodelH07Fear)

plot_model(lmermodelH07Fear, type = "slope",terms = c("logarea","logtime_table")) +
  labs(x = "Reaction Time (log)" , y = "Pupil Area (log)", title = "Interaction between Reaction time and Pupil Area for Fear valence")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggcoef(lmermodelH07Fear)
write.csv(df1v2aFear5,"CleanFormat1_df1v2aFear5.csv")
