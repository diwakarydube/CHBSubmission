#Libraries required to execute the commands
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
library(ggplot)
library(ez); library(ggplot2); library(nlme); library(pastecs); library(reshape); library(WRS)
library(ggline)
library(lmer)
library(lmerTest)
library(emmeans)
library(piecewiseSEM)
library(reghelper)
library(Rcpp)
library(ggpubr)
#load the file
df <- read.csv("Logmerged_V.0.3.csv", header = TRUE)
#set granularity as factor
df$granularity <- as.factor((df$granularity))
df$model_id <- df$granularity
ggline(df, x = "granularity", y = "val_accuracy", color = "granularity", add = c("mean_ci")) #males were more confident
fitanova12 <- aov(df$val_accuracy ~ df$granularity)
summary(fitanova12)
TukeyHSD(fitanova12)
ggline(df, x = "granularity", y = "accuracy", color = "granularity", add = c("mean_ci")) #males were more confident

#model
fitLM <- lm(val_accuracy ~ granularity, data = df)
plot(fitLM)
plot_model(fitLM, type = "pred", terms = c("granularity"))
summary(fitLM)

sjPlot::tab_model(fitLM)

plot_model(fitLM, type = "pred",terms = c("granularity")) +
  labs(x = "Granularity Levels (Validation Data Set of Images)" , y = "Validation Accuracy (CNN Model)", title = "Predicted Validation Accuracy by Model against Granularity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

library(lme4)
install.packages("lme4", type = "source")
#####
#log merged
Basemodellogmerged<-lmer(val_accuracy~1+(1|model_id),data=df, REML = FALSE) #post hoc i.e.,baseline model where 1 is the intercept
summary(Basemodellogmerged)

lmermodellogmerged<-lmer(val_accuracy~granularity+(1|model_id),data=df, REML = FALSE)
summary(lmermodellogmerged)
anova(BasemodelH03Online,lmermodelH03online)

sjPlot::tab_model(lmermodellogmerged)
r.squaredGLLM(lmermodellogmerged)
rsquared(lmermodellogmerged)
#graph in paper
windowsFonts(A = windowsFont("Times New Roman"))
plot_model(lmermodellogmerged, type = "pred",terms = c("granularity")) +
  labs(x = "Granularity" , y = "Accuracy", title = "Accuracy against granularity from the CNN model")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A',size = 13))


#merged human vs machine
df1compare <- read.csv("Logmerged_V.0.3MandH.csv", header = TRUE)
df1compare$granularity <- as.factor((df1compare$granularity))
df1compare$ID <- as.character(df1compare$ID)

lmermodelcompare<-lmer(val_accuracy~granularity*Category+(1|ID),data=df1compare, REML = FALSE)
summary(lmermodelcompare)
library(sjPlot)
sjPlot::tab_model(lmermodelcompare)

#graph in paper
plot_model(lmermodelcompare, type = "pred",terms = c("granularity","Category")) +
  labs(x = "Granularity" , y = "Accuracy", title = "Accuracy against granularity (CNN - Machine vs Experiment - Human)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = 'A',size = 13))

posthoclmecompdd<-glht(lmermodelcompare, linfct = mcp(granularity = "Tukey"))
summary(posthoclmecompdd)

###
lmermodelcomparedd<-lmer(val_accuracy~Category+(1|ID),data=df1compare, REML = FALSE)
summary(lmermodelcomparedd)
sjPlot::tab_model(lmermodelcomparedd)

posthoclmecomp<-glht(lmermodelcomparedd, linfct = mcp(Category = "Tukey"))
summary(posthoclmecomp)


###
lmermodelcomparedd<-lmer(val_accuracy~granularity + Category+(1|ID),data=df1compare, REML = FALSE)
summary(lmermodelcomparedd)
sjPlot::tab_model(lmermodelcomparedd)


lmermodelcomparedd11<-lmer(val_accuracy~granularity * Category+(1|ID),data=df1compare, REML = FALSE)
summary(lmermodelcomparedd11)
print(lmermodelcomparedd11, correlation=TRUE)
sjPlot::tab_model(lmermodelcomparedd11)
ggline(df1compare, x = "Category", y = "val_accuracy", color = "granularity", add = c("mean_ci")) #males were more confident


lmermodelcomparedd12<-lmer(val_accuracy~Category*granularity+(1|ID),data=df1compare, REML = FALSE)
summary(lmermodelcomparedd12)
posthoclmecompdd12<-glht(lmermodelcomparedd12, linfct = mcp(Category = "Tukey"))
summary(posthoclmecompdd12)
