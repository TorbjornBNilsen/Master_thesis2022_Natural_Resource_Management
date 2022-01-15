#------------------------------------------------------------------
# Loading in the data----
#Set working directory: 
setwd("C:/Users/bygdn/Desktop/Master - Mappe/R/R - Ferdig script/Script V1")
# Loading the different packages 
library(readxl)
library(lme4)
library(effects)
library(AICcmodavg)
library(nnet)
library(car)
library(DHARMa)
library(visreg)
library(ggplot2)
library(dplyr)
library(tibble)
library(caret)
library(lmtest)
library(MKmisc)
library(ResourceSelection)
library(sjPlot)
library(pROC)
library(ROCR)
#Loading the data
alldat <- as.data.frame(read_xlsx("EIA.xlsx",sheet="Data"))
#This data frame includes projects with no usable EIA data, these are removed in the new subset of the data. 

#Subsetting the data to remove the projects with unusable EIA
alldat2 <- subset(alldat, EIA <= 1,
                  select=c(Project, Approved, Year, LCOE, Capacity, Output, Landscape:Tourism_Travel))
#Removing the 54 projects with EIAs that are not usable from the data.

#Descriptive statistics (Alldat2)
summary(alldat2)
table(alldat2$Approved)

#NA-Check(Alldat2)
res.na <- as.data.frame(cbind(Column="TOTAL",Nr.NA=nrow(alldat2)-nrow(na.omit(alldat2))))
res.na[,2] <- as.numeric(as.vector(res.na[,2]))
for(c in 1:ncol(alldat2)){
  res.na <- rbind(res.na,cbind(Column=colnames(alldat2)[c],Nr.NA=nrow(alldat2)-length(na.omit(alldat2[,c]))))
}
res.na #Many NAs in most variables, with the exception of Landscape.


#Assigning the variables into groups:
groupdata <- as.data.frame(read_xlsx("EIA.xlsx",sheet="Groups"))
groups <- unique(groupdata$Group)

#MEAN-IMPACT GROUPING AND NA REMOVAL
mean.impact <- alldat2[,1:6]
mean.impact$Capacity <- as.numeric(mean.impact$Capacity)

func <- "mean" # mean,median, min or max
func.form <- function(x){
  ifelse(length(na.omit(x))==0,NA,eval(call(func,x,na.rm=T)))
}
mean.impact$Nr.themes <- apply(is.na(alldat2), 1,sum)
for(g in 1:length(groups)){
  themes <- groupdata$Theme[which(groupdata$Group==groups[g])]
  if(length(themes)==1) mean.impact[,groups[g]] <- round(alldat2[,themes],1)
  if(length(themes)>1) mean.impact[,groups[g]] <- round(apply(alldat2[,themes],1,func.form),1)
}

#NA-Check(mean.impact)
res.na <- as.data.frame(cbind(Column="TOTAL",Nr.NA=nrow(mean.impact)-nrow(na.omit(mean.impact))))
res.na[,2] <- as.numeric(as.vector(res.na[,2]))
for(c in 1:ncol(mean.impact)){
  res.na <- rbind(res.na,cbind(Column=colnames(mean.impact)[c],Nr.NA=nrow(mean.impact)-length(na.omit(mean.impact[,c]))))
}
res.na #Many NAs in physical, pollution, protection, and society.
#Can be explained by the different methods of assessments

#Omitting the variables "Physical, Pollution, Protection & Society, as they contain too many NAs.

mean.impact2 <- na.omit(mean.impact[,c("Project","Approved","Year","Capacity","Output","LCOE","Nr.themes",
                                       "Landscape","Heritage","Recreation","Nature", "LandUse","Sector")])

#NA-Check(mean.impact2)
res.na <- as.data.frame(cbind(Column="TOTAL",Nr.NA=nrow(mean.impact2)-nrow(na.omit(mean.impact2))))
res.na[,2] <- as.numeric(as.vector(res.na[,2]))
for(c in 1:ncol(mean.impact2)){
  res.na <- rbind(res.na,cbind(Column=colnames(mean.impact2)[c],Nr.NA=nrow(mean.impact2)-length(na.omit(mean.impact2[,c]))))
}
res.na # NO NAs

summary(mean.impact2)
#------------------------------------------------------------------
#Checking for the variables for correlations----
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(mean.impact2[,c(2,3,4,5,6,7)], lower.panel = panel.smooth, upper.panel = panel.cor) #Only capacity and output are correlated, this is expected.
pairs(mean.impact2[,c(3,8,9,10,11,12,13)], lower.panel = panel.smooth, upper.panel = panel.cor)
pairs(mean.impact2[,c(2,3,4,6,8,9,10,11,12,13)], lower.panel = panel.smooth, upper.panel = panel.cor)

#None of the data variables are correlated (Except Capacity and output - Used as an indicator for the correlation check)
#------------------------------------------------------------------
#Creating the data frame used in the analysis----
moddat <- mean.impact2

moddat <- subset(moddat,
                  select=c(Project, Approved, Year, LCOE, Capacity, Landscape, Heritage, Recreation,Nature, LandUse, Sector))
summary(moddat)
table(moddat$Approved)


#Categorizing the data into "High impact", "Medium impact" and "Low impact".
#I will be using the -2.5 as the cut-off between "High" and "Low" impact for non-grouped variables while using the -1.5 as the cut-off between "High and "Low" impact for grouped variables.
#This adjustment is done in order to compensate for the "Mean" value in many cases being lower. 

#Categorizing the Landscape variable - Not grouped.
moddat$Landscape <- cut(moddat$Landscape, 
                        breaks=c(-Inf,-2,Inf),
                        labels=c("High Impact","Low Impact"))

#Categorizing the Heritage variable - Not grouped (except 4 projects)
moddat$Heritage <- cut(moddat$Heritage, 
                       breaks=c(-Inf,-2,Inf),
                       labels=c("High Impact","Low Impact"))

#Categorizing the Recreation variable - Not grouped
moddat$Recreation <- cut(moddat$Recreation, 
                         breaks=c(-Inf,-2,Inf),
                         labels=c("High Impact","Low Impact"))
#Categorizing the Nature variable
moddat$Nature <- cut(moddat$Nature,
                     breaks=c(-Inf,-1.5,Inf),
                     labels=c("High Impact","Low Impact"))

#Categorizing the LandUse variable
moddat$LandUse <- cut(moddat$LandUse, 
                      breaks=c(-Inf,-1.5,Inf),
                      labels=c("High Impact","Low Impact"))

#Categorizing the Sector variable. 
moddat$Sector <- cut(moddat$Sector, 
                     breaks=c(-Inf,-1.5,Inf),
                     labels=c("High Impact","Low Impact"))

#Setting reference categories
moddat$Landscape <- relevel(moddat$Landscape, ref = "Low Impact")
moddat$Heritage <- relevel(moddat$Heritage, ref = "Low Impact")
moddat$Recreation <- relevel(moddat$Recreation, ref = "Low Impact")
moddat$Nature <- relevel(moddat$Nature, ref = "Low Impact")
moddat$LandUse <- relevel(moddat$LandUse, ref = "Low Impact")
moddat$Sector <- relevel(moddat$Sector, ref = "Low Impact")

#Data transformation of the control variable
#Capacity
moddat$Capacity <- (sqrt(moddat$Capacity))
shapiro.test(moddat$Capacity)
#Capacity is normally distributed

#Year
#Operationalizing year.
moddat$Year <- (moddat$Year-2020)
hist(moddat$Year)
shapiro.test(moddat$LCOE)
#Year is not normally distributed

#LCOE:
moddat$LCOE <- (moddat$LCOE-38)
shapiro.test(moddat$LCOE)
#LCOE is not normally distributed'
#------------------------------------------------------------------
#Descriptive statistics (moddat):----
summary(moddat)

table(moddat$Approved)

sd(moddat$Capacity)
sd(moddat$Year)
sd(moddat$LCOE)

#Two-way table of independent variables:
xtabs(~Approved + Landscape, data = moddat)
xtabs(~Approved + Heritage, data = moddat)
xtabs(~Approved + Recreation, data = moddat)#Few high impacts for recreation
xtabs(~Approved + Nature, data = moddat)
xtabs(~Approved + LandUse, data = moddat)#Few high impacts for land-use
xtabs(~Approved + Sector, data = moddat) #Very few high impacts for Sector
#------------------------------------------------------------------
#Model inspection - Single EIA variable + Controls:----
model0 <-glm(Approved ~ 1, data = moddat, family = binomial)
summary(model0)

#Model inspection : Landscape
ap.landscape1 <- glm(Approved ~ Landscape, data = moddat, family = binomial)
summary(ap.landscape1) #Landscape is not significant (0.569)
anova(ap.landscape1, test = "Chi")#None of the terms are significant (0.5603)

#The simple model looks normal. High AIC values + large P-value indicates it explains the variation in the data poorly.

#Adding Capacity to the model: 
ap.landscape2 <- glm(Approved ~ Landscape + Capacity, data = moddat, family = binomial)
summary(ap.landscape2)#No improvement in the model
anova(ap.landscape2, test = "Chi") #None of the terms are significant

#Adding the capacity did not improve the model significantly. AIC values stayed roughly the same. No terms are significant

#Removing capacity and adding LCOE 
ap.landscape3 <- glm(Approved ~ Landscape + LCOE, data = moddat, family = binomial)
summary(ap.landscape3)#Landscape is not significant, same with LCOE.
anova(ap.landscape3, test = "Chi")#None of the terms are significant

#LCOE does not add to the model but detracts from the AIC value.

ap.landscape4 <- glm(Approved ~ Landscape + Year, data = moddat, family = binomial)
summary(ap.landscape4)#Landscape is not significant, same with Year.
anova(ap.landscape4, test = "Chi")#None of the terms are significant.

aictab(cand.set=list(model0,ap.landscape1,ap.landscape2,ap.landscape3,ap.landscape4),
       modnames=c("Null model","Only Landscape","Landscape + Capacity","Landscape + LCOE","Landscape + Year"))

#None of the models including landscape explain any variation in the data. 

Model1 <- glm(Approved ~ Landscape + Year +LCOE + Capacity, data = moddat, family = binomial)
summary(Model1)
anova(Model1, test = "Chi")#None of the terms are significant.
#Bad model according to AIC.This is expected as none of the variables in this model can accurately predict any change in the data.

#Model inspection : Heritage
ap.heritage1 <- glm(Approved ~ Heritage, data = moddat, family = binomial)
summary(ap.heritage1) #High Heritage impacts are statistically significant ( 0.0116 *)
anova(ap.heritage1, test = "Chi")#None of the terms are significant (0.01026 *)

#The simple model looks normal. High AIC values + large P-values shows this model explains the variation in the data poorly (But better than the null model).
plot(allEffects(ap.heritage1))#This indicates that high impacts to heritage could increases the uncertainty of the license outcome.

#Adding Capacity to the model: 
ap.heritage2 <- glm(Approved ~ Heritage + Capacity, data = moddat, family = binomial)
summary(ap.heritage2) #Heritage is still significant. Capacity does not add to the model.
anova(ap.heritage2, test = "Chi")#Heritage is significant, while capacity is not significant.

#Adding the capacity did not improve the model significantly. AIC values stayed roughly the same. No terms are significant

#Removing capacity and adding LCOE 
ap.heritage3 <- glm(Approved ~ Heritage + LCOE, data = moddat, family = binomial)
summary(ap.heritage3) #Heritage is still significant, LCOE is not significant at all.
anova(ap.heritage3, test = "Chi")
#LCOE does not add to the model but detracts from the AIC value. With large confidence intervals

ap.heritage4 <- glm(Approved ~ Heritage + Year, data = moddat, family = binomial)
summary(ap.heritage4) #Heritage is still significant, Year is not significant.
anova(ap.heritage4, test = "Chi")#Heritage is significant, while Year is not significant.

aictab(cand.set=list(model0,ap.heritage1,ap.heritage2,ap.heritage3,ap.heritage4),
       modnames=c("Null model","Only Heritage","Heritage + Capacity","Heritage + LCOE","Heritage + Year"))
#All the models are slightly better than the null model. 

Model2 <- glm(Approved ~ Heritage + Year + LCOE + Capacity, data = moddat, family = binomial)
summary(Model2) #Heritage is significant. None of the control variables add to the model.
anova(Model2, test = "Chi")#None of the terms are significant.
#Only the model including only heritage has a lower AIC value than the null model. Adding any of the control variables, does not improve my model according to this measurement.

#Model inspection : Recreation
ap.recreation1 <- glm(Approved ~ Recreation, data = moddat, family = binomial)
summary(ap.recreation1) #High impacts to Recreation is not statistically significant (0.35957)
anova(ap.recreation1, test = "Chi")#None of the terms are significant (0.09223 .)
#The simple model looks normal. High AIC values + large P-values shows this model explains the variation in the data poorly.

#Adding Capacity to the model: 
ap.recreation2 <- glm(Approved ~ Recreation + Capacity, data = moddat, family = binomial)
summary(ap.recreation2) #Neither Recreation or capacity is significant. Capacity does not add to the model.
anova(ap.recreation2, test = "Chi")#No terms are significant.
#Adding the capacity did not improve the model significantly. AIC values stayed roughly the same. No terms are significant

#Removing capacity and adding LCOE 
ap.recreation3 <- glm(Approved ~ Recreation + LCOE, data = moddat, family = binomial)
summary(ap.recreation3) #Recreation is still close to being significant, LCOE is not significant at all.
anova(ap.recreation3, test = "Chi")#None of the terms are significant.
#LCOE does not add to the model, but detracts from the AIC value.

ap.recreation4 <- glm(Approved ~ Recreation + Year, data = moddat, family = binomial)
summary(ap.recreation4) #Recreation is still close to being significant, Year is not significant.
anova(ap.recreation4, test = "Chi")#None of the terms are significant.

aictab(cand.set=list(model0,ap.recreation1,ap.recreation2,ap.recreation3,ap.recreation4),
       modnames=c("Null model","Only Recreation","Recreation + Capacity","Recreation + LCOE","Recreation + Year"))
#None of the models including recreation explains more than the null-model.

Model3 <- glm(Approved ~ Recreation + Year + LCOE + Capacity, data = moddat, family = binomial)
summary(Model3) #Heritage is still close to being significant, Year is not significant.
anova(Model3, test = "Chi")#None of the terms are significant.
#None of the models had a lower AIC value than the null model. Adding any of the control variables, does not improve the model according to this measurement.

#Model inspection : Nature
ap.nature1 <- glm(Approved ~ Nature, data = moddat, family = binomial)
summary(ap.nature1) #High Nature impacts are significant!! (0.0322 *)
anova(ap.nature1, test = "Chi")#Nature is significant (0.02044 *)

#The simple model looks normal. High AIC values explains the variation in the data poorly (But better than the null model).
#Both P-values are lower than 0.05. The model is significant.

plot(allEffects(ap.nature1))
#The effects of the Nature variable is statistically significant and can tell us that
#a project with high impacts to nature is less likely to get their project approved. 

#Adding Capacity to the model: 
ap.nature2 <- glm(Approved ~ Nature + Capacity, data = moddat, family = binomial)
summary(ap.nature2) #Nature is still significant.  capacity is not significant. Capacity does not add to the model.
anova(ap.nature2, test = "Chi")#Nature is still significant, while capacity is not significant.
#Adding the capacity did not improve the model significantly. AIC values stayed roughly the same(increased a bit).
#Only the Nature variable is significant

#Removing capacity and adding LCOE 
ap.nature3 <- glm(Approved ~ Nature + LCOE, data = moddat, family = binomial)
summary(ap.nature3) #Nature is still significant.  LCOE is not close to being significant. Capacity does not add to the model.
anova(ap.nature3, test = "Chi")#Nature is still significant, while LCOE is not significant
#LCOE does not add to the model, but detracts from the AIC value.

ap.nature4 <- glm(Approved ~ Nature + Year, data = moddat, family = binomial)
summary(ap.nature4) #Nature is still significant.  Year is not close to being significant. Year does not add to the model.
anova(ap.nature4, test = "Chi")#Nature is still significant, while Year is not significant

aictab(cand.set=list(model0,ap.nature1,ap.nature2,ap.nature3,ap.nature4),
       modnames=c("Null model","Only Nature","Nature + Capacity","Nature + LCOE","Nature + Year"))

#All the models have a lower AIC value than the null model. Adding any of the control variables, does not 
#improve my model according to this measurement.

Model4 <- glm(Approved ~ Nature + Year + LCOE + Capacity, data = moddat, family = binomial)
summary(Model4) #Nature is still significant.  Year is not close to being significant. Year does not add to the model.
anova(Model4, test = "Chi")#Nature is still significant, while LCOE is not significant

#Model inspection : LandUse
ap.landuse1 <- glm(Approved ~ LandUse, data = moddat, family = binomial)
summary(ap.landuse1) #High landuse impacts are close to being significant. (0.07526 .)
anova(ap.landuse1, test = "Chi")#Landuse is close to being significant ( 0.07287 )
#The simple model looks normal. High AIC values + P-values show that this model explains the variation in the data poorly (But better than the null model).

#Adding Capacity to the model: 
ap.landuse2 <- glm(Approved ~ LandUse + Capacity, data = moddat, family = binomial)
summary(ap.landuse2) #Landuse is no longer close to being statistically significant. Capacity is not significant.
anova(ap.landuse2, test = "Chi")#Landuse is close to being significant, while capacity is not significant.
#Capacity does not add to the model.

ap.landuse3 <- glm(Approved ~ LandUse + LCOE, data = moddat, family = binomial)
summary(ap.landuse3) #Landuse is close to being statistically significant. LCOE is not significant.
anova(ap.landuse3, test = "Chi")#Landuse is close to being significant, while LCOE is not significant.
#LCOE does not add to the model, but detracts from the AIC value.

ap.landuse4 <- glm(Approved ~ LandUse + Year, data = moddat, family = binomial)
summary(ap.landuse4) #Landuse is close to being statistically significant. Year is not significant.
anova(ap.landuse4, test = "Chi")#Landuse is close to being significant, while Year is not significant.
#Year does not add to the model.

aictab(cand.set=list(model0,ap.landuse1,ap.landuse2,ap.landuse3,ap.landuse4),
       modnames=c("Null model","Only LandUse","LandUse + Capacity","LandUse + LCOE","LandUse + Year"))
#Only the model containing only the landuse variable has a lower AIC value than the null model. Adding any of the control variables, does not improve my model according to this measurement.

Model5 <- glm(Approved ~ LandUse + Year + LCOE + Capacity, data = moddat, family = binomial)
summary(Model5)
anova(Model5, test = "Chi")

#Model inspection: Sector
ap.sector1 <- glm(Approved ~ Sector, data = moddat, family = binomial)
summary(ap.sector1) #Landscape is not significant (0.569)
anova(ap.sector1, test = "Chi")#None of the terms are significant (0.65620)
#The simple model looks normal. High AIC values + large CI intervals shows this model
#explains the variation in the data poorly.

#Adding Capacity to the model: 
ap.sector2 <- glm(Approved ~ Sector + Capacity, data = moddat, family = binomial)
summary(ap.sector2)# No improvement in the model
anova(ap.sector2, test = "Chi") #None of the terms are significant
#Adding the capacity did not improve the model significantly. AIC values stayed roughly the 
#same. No terms are significant

#Removing capacity and adding LCOE 
ap.sector3 <- glm(Approved ~ Sector + LCOE, data = moddat, family = binomial)
summary(ap.sector3)#Sector is not significant, same with LCOE.
anova(ap.sector3, test = "Chi")#None of the terms are significant
#LCOE does not add to the model.

ap.sector4 <- glm(Approved ~ Sector + Year, data = moddat, family = binomial)
summary(ap.sector4)# #Sector is not significant, same with Year.
anova(ap.sector4, test = "Chi")#None of the terms are significant.
#Year does not add to the model.

aictab(cand.set=list(model0,ap.sector1,ap.sector2,ap.sector3,ap.sector4),
       modnames=c("Null model","Only Sector","Sector + Capacity","Sector + LCOE","Sector + Year"))

#None of the models including Sector explain any variation in the data. 
Model6 <- glm(Approved ~ Sector + Year + LCOE + Capacity , data = moddat, family = binomial)
summary(Model6)# #Sector is not significant, same with Year.
anova(Model6, test = "Chi")#None of the terms are significant.
#------------------------------------------------------------------
#Printing models as tables----
tab_model(ap.landscape1,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)

#Landscape
tab_model(Model1,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)
#Heritage
tab_model(Model2,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)
#Recreation
tab_model(Model3,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)
#Nature
tab_model(Model4,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)
#Land-Use
tab_model(Model5,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)
#Sector
tab_model(Model6,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)
#------------------------------------------------------------------
#Plotting the results----

w1=plot(allEffects(Model2), response = T, ylab="Predicted chance of approval", main = "Influence of high reported impacts to Heritage",
        rug=FALSE, colors = c("blue", "black"),grid=TRUE,
        confint = list(style = "bars"),
        selection = 1, xlab = "Impact on Heritage", font.lab  = 2)

w1

w2=plot(allEffects(Model4), response = T, ylab="Predicted chance of approval", main = "Influence of high reported impacts to Nature",
        rug=FALSE, colors = c("blue", "black"),grid=TRUE,
        confint = list(style = "bars"),
        selection = 1, xlab = "Impact on Nature", font.lab  = 2)
w2

#------------------------------------------------------------------
#Receiver Operating Characteristic (ROC) Curve ----
#ROC - Landscape
Model1 <-  glm(Approved ~ Landscape + Year + LCOE + Capacity, data = moddat, family = "binomial")
summary(Model1)
p1 <- predict(Model1, moddat)
tab1 <- table(p1, moddat$Approved)
tab1
pred1 <- predict(Model1,moddat,type = "response")
pred1 <- prediction(pred1, moddat$Approved)
#Receiver Operating Characteristic (ROC) Curve
roc1 <- performance(pred1, "tpr","fpr")

plot(roc1,
     colorize=F,
     main = "ROC Curve - Model 1",
     ylab = "Sensitivity",
     xlab = "1- Specificity")
abline(a=0, b=1)

#Area under the curve (AUC)
auc1 <- performance(pred1, "auc")
auc1 <- unlist(slot(auc1, "y.values"))
auc1 <- round(auc1, 4)
auc1

legend (.8,.2, auc1, title = "AUC", cex = 1.2)

#ROC - Heritage
Model2 <-  glm(Approved ~ Heritage + Year + LCOE + Capacity, data = moddat, family = "binomial")
summary(Model2)
p2 <- predict(Model2, moddat)
tab2 <- table(p2, moddat$Approved)
tab2

pred2 <- predict(Model2,moddat,type = "response")
pred2 <- prediction(pred2, moddat$Approved)
#Receiver Operating Characteristic (ROC) Curve
roc2 <- performance(pred2, "tpr","fpr")

plot(roc2,
     colorize=F,
     main = "ROC Curve - Model 2",
     ylab = "Sensitivity",
     xlab = "1- Specificity")
abline(a=0, b=1)

#Area under the curve (AUC)
auc2 <- performance(pred2, "auc")
auc2 <- unlist(slot(auc2, "y.values"))
auc2 <- round(auc2, 4)

auc2

legend (.8,.2, auc2, title = "AUC", cex = 1.2)

#ROC - Recreation
Model3 <-  glm(Approved ~ Recreation + Year + LCOE + Capacity, data = moddat, family = "binomial")
summary(Model3)
p3 <- predict(Model3, moddat)
tab3 <- table(p3, moddat$Approved)
tab3

pred3 <- predict(Model3,moddat,type = "response")

pred3 <- prediction(pred3, moddat$Approved)
#Receiver Operating Characteristic (ROC) Curve
roc3 <- performance(pred3, "tpr","fpr")

plot(roc3,
     colorize=F,
     main = "ROC Curve - Model 3",
     ylab = "Sensitivity",
     xlab = "1- Specificity")
abline(a=0, b=1)

#Area under the curve (AUC)
auc3 <- performance(pred3, "auc")
auc3 <- unlist(slot(auc3, "y.values"))
auc3 <- round(auc3, 4)
auc3

legend (.8,.2, auc3, title = "AUC", cex = 1.2)

#ROC - Nature
Model4 <-  glm(Approved ~ Nature + Year + LCOE + Capacity, data = moddat, family = "binomial")
summary(Model4)
p4 <- predict(Model4, moddat)
tab4 <- table(p4, moddat$Approved)
tab4

pred4 <- predict(Model4,moddat,type = "response")
head(pred4)

head(moddat$Approved)

hist(pred4)

pred4 <- prediction(pred4, moddat$Approved)
#Receiver Operating Characteristic (ROC) Curve
roc4 <- performance(pred4, "tpr","fpr")

plot(roc4,
     colorize=F,
     main = "ROC Curve - Model 4",
     ylab = "Sensitivity",
     xlab = "1- Specificity")
abline(a=0, b=1)

#Area under the curve (AUC)
auc4 <- performance(pred4, "auc")
auc4 <- unlist(slot(auc4, "y.values"))
auc4 <- round(auc4, 4)
auc4

legend (.8,.2, auc4, title = "AUC", cex = 1.2)

#ROC - Landuse
Model5 <-  glm(Approved ~ LandUse + Year + LCOE + Capacity, data = moddat, family = "binomial")
summary(Model5)
p5 <- predict(Model5, moddat)
tab5 <- table(p5, moddat$Approved)
tab5

pred5 <- predict(Model5,moddat,type = "response")
pred5 <- prediction(pred5, moddat$Approved)
#Receiver Operating Characteristic (ROC) Curve
roc5 <- performance(pred5, "tpr","fpr")

plot(roc5,
     colorize=F,
     main = "ROC Curve - Model 5",
     ylab = "Sensitivity",
     xlab = "1- Specificity")
abline(a=0, b=1)

#Area under the curve (AUC)
auc5 <- performance(pred5, "auc")
auc5 <- unlist(slot(auc5, "y.values"))
auc5 <- round(auc5, 4)
auc5

legend (.8,.2, auc5, title = "AUC", cex = 1.2)

#ROC - Sector
Model6 <-  glm(Approved ~ Sector + Year + LCOE + Capacity, data = moddat, family = "binomial")
summary(Model6)
p6 <- predict(Model6, moddat)
tab6 <- table(p6, moddat$Approved)
tab6

pred6 <- predict(Model6,moddat,type = "response")
pred6 <- prediction(pred6, moddat$Approved)
#Receiver Operating Characteristic (ROC) Curve
roc6 <- performance(pred6, "tpr","fpr")

plot(roc6,
     colorize=F,
     main = "ROC Curve - Model 6",
     ylab = "Sensitivity",
     xlab = "1- Specificity")
abline(a=0, b=1)

#Area under the curve (AUC)
auc6 <- performance(pred6, "auc")
auc6 <- unlist(slot(auc6, "y.values"))
auc6 <- round(auc6, 4)
auc6

legend (.8,.2, auc6, title = "AUC", cex = 1.2)
#------------------------------------------------------------------
#Model diagnostics: Checking for the variables for correlations----
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(moddat[,c(2,3,4,5)], lower.panel = panel.smooth, upper.panel = panel.cor)
pairs(moddat[,c(6,7,8,9,10,11)], lower.panel = panel.smooth, upper.panel = panel.cor)

#Model diagnostics: Checking the models for collinearity ----
library(DHARMa)
#Landscape - Model 1 & 2
testDispersion(ap.landscape1)
simulationOutput1 <- simulateResiduals(fittedModel = ap.landscape1, plot = F)
plot(simulationOutput1)

testDispersion(Model1)
simulationOutput2 <- simulateResiduals(fittedModel = Model1, plot = F)
plot(simulationOutput2)

#Heritage - Model 3 & 4
testDispersion(ap.heritage1)
simulationOutput3 <- simulateResiduals(fittedModel = ap.heritage1, plot = F)
plot(simulationOutput3)

testDispersion(Model2)
simulationOutput4 <- simulateResiduals(fittedModel = Model2, plot = F)
plot(simulationOutput4)
#Problem with residuals vs predicted!

#Recreation - Model 5 & 6
testDispersion(ap.recreation1)
simulationOutput5 <- simulateResiduals(fittedModel = ap.recreation1, plot = F)
plot(simulationOutput5)

testDispersion(Model3)
simulationOutput6 <- simulateResiduals(fittedModel = Model3, plot = F)
plot(simulationOutput6)

#Nature - Model 7 & 8
testDispersion(ap.nature1)
simulationOutput7 <- simulateResiduals(fittedModel = ap.nature1, plot = F)
plot(simulationOutput7)

testDispersion(Model4)
simulationOutput8 <- simulateResiduals(fittedModel = Model4, plot = F)
plot(simulationOutput8)

#Landuse - Model 9 & 10
testDispersion(ap.landuse1)
simulationOutput9<- simulateResiduals(fittedModel = ap.landuse1, plot = F)
plot(simulationOutput9)

testDispersion(Model5)
simulationOutput10 <- simulateResiduals(fittedModel = Model5, plot = F)
plot(simulationOutput10)

#Sector - Model 11 & 12
testDispersion(ap.landuse1)
simulationOutput11 <- simulateResiduals(fittedModel = ap.landuse1, plot = F)
plot(simulationOutput11)

testDispersion(Model6)
simulationOutput12 <- simulateResiduals(fittedModel = Model6, plot = F)
plot(simulationOutput12)
#Problem with residuals vs predicted!

#------------------------------------------------------------------
#Using continuous variables:

ap.fit0 <- glm(Approved~1,data=mean.impact2,family=binomial)
ap.fit1 <- glm(Approved~Landscape,data=mean.impact2,family=binomial)
ap.fit2 <- glm(Approved~Heritage,data=mean.impact2,family=binomial)
ap.fit3 <- glm(Approved~Recreation,data=mean.impact2,family=binomial)
ap.fit4 <- glm(Approved~Nature,data=mean.impact2,family=binomial)
ap.fit5 <- glm(Approved~LandUse,data=mean.impact2,family=binomial)
ap.fit6 <- glm(Approved~Sector,data=mean.impact2,family=binomial)

aictab(cand.set=list(ap.fit0,ap.fit1,ap.fit2,ap.fit3,ap.fit4,ap.fit5,ap.fit6),
       modnames=c("Null model","Landscape model","Heritage model","Recreation model","Nature model","Land-use model","Sector model"))
#Recreation is important

summary(ap.fit1)#Impacts to Landscape is not significant
summary(ap.fit2)#Impacts to Heritage is close to being significant
summary(ap.fit3)#Impacts to Recreation is significant
summary(ap.fit4)#Impacts to Nature is significant
summary(ap.fit5)#Impacts to Land-use is not significant
summary(ap.fit6)#Impacts to Sector is not significant

#Plotting continuous models: 

#Landscape
tab_model(ap.fit1,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)
#Heritage
tab_model(ap.fit2,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)
#Recreation
tab_model(ap.fit3,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)
#Nature
tab_model(ap.fit4,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)
#Land-use
tab_model(ap.fit5,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)
#Sector
tab_model(ap.fit6,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)
#------------------------------------------------------------------


