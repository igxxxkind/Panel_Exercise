
setwd("D:\\projects\\Panel_exercise")

library("plm")
library(car) 
library("apsrtable")
library(tseries)
library(lmtest)
library(foreign)
library(ggplot2)
library(Rmisc)

#upload the data
#read the data
# DATA = as.data.frame(read.dta("panel.proj.dta"))
# 
# #colnames(DATA) = c("firm", "year", "Manage", "lncost", "profitability", "lnwage_skilled", "lnwage_UNskilled",
# #  "size", "lnage", "expint","markets", "private", "service")
#   
# DATAdim = dim(DATA)

#read the data
Panel = read.xlsx("dataset.xlsx",sheetIndex = 1)

lag = function(data,lags){
lags = sapply(1:lags, function(x) c(rep(NA, length.out = x), 
                                   (data)[1:(length((data)) - x)]))
  return(lags)  
}

#conditioning the plots
coplot(y ~ year|country, type = "l", data = Panel)
coplot(y ~ year|country, type = "b", data = Panel)

##bars are plotted from left to right starting at the bottom row
Panelsum = summarySE(Panel,measurevar = "y",groupvars = c("country"))
PanelsumAY = summarySE(Panel,measurevar = "y",groupvars = c("year"))


ggplot(Panelsum, aes(x=country, y=y, group = 1)) + 
  geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=.3, color = "blue") +
  geom_line(lwd = 1) +
  geom_point()+
  ggtitle("Heterogeneity accross countries")
ggsave("HetAC.pdf")


ggplot(PanelsumAY, aes(x=factor(year), y=y, group = 1)) + 
  geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=.3, color = "blue") +
  geom_line(lwd = 1) +
  geom_point()+
  scale_x_discrete()+
  xlab("years")+
  ggtitle("Heterogeneity accross years")
ggsave("HetAY.pdf")

#####################################
#######FIXED EFFECTS Models##########
#####################################

###run a simple OLS regression for reference
simple.ols = lm(y ~ x1, data = Panel) #with intercept
simple.ols2 = lm(y ~ x1-1, data = Panel) #without

#####let's model fixed effects with OLS and dummy variables

fixed.dlm = lm(y ~ x1 + factor(Panel$country)-1, data = Panel) #OLS with dummy vars
summary(fixed.dlm)

### plot the fitted values
yhat = fixed.dlm$fitted ##fitted values from OLS with dummy vars

#construct a scatterplot given the dummy variables
scatterplot(yhat ~ Panel$x1|Panel$country, xlab = "x1", ylab = "yhat")
abline(simple.ols, col = "red", lwd = 2)



apsrtable(simple.ols,fixed.dlm,model.names = c("OLS","OLS.w.Dummies"))

###Result: individual heterogeneity matters

#####Panel data

fixed.plm = plm(formula = y~x1,data = Panel,index = c("country","year"),model = "within")
summary(fixed.plm)

fixed.effects = fixef(fixed.plm) ###fixed effects for the countries (i.e. values of the intercept for each country)

#test the significance: suppose OLS is better

pFtest(fixed.plm, simple.ols)
###H0 is rejected - fixed effects is better

###########################################
###########RANDOM EFFECTS MODELS###########
###########################################

random.plm = plm(formula = y~x1, data = Panel, index = c("country", "year"),model = "random")
summary(random.plm)

###the coefficients include within and between variation and thus are tricky to interpret
###generally, the regression seem to be not as significant as fixed effects model


###run a Hausmann test

phtest(fixed.plm,random.plm)
###here the NUll hypothesis is that preferred model is random effets
###we cannot reject the null, strictly speaking

################################
#########Variations#############
################################
#########Fixed Time effects#####
################################

plm.data = pdata.frame(Panel,index = c("country", "year"))

fixed.plm.time = plm(formula = y~x1 + factor(year),data = plm.data)
summary(fixed.plm.time)
#F-statistic is not significant 

###test for significance of time effets

pFtest(fixed.plm.time,fixed.plm)

#no need to use fixed time effects

plmtest(fixed.plm,effect = "time",type = "bp")
##another evidence to reject significance of time effects


####Regular OLS from panel data - pooling model

pooled.plm = plm(formula = y~x1,data = plm.data, model = "pooling")
summary(pooled.plm)
summary(simple.ols)

##both are identical

##LM test for random effects. H0: OLS is better

plmtest(pooled.plm,type = "bp")
##do not reject H0 - no evidence for random effects model.


#################################################
###########test for serial and contemporaneous correlation###########
#################################################
#Sperical errors are very important


####contemporaneous correlation####

#breush-pagan test
pcdtest(fixed.plm,test = "lm")
##do not reject independence of residuals

#pesaran test
pcdtest(fixed.plm,test = "cd")
#do not reject independence of residuals

#the residuals are OK

####serial correlation#####

pbgtest(fixed.plm)

#no serial correlation in residuals


####UNIT ROOT TEST#####

adf.test(x = plm.data$y,k = 2)
##reject the null of unit roots


####Test for homoskedasticity #####

bptest(y~x1 + factor(country),data = plm.data,studentize = F)
##test indicates some heteroscedasticity

###no serial correlation, but there is some heteroscedasticity - no spherical errors. Use HAC standard errors.

###extract coefficients and summary statistics from the corresponding output and test wrt HAC matrices 
coeftest(random.plm) ##standard estimates

coeftest(random.plm,vcovHC)  ##heteroscedasticity consistent stand errors

coeftest(random.plm,vcovHC(random.plm,type = "HC1")) 
coeftest(random.plm,vcovHC(random.plm,type = "HC2")) 
coeftest(random.plm,vcovHC(random.plm,type = "HC3")) 

##standard errors for the random effects model given different HAC matrices
sapply(c("HC0","HC1","HC2","HC3"), function(x) sqrt(diag(vcovHC(random.plm, type = x))))

###repeat the tests for the fixed effects model
coeftest(fixed.plm) ##standard estimates

coeftest(fixed.plm,vcovHC)  ##heteroscedasticity consistent stand errors

coeftest(fixed.plm,vcovHC(fixed.plm,type = "HC1")) 
coeftest(fixed.plm,vcovHC(fixed.plm,type = "HC2")) 
coeftest(fixed.plm,vcovHC(fixed.plm,type = "HC3")) 

###fixed effects become less significznt if we control for heteroscedasticity

##standard errors for the fixed effects model given different HAC matrices
sapply(c("HC0","HC1","HC2","HC3"), function(x) sqrt(diag(vcovHC(fixed.plm, type = x))))



####################################################
########DYNAMIC PANEL DATA##########################
####################################################
library("pdynmc")



