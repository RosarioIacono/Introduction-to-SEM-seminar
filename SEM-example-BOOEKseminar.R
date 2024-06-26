# Example of covariance based Structural Equation Modeling testing
# Sources:
#https://cran.r-project.org/web/packages/lavaanPlot/vignettes/Intro_to_lavaanPlot.html
#https://www.imachordata.com/2011/05/02/ecological-sems-and-composite-variables-what-why-and-how/

library(lavaan)
library(lavaanPlot)

library(tidyverse)
library(semPlot)

cards<-read.table("./data/cardinale_et_al_2008_data.txt", sep="\t", header=T)
cards<-na.omit(cards)

cardModel<-'
  SA ~ logN + logNcen2 + SR
  logChl ~ SA + logN
  GPP ~ logN + logChl
  SR ~~ 0*logN + 0*logNcen2 #from path model - otherwise these are estimated
  '

#fixed.x=F in order to allow the dropping of paths between
#SR and logN and LogNcen2
cardFit <- sem(cardModel, data=cards, fixed.x=F)

lavaanPlot(model = cardFit, 
           #labels = labels, 
           node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE, 
           stand = TRUE)

semPaths(cardFit, what = "eq", whatLabels = "std", style = "mx",  
         color = colorlist, rotation =1 , layout = "tree",  nCharNodes = 7, 
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5)


#extracting information from the model
summary(cardFit) #gives a nice overview of a fitted model
standardizedSolution(cardFit) #returns a data.frame containing all the model parameters in the rows
parameterEstimates(fit) # similar to standardizedSolution() only shows the standardized parameter estimates and corresponding standard errors, z-values, p-values and confidence intervals.
lavResiduals(fit) #gives more extensive information about the residuals
AIC(fit) #Akaike information criterion, estimates the quality of each model, relative to each of the other models


compositeModel<-'
	#1) define the composite, scale to logN
	Nitrogen ~ logN + 1*logNcen2 #loading on the significant path!

	#2) Specify 0 error variance
	Nitrogen ~~ 0*Nitrogen
  
  	#3) now, because we need to represent this as a latent variable
  	#show how species richness is an _indicator_ of nitrogen
  	Nitrogen =~ SA

	#4) BUT, make sure the variance of SA is estimated
  	SA ~~ SA

	#Regional Richness also has an effect
	SA ~ SR
	
	#And account for the derivation of the square term from the linear term
	logNcen2 ~ logN
  	  	
  	'

# we specify std.lv=T so that the Nitrogen-SA relationship isn't fixed to 1
compositeFit <-sem(compositeModel, data=cards, std.lv=T)

lavaanPlot(model = compositeFit, 
           #labels = labels, 
           node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = FALSE, 
           stand = TRUE)

summary(compositeFit)
standardizedSolution(compositeFit)

#Multiple group SEM
# Data from The classic Holzinger and Swineford (1939) dataset consists of mental ability test scores 
#of seventh- and eighth-grade children from two different schools (Pasteur and Grant-White). In the 
#original dataset (available in the MBESS package), there are scores for 26 tests. However, a smaller 
#subset with 9 variables is more widely used in the literature (for example in Joreskog's 1969 paper, 
#which also uses the 145 subjects from the Grant-White school only).
#?HolzingerSwineford1939

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- sem(HS.model, 
           data = HolzingerSwineford1939, 
           group = "school")

lavaanPlot(model = fit, 
           #labels = labels, 
           node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE, 
           stand = TRUE)


semPaths(fit, what = "eq", 
         whatLabels = "std", 
         style = "mx",  
         color = colorlist, 
         rotation =1, 
         layout = "tree",  
         nCharNodes = 7,
         shapeMan = "rectangle", 
         sizeMan = 8, 
         sizeMan2 = 5)


summary(cardFit) #gives a nice overview of a fitted model
standardizedSolution(cardFit) #returns a data.frame containing all the model parameters in the rows
parameterEstimates(fit) # similar to standardizedSolution() only shows the standardized parameter estimates and corresponding standard errors, z-values, p-values and confidence intervals.
lavResiduals(fit) #gives more extensive information about the residuals
AIC(fit) #Akaike information criterion, estimates the quality of each model, relative to each of the other models

