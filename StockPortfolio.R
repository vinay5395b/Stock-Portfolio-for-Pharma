#Final project--04/16/2020


# Download the 3 data set, and lable them 
#   ROC, GIL, MOD. These data sets reperesent daily prices 
#   of three pharma companies
#   Set the first column in each data set to the date format 
#   and the remaining columns in numerical format.

library(readxl)
ROC	<-	read_excel("RocheData.xlsx",	col_types	=	c("date","numeric","numeric"))
head(ROC)		
GIL	<-	read_excel("GileadData.xlsx",	col_types	=	c("date","numeric","numeric"))
head(GIL)

MDR	<-	read_excel("ModernaData.xlsx",	col_types	=	c("date","numeric","numeric"))
head(MDR)
names(MDR)



#2) 
date	<-	ROC$date	
ROCPrice	<-	ROC$`sharePrice`	
GILPrice	<-	GIL$`sharePrice`
MDRPrice	<-	MDR$`sharePrice`


#3) Use date variable to create attribute "time" for 
date
attr(ROCPrice,"time")	<-	date
attr(GILPrice,"time")	<-	date
attr(MDRPrice,"time")	<-	date

#4) Create three variables that represent daily returns on all three shares
#   by using returnseries()(part of FRAPO package) function. 

library(FRAPO)
ROCRet	<-	returnseries(ROCPrice)
GILRet	<-	returnseries(GILPrice)
MDRRet	<-	returnseries(MDRPrice)	

#5) Use date variable to create attribute "time" 
attr(ROCRet,"time")	<-	date
attr(GILRet,"time")	<-	date
attr(MDRRet,"time")	<-	date	


#6) Create a character variable PriceDates which extracts the dates 
#   from the ROCRet variable by using 
#   as.character(format(as.POSIXct(attr()),"%Y-%m-%d")) function

PriceDates	<-	as.character(format(as.POSIXct(attr(ROCRet,	"time")),"%Y-%m-%d"))



ROCReturns	<-	timeSeries(ROCRet,	charvec	=	PriceDates)
GILReturns	<-	timeSeries(GILRet,	charvec	=	PriceDates)
MDRReturns	<-	timeSeries(MDRRet,	charvec	=	PriceDates)

#8)
colnames(ROCReturns)	<-	"ROCReturns"	
colnames(GILReturns)	<-	"GILReturns"	
colnames(MDRReturns)	<-	"MDRReturns"	

#9) Divide the output window into 3 by 2 matrix by using par() function
par(mfrow=c(3,2))	

# 10) Generate a time series plot of Daily Returns
#   
library(fBasics)
seriesPlot(ROCReturns)	
seriesPlot(GILReturns)	
seriesPlot(MDRReturns)	

#11) Generate a box plot of Returns
boxPlot(ROCReturns)	
boxPlot(GILReturns)	
boxPlot(MDRReturns)	

#12)  Generate a acf and pacf of Returns

acf(ROCReturns,lag.max=20,na.action	=	na.omit, plot	=	TRUE)
pacf(ROCReturns,	na.action	=	na.pass,plot	=	TRUE)	

acf(GILReturns,lag.max=20,na.action	=	na.omit,plot	=	TRUE)
pacf(GILReturns,	na.action	=	na.pass,plot	=	TRUE)	

acf(MDRReturns,lag.max=20,na.action	=	na.omit,plot	=	TRUE)
pacf(MDRReturns,	na.action	=	na.pass,plot	=	TRUE)	

#13) Generate a QQ plots 

par(mfrow=c(3,1))
qqnormPlot(na.omit(ROCReturns))	
qqnormPlot(na.omit(GILReturns))	
qqnormPlot(na.omit(MDRReturns))	


### We can see that the distributions are normal


#14) Generate acf and pcf of the absolute returns
ROCabsreturn	<-	abs(ROCReturns)
GILabsreturn	<-	abs(GILReturns)
MDRabsreturn	<-	abs(MDRReturns)


#15) Generate Volotility Clustering Plot of absolute daily returns 

plot(ROCabsreturn)	
plot(GILabsreturn)	
plot(MDRabsreturn)	


#16)  Create new data set called PriceCoins that includes prices 
#     of all three shares

PriceShare	<-	cbind(ROCPrice,GILPrice,MDRPrice)	


#17) by using the package zoo create an element of class "zoo" 
# labled PriceCoinsZoo, and includes Prices of all three shares
library(zoo)
PriceShareZoo	<-	as.zoo(PriceShare)[,c("ROCPrice","GILPrice","MDRPrice")]

#18) Plot a time series graph of prices of all three coins

plot(PriceShareZoo)	

#19) Create a variable that calculates daily returns 
#   (first difference of natural logs) on different shares

ShareReturn	<-	diff(log(PriceShareZoo))	*	100
head(ShareReturn)	

#20) Plot a time series graph of returns of all three shares

plot(ShareReturn)

#21)  Plot cross covariance functions between returns and between 
#     absolute returns 

par(mfrow=c(3,2),mar=c(5.1,	4.1,	4.1,	2.1),	oma=c(0,	0,	0,	0),	mgp=c(3,	1,	0),	las=1)
ccf(ShareReturn[,1],	ShareReturn[,2])	
ccf(ShareReturn[,1],	ShareReturn[,3])	
ccf(ShareReturn[,2],	ShareReturn[,1])	
ccf(ShareReturn[,2],	ShareReturn[,3])
ccf(ShareReturn[,3],	ShareReturn[,2])	
ccf(ShareReturn[,3],	ShareReturn[,1])

#22) Generate plots of rolling correlations 

rollc	<-	function(x)	{dim	<-	ncol(x)	
rcor	<-	cor(x)	[lower.tri(diag(dim),	diag	=	FALSE)]	
return(rcor)	}

rcor	<-	rollapply(ShareReturn,	width	=	250,	rollc,	align	=	"right",	by.column	=	FALSE)
colnames(rcor)	<-	c("Roche	vs	Gilead",	"Roche	vs	Moderna",	"Gilead	vs	Moderna")

plot(rcor,	main	=	"",	xlab	=	"")

#23) Getting average returns
expROCret <- mean(ShareReturn$ROCPrice) #0.06831144
expGILret <- mean(ShareReturn$GILPrice) #0.02573128
expMDRret <- mean(ShareReturn$MDRPrice) #0.1967395

# MDR has the highest return average of all three stocks



##### CALCULATING RISK


#24) formula that takes return series of the three stocks and 
#    associated weights as inputs and returns a vector with values of risk 
#    and expected return for a portfolio of three stocks. For this project, we are using
#    weights as  

portriskret<-function(x,y,z,wx,wy){
  varx<-var(x, na.rm = TRUE)
  vary<-var(y, na.rm = TRUE)
  varz<-var(z, na.rm = TRUE)
  meanx<-mean(x, na.rm = TRUE)
  meany<-mean(y, na.rm = TRUE)
  meanz<-mean(z, na.rm = TRUE)
  corxy<-cor(x,y, use="pairwise.complete.obs")
  corxz<-cor(x,z, use="pairwise.complete.obs")
  coryz<-cor(z,y, use="pairwise.complete.obs")
  risk<-wx^2*varx+wy^2*vary+(1-wx-wy)^2*varz+
    2*sqrt(varx)*sqrt(vary)*wx*wy*corxy+
    2*sqrt(varx)*sqrt(varz)*wx*(1-wx-wy)*corxz+
    2*sqrt(vary)*sqrt(varz)*wy*(1-wx-wy)*coryz
  ERp<-wx*meanx+wy*meany+(1-wx-wy)*meanz
  c(ERp,risk)
}

rocret <- ShareReturn$ROCPrice
gilret <- ShareReturn$GILPrice
mdrret <- ShareReturn$MDRPrice

portriskret(rocret, gilret, mdrret, 0.6, 0.35)

# This gives us an expected return value of 0.059 and risk value 2.204

# 24) Using the covarience "coinscov" matrix to find weights of the 
#    "global minimum variance portfolio". 

stockcov <- cov(ShareReturn, use="pairwise.complete.obs")
stockcov

GMV<-PGMV(stockcov)
GMV

# The optimal weights suggested by the GMV method are
# ROCPrice GILPrice MDRPrice 
# 62.8578  32.3926   4.7495
# which are not too far from our default weights

# Let's check if these weights give us better results

portriskret(rocret, gilret, mdrret, 0.6285, 0.3239)
# This gives us an expected return value of 0.056 and risk value 2.201

# As we can see, there is not much difference between the values when the weights are changed


# storing the weights
w<-Weights(GMV)
wroc<-as.numeric(w[1])/100
wgil<-as.numeric(w[2])/100
wmdr<-as.numeric(w[3])/100


library(timeSeries)
library(ghyp)
library(fBasics)

myport <- data.frame(ShareReturn)
row.names(myport) <- NULL
datets<-as.character(PriceDates)
datets <- datets[-1]
myportts<-timeSeries(myport, charvec = datets)

AIC <- stepAIC.(myportts, control = list(maxit = 1000))
AIC$fit.table

# The AIC value for VG is the lowest. Hence, we will be choosing for our portfolio
VGfit <- fit.VGmv(myportts, symmetric = FALSE,
                      control = list(maxit = 1000))
summary(VGfit)

## Calculating losses


portlosstsROC <- timeSeries(-1 * ShareReturn$ROCPrice, charvec = datets)
portlosstsGIL <- timeSeries(-1 * ShareReturn$GILPrice, charvec = datets)
portlosstsMDR <- timeSeries(-1 * ShareReturn$MDRPrice, charvec = datets)


#Using fExtemes package and blockMaxima() function extract the highest monthly losses of our 
#    portfolio

par(mfrow=c(3,1))
library(fExtremes)
portlossbmmROC <- blockMaxima(portlosstsROC, block = "monthly", doplot = TRUE)
portlossbmmGIL <- blockMaxima(portlosstsGIL, block = "monthly", doplot = TRUE)
portlossbmmMDR <- blockMaxima(portlosstsMDR, block = "monthly", doplot = TRUE)

#25) Using fExtemes package estimate generalized extreme value function for "portlossbmm". 
# Using maximum likelyhood method.

portlossgevROC <- gevFit(portlossbmmROC, type="mle")
summary(portlossgevROC)

portlossgevGIL <- gevFit(portlossbmmGIL, type="mle")
summary(portlossgevGIL)

portlossgevMDR <- gevFit(portlossbmmMDR, type="mle")
summary(portlossgevMDR)


## 26) gevrlevelPlot() function to estimate the highest losses with 90%, 95% and 99% confidence. 
 
# Showing the plots for different confidence values
gevrlevelPlot(portlossgevROC, ci = c(0.9, 0.95, 0.99))

#                     min       v     max    kBlocks
#GEV Return Level 4.353758 6.25558 15.9533      20

gevrlevelPlot(portlossgevGIL, ci = c(0.9, 0.95, 0.99))

#                     min       v      max    kBlocks
#GEV Return Level 4.935951 6.35352 11.85618      20

gevrlevelPlot(portlossgevMDR, ci = c(0.9, 0.95, 0.99))

#                     min       v      max    kBlocks
#GEV Return Level 11.90195 14.1529 20.42569      20




### 27) Using ismev and evir libraries to estimate generalized extreem value function for "portlossbmm".

# for ROC
detach("package:fExtremes", unload=TRUE)
library(ismev)
library(evir)

date <- date[-1]
portlossROC <- -1 * ShareReturn$ROCPrice
attributes(portlossGIL)$times <- date


GEV2 <- gev(portlossROC, block = "month")
GEV2
gevismev <- gev.fit(GEV2$data)
gev.diag(gevismev)
mLoss <- max(gevismev$data)
mLoss 
# Maximum loss = 9.388

# 28) Calculating years
mYears <- 1 / (1 - pgev(mLoss, mu = gevismev$mle[1],
                        sigma = gevismev$mle[2],
                        xi = gevismev$mle[3])) / 2
mYears

# 33 years
