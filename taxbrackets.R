# Sets the working directory. This sets it to the "index" folder on my desktop

setwd("C:/Users/kep/Documents/GitHub/taxbrackets")


#Clears all datasets and variables from memory

rm(list=ls())

#Load Data
cpi<-read.csv("cpitables.csv", header = TRUE, fill = TRUE, sep = ",")

##########################Consumer Price Index Calculations###########################

cpi$average = NULL
cpi$fiscalyear = NULL

for (i in 2:length(cpi[,1])){ 
  cpi$average[i]<-rowMeans(cpi[i,2:12], na.rm=TRUE)
  }

#Starting in FY1914. Do not have data on CY1912, so cannot complete FY 2013

for (i in 2:length(cpi[,1])){
  cpi$fiscalyear[i]<-sum(rowSums(cpi[i-1,10:13]),rowSums(cpi[i,2:9]))/12
}

#Reference Year (This sets the year to which you are adjusting)
refyear<-2014

#####################Ordinary Income Tax Bracket calculations##########################

#Input base-year parameters:

  #Base Year 1992 brackets (Tops of Brackets)
  baseyearmarried1992<-c(44200,89150)
  names(baseyearmarried1992) = c("15%", "25%")
  baseyearheadofhousehold1992<-c(29600,76400)
  names(baseyearheadofhousehold1992) = c("15%", "25%")
  baseyearsingle1992<-c(22100,53500)
  names(baseyearsingle1992) = c("15%", "25%")

  #Base Year 1993 brackets (Tops of Brackets)
  
  baseyearmarried1993<-c(140000,250000)
  names(baseyearmarried1993) = c("28%", "33%")
  baseyearheadofhousehold1993<-c(127500,250000)
  names(baseyearheadofhousehold1993) = c("28%", "33%")
  baseyearsingle1993<-c(115000,250000)
  names(baseyearsingle1993) =c("28%", "33%")

  #Base Year 2002 Brackets (Tops of Brackets)
  baseyearsingle2002<-c(7000)
  names(baseyearsingle2002) = c("10%")
  baseyearheadofhousehold2002<-c(10000)
  names(baseyearheadofhousehold2002) = c("10%")
  baseyearmarried2002<-c(14000)
  names(baseyearmarried2002) = c("10%")

  #Base Year 2012
  baseyearsingle2012<-c(400000)
  names(baseyearsingle2012) = c("35%")
  baseyearheadofhousehold2012<-c(425000)
  names(baseyearheadofhousehold2012) = c("35%")
  baseyearmarried2012<-c(450000)
  names(baseyearmarried2012) = c("35%")

#Adjust Each bracket for inflation by its base year

  #1992 Base Year adjustments
  baseyearmarried1992<-baseyearmarried1992*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==1992])
  baseyearsingle1992<-baseyearsingle1992*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==1992])
  baseyearheadofhousehold1992<-baseyearheadofhousehold1992*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==1992])

  #1993 Base Year Adustments
  baseyearmarried1993<-baseyearmarried1993*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==1993])
  baseyearheadofhousehold1993<-baseyearheadofhousehold1993*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==1993])
  baseyearsingle1993<-baseyearsingle1993*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==1993])

  #2002 Base Year Adjustments
  baseyearsingle2002<-baseyearsingle2002*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==2002])
  baseyearheadofhousehold2002<-baseyearheadofhousehold2002*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==2002])
  baseyearmarried2002<-baseyearmarried2002*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==2002])

  #2012 Base Year Adjustments
  baseyearsingle2012<-baseyearsingle2012*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==2012])
  baseyearheadofhousehold2012<-baseyearheadofhousehold2012*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==2012])
  baseyearmarried2012<-baseyearmarried2012*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==2012])

#Rounding Each base Year by IRS Method (Down to Nearest $50 for all brackets Except for 10% single bracket ($25))

  baseyearmarried1992<-baseyearmarried1992-(baseyearmarried1992%%50)
  baseyearsingle1992<-baseyearsingle1992-(baseyearsingle1992%%50)
  baseyearheadofhousehold1992<-baseyearheadofhousehold1992-(baseyearheadofhousehold1992%%50)
  
  baseyearmarried1993<-baseyearmarried1993-(baseyearmarried1993%%50)
  baseyearheadofhousehold1993<-baseyearheadofhousehold1993-(baseyearheadofhousehold1993%%50)
  baseyearsingle1993<-baseyearsingle1993-(baseyearsingle1993%%50)
  
  baseyearsingle2002<-baseyearsingle2002-(baseyearsingle2002%%25) #the rounding convention for 2002 single is for 25 dollars
  baseyearheadofhousehold2002<-baseyearheadofhousehold2002-(baseyearheadofhousehold2002%%50)
  baseyearmarried2002<-baseyearmarried2002-(baseyearmarried2002%%50)
  
  baseyearsingle2012<-baseyearsingle2012-(baseyearsingle2012%%50)
  baseyearheadofhousehold2012<-baseyearheadofhousehold2012-(baseyearheadofhousehold2012%%50)
  baseyearmarried2012<-baseyearmarried2012-(baseyearmarried2012%%50)

#Combine each baseyear to get the three brackets (remember, these are bracket tops!)
#There is a 39.6 percent bracket on highest income

  single<-c(baseyearsingle2002,baseyearsingle1992,baseyearsingle1993,baseyearsingle2012)
  headofhousehold<-c(baseyearheadofhousehold2002,baseyearheadofhousehold1992,baseyearheadofhousehold1993,baseyearheadofhousehold2012)
  married<-c(baseyearmarried2002,baseyearmarried1992,baseyearmarried1993,baseyearmarried2012)

##########################Standard Deduction and Personal Exemption################################

#Parameters in Tax Code
  standarddeduction<-c(3000,6000,4400)
  names(standarddeduction)<-c("single","married","head of household")
  personalexemption<-2000

#CPI adjustments (Base year for standard deduction: 1987, personal exemption: 1988)
  standarddeduction<-standarddeduction*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==1987])
  personalexemption<-personalexemption*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==1988])

#Rounding to nearest 50
  standarddeduction<-standarddeduction-(standarddeduction%%50)
  personalexemption<-personalexemption-(personalexemption%%50)

###################################PEP and Pease###########################################

#Parameters in Tax Code
  PEPandPease<-c(250000,300000,275000)
  names(PEPandPease)<-c("single","married","head of household")

#CPI Adjustments. Base Year for PEP and Pease is 2012
  PEPandPease<-PEPandPease*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==2012])

#Rounding to nearest 50
  PEPandPease<-PEPandPease-(PEPandPease%%50)

#Calculating the phase out of Pease: The first 2 percent is taken immediately at the limit. To do this you need
#to subtract out 2500 from the phaseout length. (I don't actually know if this is how it actually works, but its how the IRS 
#Did it last year)
  PEPphaseout<-PEPandPease+(2500/.02)-2500

##################################AMT Exemption Amounts######################################

#Parameters in Tax Code
  AMT<-c(50600,78750)
  names(AMT) = c("single","married")

#CPI Adjustment for AMT (Base Year 2011)
  AMT<-AMT*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==2011])

#Rounding (AMT is rounded to nearest $100, in either direction)
  AMT<-round(AMT, digits=-2)

##################################Earned Income Tax Credit###################################

#EITC Parameters: Remember: [row,column]

  #Credit Size
  EITCCredit<-matrix(c(.0765,0.0765,.34,.1598,.4,.2106,.45,.2106),4,2, byrow = TRUE)
  colnames(EITCCredit) = c("phase-in rate", "phase-out rate")
  rownames(EITCCredit) = c("No Children","One Child","2 Children","Three or More Children")

  #Income Parameters (There are only single parameters in law)
  EITCIncomeSingle<-matrix(c(4220,5280,6330,11610,8890,11610,8890,11610),4,2, byrow = TRUE)
  colnames(EITCIncomeSingle) = c("Income: Max Credit", "Income: Phase-out")
  rownames(EITCIncomeSingle) = c("No Children","One Child","2 Children","3 or More Children")

#Adjust for inflation
  EITCIncomeSingle<-EITCIncomeSingle*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==1995])

#EITC Married Parameters (Original EITC did not adjust for marriage, this was later added)

  #Marriage Penalty Fix
  penalty<-5000
  #The Penalty fix has a 2008 base year
  penalty<-penalty*(cpi$fiscalyear[cpi$Year==refyear]/cpi$fiscalyear[cpi$Year==2008])
  #Round to nearest $10
  penalty<-round(penalty, digits= -1)

#Creating the Married EITC parameters

#Create variable (identical to Single parameters)
EITCIncomeMarried<-EITCIncomeSingle
  
  #Add the marriage adjustment (called penalty)
  EITCIncomeMarried[1:4,2]<-EITCIncomeMarried[1:4,2]+penalty

#Round both to nearest 10 AFTER marriage penalty and inflation adjustments
  EITCIncomeSingle<-round(EITCIncomeSingle, digits = -1)
  EITCIncomeMarried<-round(EITCIncomeMarried, digits = -1)

#EITC Single TF Table:
  EITCSingle<-matrix(c(c(EITCIncomeSingle[1:4,1]),c(EITCIncomeSingle[1:4,1]*EITCCredit[1:4,1]),c(EITCIncomeSingle[1:4,2]),c(EITCIncomeSingle[1:4,2]+((EITCIncomeSingle[1:4,1]*EITCCredit[1:4,1])/EITCCredit[1:4,2]))),4,4,byrow = TRUE)
  colnames(EITCSingle) = c("No Children","One Child","Two Children", "Three of More Children")
  rownames(EITCSingle) = c("Earned Income Level for Max Credit", "Maximum Credit","Income When Phase-out Begins", "Income Level When Phase-out Ends (Credit Equals Zero)")

#EITC Married TF Table:
  EITCMarried<-matrix(c(c(EITCIncomeMarried[1:4,1]),c(EITCIncomeMarried[1:4,1]*EITCCredit[1:4,1]),c(EITCIncomeMarried[1:4,2]),c(EITCIncomeMarried[1:4,2]+((EITCIncomeMarried[1:4,1]*EITCCredit[1:4,1])/EITCCredit[1:4,2]))),4,4,byrow = TRUE)
  colnames(EITCMarried) = c("No Children","One Child","Two Children", "Three of More Children")
  rownames(EITCMarried) = c("Earned Income Level for Max Credit", "Maximum Credit","Income When Phase-out Begins", "Income Level When Phase-out Ends (Credit Equals Zero)")

#Needs to be rounded again after calculting the credit amounts and Phaseout limits (Rounded to nearest dollar)
  EITCSingle<-round(EITCSingle, digits = 0)
  EITCMarried<-round(EITCMarried, digits = 0)

#Results:
  single
  married
  headofhousehold
  standarddeduction
  personalexemption
  PEPandPease
  PEPphaseout
  AMT
  EITCSingle
  EITCMarried