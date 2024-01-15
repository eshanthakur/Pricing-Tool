Lxs = c(10000.0000,9994.0000,9988.0636,9982.2006,9976.3909,9970.6346,9964.9313,9959.2613,9953.6144,9947.9807,9942.3402,9936.6730,9930.9694,9925.2094,9919.3535,9913.3821,9907.2655,9900.9645,9894.4299,9887.6126,9880.4540,9872.8954,9864.8688,9856.2863,9847.0510,9837.0661,9826.2060,9814.3359,9801.3123,9786.9534,
        9771.0789,9753.4714,9733.8865,9712.0728,9687.7149,9660.5021,9630.0522,9595.9715,9557.8179,9515.1040,9467.2906,9413.8004,9354.0040,9287.2164,9212.7143,9129.7170,9037.3973,8934.8771,8821.2612,8695.6199,8557.0118,8404.4916,8237.1329,8054.0544,7854.4508,7637.6208,7403.0084,7150.2401,6879.1673,6589.9258,
        6282.9803,5959.1680,5619.7577,5266.4604,4901.4789,4527.4960,4147.6708,3765.5998,3385.2479,3010.8395,2646.7416,2297.2976,1966.6499,1658.5545,1376.1906,1121.9889,897.5025,703.3242,539.0643,403.4023,294.2061,208.7060,143.7120,95.8476,61.7733,38.3796,22.9284,13.1359,7.1968,3.7596,1.8669,0.8784,0.3903,0.1632,0.0640,0.0234,
        0.0080,0.0025,0.0007,0.0002,0.0000,0.0000,0.0000,0.0000)
length(Lxs)-17
Lxs_X= Lxs[-c(1:4)]
Lxs_X
Lxs_x= Lxs_X[1:71]
Lxs_x

##whole life annuity advance
Wholelifeannuity = function(age,interestrate){
  discountrate = 1/(1 + interestrate)
  survivalprobabilities = (Lxs_x[-(1:(age-20+1))]/Lxs_x[age-20 + 1])
  alldiscountrates = discountrate^(1:length(survivalprobabilities))
  Output = sum(c(1, (alldiscountrates * survivalprobabilities)))
  return(Output)}
Wholelifeannuity(21,0.04)

## whole life annuity in arrears
Wholelifeannuity_arrears = function(age, interestrate){
  output= Wholelifeannuity(age,interestrate)-1
  return(output)
}
Wholelifeannuity_arrears(20,0.04)


##term annuity in advance

Termannuitydue = function(age,interestrate,term){
  discountfactor = (Lxs_x[age-20+1+term]/Lxs_x[age-20+1]) * ((1/(1 + interestrate))^term)
  Output = Wholelifeannuity(age,interestrate) - discountfactor*Wholelifeannuity(age+term,interestrate)
  return(Output)}

##Assurance

##Whole Life Assurance
##1)Whole life assurance paid at the end of the year
WholelifeAssurance=function(age,interestrate){
  output= ( 1- ((interestrate/(1+interestrate))* Wholelifeannuity(age,interestrate)) )
  return(output)
}


##1)Whole life assurance paid immediately on death
WholelifeAssurance_Tx= function(age,interestrate){
  output= ((1+interestrate)^0.5) * WholelifeAssurance(age,interestrate)
  return(output)
}


##term assurance
##1)Term assurance paid at the end of the year
Termassurance= function(age,interestrate,term){
  discountfactor = (Lxs_x[age-20+1+term]/Lxs_x[age-20+1]) * ((1/(1 + interestrate))^term)
  output= WholelifeAssurance(age,interestrate) - discountfactor*WholelifeAssurance(age+term,interestrate)
  return(output)
}



##2)Term assurance paid immediately on death

Termassurance_Tx= function(age,interestrate,term){
  output= ((1+interestrate)^0.5) * Termassurance(age,interestrate,term)
  return(output)
}


##Pure Endownment
##1)Pure endownment paid at the end of the year
PureEndownment= function(age,interestrate,term){
  output = (Lxs_x[age-20+1+term]/Lxs_x[age-20+1]) * ((1/(1 + interestrate))^term)
  return(output)
}

##2)Pure endownment paid aimmediately on death

PureEndownment_Tx= function(age,interestrate,term){
  output= ((1+interestrate)^0.5) * PureEndownment(age,interestrate,term)
  return(output)
}


##Endownment
##1) endownment paid at the end of the year
Endownment = function(age, interestrate, term){
  output= 1 - ((interestrate/(1+interestrate))*Termannuitydue(age,interestrate,term))
  return(output)
}

##2)Endownment paid immediately on death

Endownment_Tx= function(age,interestrate,term){
  output= Termassurance_Tx(age,interestrate,term) + PureEndownment_Tx(age,interestrate,term)
  return(output)
}



##PREMIUMS for SA paid at the end of year of death

##1) TERM ASSUARANCE

##1.1 single premium

single_premium_TA = function(age,interestrate, term, initial_exp, claim_exp, SA){
  a= ( (SA* Termassurance(age,interestrate,term)) * (1+claim_exp) )
  b= ((1-initial_exp))
  output= a/b
  return(output)
}


## 1.2 level premium

level_premium_TA = function(age,interestrate, term, initial_exp, premium_exp,  claim_exp, SA){
  a= ( (SA* Termassurance(age,interestrate,term)) * (1+claim_exp) )
  b= Termannuitydue(age,interestrate,term)- initial_exp - (premium_exp* (Termannuitydue(age,interestrate,term) -1 ) ) 
  output= a/b
  return(output)
}



##2) Pure Endownment

##2.1 single premium
single_premium_PE = function(age,interestrate, term, initial_exp, claim_exp, SA){
  a= ( (SA* PureEndownment(age,interestrate,term)) * (1+claim_exp) )
  b= ((1-initial_exp))
  output= a/b
  return(output)
}


## 2.2 level premium

level_premium_PE = function(age,interestrate, term, initial_exp, premium_exp,  claim_exp, SA){
  a= ( (SA* PureEndownment(age,interestrate,term)) * (1+claim_exp) )
  b= Termannuitydue(age,interestrate,term)- initial_exp - (premium_exp* (Termannuitydue(age,interestrate,term) -1 ) ) 
  output= a/b
  return(output)
}




##3)Endownment

##3.1 single premium

single_premium_E = function(age,interestrate, term, initial_exp, claim_exp, SA){
  a= ( (SA* Endownment(age,interestrate,term)) * (1+claim_exp) )
  b= ((1-initial_exp))
  output= a/b
  return(output)
}


## 3.2 level premium
level_premium_E = function(age,interestrate, term, initial_exp, premium_exp,  claim_exp, SA){
  a= ( (SA* Endownment(age,interestrate,term)) * (1+claim_exp) )
  b= Termannuitydue(age,interestrate,term)- initial_exp - (premium_exp* (Termannuitydue(age,interestrate,term) -1 ) ) 
  output= a/b
  return(output)
}



##PREMIUMS paid immediately on death

##1) TERM ASSUARANCE

##1.1 single premium

single_premium_TA_Tx = function(age,interestrate, term, initial_exp, claim_exp, SA){
  a= ( (SA* Termassurance_Tx(age,interestrate,term)) * (1+claim_exp) )
  b= ((1-initial_exp))
  output= a/b
  return(output)
}


## 1.2 level premium

level_premium_TA_Tx = function(age,interestrate, term, initial_exp, premium_exp,  claim_exp, SA){
  a= ( (SA* Termassurance_Tx(age,interestrate,term)) * (1+claim_exp) )
  b= Termannuitydue(age,interestrate,term)- initial_exp - (premium_exp* (Termannuitydue(age,interestrate,term) -1 ) ) 
  output= a/b
  return(output)
}



##2) Pure Endownment

##2.1 single premium

single_premium_PE_Tx = function(age,interestrate, term, initial_exp, claim_exp, SA){
  a= ( (SA* PureEndownment_Tx(age,interestrate,term)) * (1+claim_exp) )
  b= ((1-initial_exp))
  output= a/b
  return(output)
}

##2.2 level premium

level_premium_PE_Tx = function(age,interestrate, term, initial_exp, premium_exp,  claim_exp, SA){
  a= ( (SA* PureEndownment_Tx(age,interestrate,term)) * (1+claim_exp) )
  b= Termannuitydue(age,interestrate,term)- initial_exp - (premium_exp* (Termannuitydue(age,interestrate,term) -1 ) ) 
  output= a/b
  return(output)
}


##3)Endownment

##3.1 single premium
single_premium_E_Tx = function(age,interestrate, term, initial_exp, claim_exp, SA){
  a= ( (SA* Endownment_Tx(age,interestrate,term)) * (1+claim_exp) )
  b= ((1-initial_exp))
  output= a/b
  return(output)
}

## 3.2 level premium

level_premium_E_Tx = function(age,interestrate, term, initial_exp, premium_exp,  claim_exp, SA){
  a= ( (SA* Endownment_Tx(age,interestrate,term)) * (1+claim_exp) )
  b= Termannuitydue(age,interestrate,term)- initial_exp - (premium_exp* (Termannuitydue(age,interestrate,term) -1 ) ) 
  output= a/b
  return(output)
}


##for y

Lxs_y = Lxs[1:71]
Lxs_y



##whole life annuity advance
Wholelifeannuity_y = function(age,interestrate){
  discountrate = 1/(1 + interestrate)
  survivalprobabilities = (Lxs_y[-(1:(age-20 + 1))]/Lxs_y[age-20 + 1])
  alldiscountrates = discountrate^(1:length(survivalprobabilities))
  Output = sum(c(1, (alldiscountrates * survivalprobabilities)))
  return(Output)}
Wholelifeannuity_y(25,0.04)

##whole life annuity arrears
Wholelifeannuity_arrears_y = function(age, interestrate){
  output= Wholelifeannuity_y(age,interestrate)-1
  return(output)
}


##term annuity in advance

Termannuitydue_y = function(age,interestrate,term){
  discountfactor = (Lxs_y[age-20+1+term]/Lxs_y[age-20+1]) * ((1/(1 + interestrate))^term)
  Output = Wholelifeannuity_y(age,interestrate) - discountfactor*Wholelifeannuity_y(age+term,interestrate)
  return(Output)}
Termannuitydue_y(20,0.4,5)


##Assurance

##Whole Life Assurance
##1)Whole life assurance paid at the end of the year
WholelifeAssurance_y=function(age,interestrate){
  output= ( 1- ((interestrate/(1+interestrate))* Wholelifeannuity_y(age,interestrate)) )
  return(output)
}



##1)Whole life assurance paid immediately on death
WholelifeAssurance_Tx_y= function(age,interestrate){
  output= ((1+interestrate)^0.5) * WholelifeAssurance_y(age,interestrate)
  return(output)
}


##term assurance
##1)Term assurance paid at the end of the year
Termassurance_y= function(age,interestrate,term){
  discountfactor = (Lxs_y[age-20+1+term]/Lxs_y[age-20+1]) * ((1/(1 + interestrate))^term)
  output= WholelifeAssurance_y(age,interestrate) - discountfactor*WholelifeAssurance_y(age+term,interestrate)
  return(output)
}

##2)Term assurance paid immediately on death

Termassurance_Tx_y= function(age,interestrate,term){
  output= ((1+interestrate)^0.5) * Termassurance_y(age,interestrate,term)
  return(output)
}


##Pure Endownment
##1)Pure endownment paid at the end of the year
PureEndownment_y= function(age,interestrate,term){
  output = (Lxs_y[age-20+1+term]/Lxs_y[age-20+1]) * ((1/(1 + interestrate))^term)
  return(output)
}

##2)Pure endownment paid aimmediately on death

PureEndownment_Tx_y= function(age,interestrate,term){
  output= ((1+interestrate)^0.5) * PureEndownment_y(age,interestrate,term)
  return(output)
}



##Endownment
##1) endownment paid at the end of the year
Endownment_y = function(age, interestrate, term){
  output= 1 - ((interestrate/(1+interestrate))*Termannuitydue_y(age,interestrate,term))
  return(output)
}

##2)Endownment paid immediately on death

Endownment_Tx_y= function(age,interestrate,term){
  output= Termassurance_Tx_y(age,interestrate,term) + PureEndownment_Tx_y(age,interestrate,term)
  return(output)
}


##PREMIUMS for SA paid at the end of year of death

##1) TERM ASSUARANCE

##1.1 single premium

single_premium_TA_y = function(age,interestrate, term, initial_exp, claim_exp, SA){
  a= ( (SA* Termassurance_y(age,interestrate,term)) * (1+claim_exp) )
  b= ((1-initial_exp))
  output= a/b
  return(output)
}


## 1.2 level premium

level_premium_TA_y = function(age,interestrate, term, initial_exp, premium_exp,  claim_exp, SA){
  a= ( (SA* Termassurance_y(age,interestrate,term)) * (1+claim_exp) )
  b= Termannuitydue_y(age,interestrate,term)- initial_exp - (premium_exp* (Termannuitydue_y(age,interestrate,term) -1 ) ) 
  output= a/b
  return(output)
}


##2) Pure Endownment

##2.1 single premium
single_premium_PE_y = function(age,interestrate, term, initial_exp, claim_exp, SA){
  a= ( (SA* PureEndownment_y(age,interestrate,term)) * (1+claim_exp) )
  b= ((1-initial_exp))
  output= a/b
  return(output)
}


## 2.2 level premium

level_premium_PE_y = function(age,interestrate, term, initial_exp, premium_exp,  claim_exp, SA){
  a= ( (SA* PureEndownment_y(age,interestrate,term)) * (1+claim_exp) )
  b= Termannuitydue_y(age,interestrate,term)- initial_exp - (premium_exp* (Termannuitydue_y(age,interestrate,term) -1 ) ) 
  output= a/b
  return(output)
}


##3)Endownment

##3.1 single premium

single_premium_E_y = function(age,interestrate, term, initial_exp, claim_exp, SA){
  a= ( (SA* Endownment_y(age,interestrate,term)) * (1+claim_exp) )
  b= ((1-initial_exp))
  output= a/b
  return(output)
}


## 3.2 level premium
level_premium_E_y = function(age,interestrate, term, initial_exp, premium_exp,  claim_exp, SA){
  a= ( (SA* Endownment_y(age,interestrate,term)) * (1+claim_exp) )
  b= Termannuitydue_y(age,interestrate,term)- initial_exp - (premium_exp* (Termannuitydue_y(age,interestrate,term) -1 ) ) 
  output= a/b
  return(output)
}



##PREMIUMS paid immediately on death

##1) TERM ASSUARANCE

##1.1 single premium

single_premium_TA_Tx_y = function(age,interestrate, term, initial_exp, claim_exp, SA){
  a= ( (SA* Termassurance_Tx_y(age,interestrate,term)) * (1+claim_exp) )
  b= ((1-initial_exp))
  output= a/b
  return(output)
}


## 1.2 level premium

level_premium_TA_Tx_y = function(age,interestrate, term, initial_exp, premium_exp,  claim_exp, SA){
  a= ( (SA* Termassurance_Tx_y(age,interestrate,term)) * (1+claim_exp) )
  b= Termannuitydue_y(age,interestrate,term)- initial_exp - (premium_exp* (Termannuitydue_y(age,interestrate,term) -1 ) ) 
  output= a/b
  return(output)
}


##2) Pure Endownment

##2.1 single premium

single_premium_PE_Tx_y = function(age,interestrate, term, initial_exp, claim_exp, SA){
  a= ( (SA* PureEndownment_Tx_y(age,interestrate,term)) * (1+claim_exp) )
  b= ((1-initial_exp))
  output= a/b
  return(output)
}

##2.2 level premium

level_premium_PE_Tx_y = function(age,interestrate, term, initial_exp, premium_exp,  claim_exp, SA){
  a= ( (SA* PureEndownment_Tx_y(age,interestrate,term)) * (1+claim_exp) )
  b= Termannuitydue_y(age,interestrate,term)- initial_exp - (premium_exp* (Termannuitydue_y(age,interestrate,term) -1 ) ) 
  output= a/b
  return(output)
}


##3)Endownment

##3.1 single premium
single_premium_E_Tx_y = function(age,interestrate, term, initial_exp, claim_exp, SA){
  a= ( (SA* Endownment_Tx_y(age,interestrate,term)) * (1+claim_exp) )
  b= ((1-initial_exp))
  output= a/b
  return(output)
}

## 3.2 level premium

level_premium_E_Tx_y = function(age,interestrate, term, initial_exp, premium_exp,  claim_exp, SA){
  a= ( (SA* Endownment_Tx_y(age,interestrate,term)) * (1+claim_exp) )
  b= Termannuitydue_y(age,interestrate,term)- initial_exp - (premium_exp* (Termannuitydue_y(age,interestrate,term) -1 ) ) 
  output= a/b
  return(output)
}


##Featured Product

J_WholeAnnuityDue= function(age_x,age_y,interestrate){
  survivialprobability_x= ( Lxs_x[-(1:(age_x-20+1))] / Lxs_x[age_x-20+1]  )
  survivialprobability_y= ( Lxs_y[-(1:(age_y-20+1))] / Lxs_y[age_y-20+1]  )
  v= (1/(1+interestrate))
  p= v^ (1:length(survivialprobability_x))
  E= sum(c(1, (p*survivialprobability_x*survivialprobability_y ) ))
  return(E)
}

J_WholeAnnuityDue(20,21,0.05)

J_TermAnnuityDue= function(age_x,age_y,interestrate,term){
  discount= (Lxs_x[age_x -20 +1+term]*Lxs_y[age_y -20 +1+term]) / (Lxs_x[age_x -20 +1 ]* Lxs_y[age_y -20 +1 ])
  E= J_WholeAnnuityDue(age_x,age_y,interestrate)- (discount * J_WholeAnnuityDue(age_x+term,age_y+term,interestrate))
  return(E)
}
J_TermAnnuityDue(20,23,0.04,5)
J_TermAnnuityDue(45,29,0.05,5)


##ddota
rrr=function(age_x,age_y,interestrate){
  output= Wholelifeannuity(age_x,interestrate)+Wholelifeannuity_y(age_y,interestrate)-J_WholeAnnuityDue(age_x,age_y,interestrate)
  return(output)
}
rrr(24,25,0.05)

we= function(age_x,age_y,interestrate,term){
  survivialprobability_x= ( Lxs_x[(1:(age_x-20+1+term))] / Lxs_x[age_x-20+1]  )
  deathprobability_y= 1 -  ( Lxs_y[(1:(age_y-20+1+term))] / Lxs_y[age_y-20+1]  )
  v= (1/(1+interestrate))
  p= v^ (1:length(term))
  E= sum(c(1, (p*survivialprobability_x*deathprobability_y* Wholelifeannuity(age_x+term,interestrate) ) ))
  return(E)
}
we(24,30,0.05,10)

be= function(age_x,age_y,interestrate,term){
  deathprobability_x= 1-( Lxs_x[(1:(age_x-20+1+term))] / Lxs_x[age_x-20+1]  )
  survivalprobability_y= ( Lxs_y[(1:(age_y-20+1+term))] / Lxs_y[age_y-20+1]  )
  v= (1/(1+interestrate))
  p= v^ (1:length(term))
  E= sum(c(1, (p*deathprobability_x*survivalprobability_y* Wholelifeannuity(age_y+term,interestrate) ) ))
  return(E)
}
be(24,30,0.05,10)

tree=function(age_x,age_y,interestrate,term){
  survivalprobability_x= ( Lxs_x[(1:(age_x-20+1+term))] / Lxs_x[age_x-20+1]  )
  survivalprobability_y=( Lxs_y[(1:(age_y-20+1+term))] / Lxs_y[age_y-20+1]  )
  v= (1/(1+interestrate))
  p= v^ (1:length(term))
  E= sum(c(1, (p*survivalprobability_x*survivalprobability_y* rrr(age_x+term,age_y+term,interestrate) ) ))
  return(E)
  
}


tree(24,30,0.05,10)

J_TermLastSurvivalContinuos = function(age_x,age_y,interestrate,term){
  output= rrr(age_x,age_y,interestrate) -we(age_x,age_y,interestrate,term)-be(age_x,age_y,interestrate,term)-tree(age_x,age_y,interestrate,term) -0.5
  return(output)
}
J_TermLastSurvivalContinuos(24,25,.03,10)

J_TermAnnuityContinuos=function(age_x,age_y,interestrate,term){
  output=J_TermAnnuityDue(age_x,age_y,interestrate,term)-0.5
  return(output)
}
J_TermAnnuityContinuos(24,25,.03,10)

TermannuityContinous_x=function(age,interestrate,term){
  Output=Termannuitydue_y(age,interestrate,term) - 0.5
  return(Output)
}
TermannuityContinous_x(24,25,10)

Annuity_payable_to_x=function(age_x,age_y,interestrate,term){
  output= -(J_TermLastSurvivalContinuos(age_x,age_y,interestrate,term)+J_TermAnnuityContinuos(age_x,age_y,interestrate,term)- TermannuityContinous_x(age_y,interestrate,term))
  return(output)
}

Annuity_payable_to_x(27,20,0.05,10)


single_premium_for_joint=function(age_x,age_y,interestrate,term,initial_exp,claim_exp,annuity_benefit){
  prem= (annuity_benefit * (1+claim_exp)* Annuity_payable_to_x(age_x,age_y,interestrate,term) ) / (1-initial_exp)
  return(prem)
}
single_premium_for_joint(24,33,0.05,6,0.02,0.03,100000)






##SHINY

library(shiny)
library(ggplot2)
library(dplyr)
ui = fluidPage(
  titlePanel("Insurance Benefit Valuation Tool"),
  sidebarPanel(
    radioButtons("PolicyHolder",label="Policy", choices = list("X","Y","XY"), selected = "X"),
    sliderInput(inputId="Age_X",label="Age of X", value = 30,min=20,max=90),
    sliderInput(inputId="Age_Y",label="Age of Y", value = 30,min=20,max=90),
    numericInput("IR",label="Interest Rate (in %)",value=4,min=0,max =15),
    radioButtons("BenefitPayment",label="Insurance Benefit payment", choices = list("End of year of death","Immediately on death"), selected = "End of year of death"),
    radioButtons("Assurance",label="Insurance Benefit", choices = list("Term Assurance","Pure Endownment","Endownment"), selected = "Pure Endownment"),
    radioButtons("Premium",label="Premium Payment", choices = list("Single Premium","Level Premium"), selected = "Single Premium"),
    numericInput(inputId="AssuredSum",label="Assured Sum",value=1,min=0,max=100000000),
    numericInput(inputId="annuity_benefit",label="Annuity Benefit",value=1,min=0,max=100000000),
    numericInput(inputId="term",label="Term",value=4,min=0,max=100000),
    numericInput("claim_exp",label="Claim Expense(%of Benefit Amount)",value=1,min=0,max =50),
    numericInput("premium_exp",label="Premium Expense(0-Single Premiums)",value=5,min=0,max =50),
    numericInput("initial_exp",label="Initial Expense",value= 5,min=0,max =100000),
  ),
  mainPanel(
    textOutput("OutputText"),
    plotOutput("Plot")
    
            )
  
)



server = function(input,output) {
  
  output$OutputText = renderText( {
    
    age_X = input$Age_X
    age_Y = input$Age_Y
    AssuredSum=input$AssuredSum
    annuity_benefit = input$annuity_benefit
    term=input$term
    InterestRate = input$IR/100
    claim_exp= input$claim_exp/100
    initial_exp=input$initial_exp/100
    premium_exp=input$premium_exp/100
    
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="End of year of death") & (input$Assurance=="Term Assurance") & (input$Premium=="Single Premium")) {Premium= single_premium_TA(age_X,InterestRate,term,initial_exp,claim_exp,AssuredSum)}
    if (  (input$PolicyHolder=="X") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Term Assurance")&(input$Premium=="Level Premium") ) {Premium= level_premium_TA(age_X,InterestRate,term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if (   (input$PolicyHolder=="X") &    (input$BenefitPayment=="End of year of death") &(input$Assurance=="Pure Endownment")&(input$Premium=="Single Premium")) {Premium= single_premium_PE(age_X,InterestRate,term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Pure Endownment")&(input$Premium=="Level Premium"))  {Premium= level_premium_PE(age_X,InterestRate,term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Endownment")&(input$Premium=="Single Premium")){Premium= single_premium_E(age_X,InterestRate,term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Endownment")&(input$Premium=="Level Premium"))  {Premium= level_premium_E(age_X,InterestRate,term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Term Assurance")&(input$Premium=="Single Premium")) {Premium= single_premium_TA_Tx(age_X,InterestRate,term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Term Assurance")&(input$Premium=="Level Premium")) {Premium= level_premium_TA_Tx(age_X,InterestRate,term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Pure Endownment")&(input$Premium=="Single Premium")) {Premium= single_premium_PE_Tx(age_X,InterestRate,term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Pure Endownment")&(input$Premium=="Level Premium")){Premium= level_premium_PE_Tx(age_X,InterestRate,term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Endownment")&(input$Premium=="Single Premium")) {Premium= single_premium_E_Tx(age_X,InterestRate,term,initial_exp,claim_exp,AssuredSum) } 
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Endownment")&(input$Premium=="Level Premium")) {Premium= level_premium_E_Tx(age_X,InterestRate,term,initial_exp,premium_exp,claim_exp,AssuredSum) } 
    
    
    
    
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="End of year of death") & (input$Assurance=="Term Assurance") & (input$Premium=="Single Premium")) {Premium= single_premium_TA_y(age_Y,InterestRate,term,initial_exp,claim_exp,AssuredSum)}
    if (  (input$PolicyHolder=="Y") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Term Assurance")&(input$Premium=="Level Premium") ) {Premium= level_premium_TA_y(age_Y,InterestRate,term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if (   (input$PolicyHolder=="Y") &    (input$BenefitPayment=="End of year of death") &(input$Assurance=="Pure Endownment")&(input$Premium=="Single Premium")) {Premium= single_premium_PE_y(age_Y,InterestRate,term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Pure Endownment")&(input$Premium=="Level Premium"))  {Premium= level_premium_PE_y(age_Y,InterestRate,term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Endownment")&(input$Premium=="Single Premium")){Premium= single_premium_E_y(age_Y,InterestRate,term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Endownment")&(input$Premium=="Level Premium"))  {Premium= level_premium_E_y(age_Y,InterestRate,term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Term Assurance")&(input$Premium=="Single Premium")) {Premium= single_premium_TA_Tx_y(age_Y,InterestRate,term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Term Assurance")&(input$Premium=="Level Premium")) {Premium= level_premium_TA_Tx_y(age_Y,InterestRate,term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Pure Endownment")&(input$Premium=="Single Premium")) {Premium= single_premium_PE_Tx_y(age_Y,InterestRate,term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Pure Endownment")&(input$Premium=="Level Premium")){Premium= level_premium_PE_Tx_y(age_Y,InterestRate,term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Endownment")&(input$Premium=="Single Premium")) {Premium= single_premium_E_Tx_y(age_Y,InterestRate,term,initial_exp,claim_exp,AssuredSum) } 
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Endownment")&(input$Premium=="Level Premium")) {Premium= level_premium_E_Tx_y(age_Y,InterestRate,term,initial_exp,premium_exp,claim_exp,AssuredSum) } 
    
    if((input$PolicyHolder=="XY")){Premium=single_premium_for_joint(age_X,age_Y,InterestRate,term,initial_exp,claim_exp,annuity_benefit) }
    
    c=Premium
    print(c)
  }
  )
  
  
  output$Plot <- renderPlot({
    
    age_X = input$Age
    age_Y = input$Age
    AssuredSum=input$AssuredSum
    term=input$term
    InterestRate = input$IR/100
    claim_exp= input$claim_exp/100
    initial_exp=input$initial_exp/100
    premium_exp=input$premium_exp/100
    InterestRates = seq(0,15,length.out=50)/100
    Premiums=c()
    for(i in 1:length(InterestRates)){
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="End of year of death") & (input$Assurance=="Term Assurance") & (input$Premium=="Single Premium")) {Premiums[i]= single_premium_TA(age_X,InterestRates[i],term,initial_exp,claim_exp,AssuredSum)}
    if (  (input$PolicyHolder=="X") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Term Assurance")&(input$Premium=="Level Premium") ) {Premiums[i]= level_premium_TA(age_X,InterestRates[i],term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if (   (input$PolicyHolder=="X") &    (input$BenefitPayment=="End of year of death") &(input$Assurance=="Pure Endownment")&(input$Premium=="Single Premium")) {Premiums[i]= single_premium_PE(age_X,InterestRates[i],term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Pure Endownment")&(input$Premium=="Level Premium"))  {Premiums[i]= level_premium_PE(age_X,InterestRates[i],term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Endownment")&(input$Premium=="Single Premium")){Premiums[i]= single_premium_E(age_X,InterestRates[i],term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Endownment")&(input$Premium=="Level Premium"))  {Premiums[i]= level_premium_E(age_X,InterestRates[i],term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Term Assurance")&(input$Premium=="Single Premium")) {Premiums[i]= single_premium_TA_Tx(age_X,InterestRates[i],term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Term Assurance")&(input$Premium=="Level Premium")) {Premiums[i]= level_premium_TA_Tx(age_X,InterestRates[i],term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Pure Endownment")&(input$Premium=="Single Premium")) {Premiums[i]= single_premium_PE_Tx(age_X,InterestRates[i],term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Pure Endownment")&(input$Premium=="Level Premium")){Premiums[i]= level_premium_PE_Tx(age_X,InterestRates[i],term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Endownment")&(input$Premium=="Single Premium")) {Premiums[i]= single_premium_E_Tx(age_X,InterestRates[i],term,initial_exp,claim_exp,AssuredSum) } 
    if ( (input$PolicyHolder=="X") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Endownment")&(input$Premium=="Level Premium")) {Premiums[i]= level_premium_E_Tx(age_X,InterestRates[i],term,initial_exp,premium_exp,claim_exp,AssuredSum) } 
    
    
    
    
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="End of year of death") & (input$Assurance=="Term Assurance") & (input$Premium=="Single Premium")) {Premiums[i]= single_premium_TA_y(age_Y,InterestRates[i],term,initial_exp,claim_exp,AssuredSum)}
    if (  (input$PolicyHolder=="Y") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Term Assurance")&(input$Premium=="Level Premium") ) {Premiums[i]= level_premium_TA_y(age_Y,InterestRates[i],term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if (   (input$PolicyHolder=="Y") &    (input$BenefitPayment=="End of year of death") &(input$Assurance=="Pure Endownment")&(input$Premium=="Single Premium")) {Premiums[i]= single_premium_PE_y(age_Y,InterestRates[i],term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Pure Endownment")&(input$Premium=="Level Premium"))  {Premiums[i]= level_premium_PE_y(age_Y,InterestRates[i],term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Endownment")&(input$Premium=="Single Premium")){Premiums[i]= single_premium_E_y(age_Y,InterestRates[i],term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="End of year of death") &(input$Assurance=="Endownment")&(input$Premium=="Level Premium"))  {Premiums[i]= level_premium_E_y(age_Y,InterestRates[i],term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Term Assurance")&(input$Premium=="Single Premium")) {Premiums[i]= single_premium_TA_Tx_y(age_Y,InterestRates[i],term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Term Assurance")&(input$Premium=="Level Premium")) {Premiums[i]= level_premium_TA_Tx_y(age_Y,InterestRates[i],term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Pure Endownment")&(input$Premium=="Single Premium")) {Premiums[i]= single_premium_PE_Tx_y(age_Y,InterestRates[i],term,initial_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Pure Endownment")&(input$Premium=="Level Premium")){Premiums[i]= level_premium_PE_Tx_y(age_Y,InterestRates[i],term,initial_exp,premium_exp,claim_exp,AssuredSum) }
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Endownment")&(input$Premium=="Single Premium")) {Premiums[i]= single_premium_E_Tx_y(age_Y,InterestRates[i],term,initial_exp,claim_exp,AssuredSum) } 
    if ( (input$PolicyHolder=="Y") &(input$BenefitPayment=="Immediately on death")&(input$Assurance=="Endownment")&(input$Premium=="Level Premium")) {Premiums[i]= level_premium_E_Tx_y(age_Y,InterestRates[i],term,initial_exp,premium_exp,claim_exp,AssuredSum) } 
    
    if((input$PolicyHolder=="XY")){Premiums[i]=single_premium_for_joint(age_X,age_Y,InterestRates[i],term,initial_exp,claim_exp,annuity_benefit) }
    }
    
    
    DataF = data.frame(InterestRates[i],Premiums[i])
    Theme <- theme(plot.title = element_text( face = "bold", size = (20),hjust = 0.5),
                   legend.title = element_text(face = "bold.italic", family = "Helvetica"),
                   axis.title = element_text( size = (15)),
                   axis.text = element_text(size = (15)),
                   text = element_text(size=20))
    Outputplot = ggplot(DataF, aes(InterestRates,Premium)) + geom_line() + labs(y = " ",x="Effective interest rate") + mynamestheme
    
    print(OutputPlot)
    
    
  }
  )
}


shinyApp(ui = ui , server = server)






















































