# app.R

# -----------------------------
# Data (l_x) and table slicing
# -----------------------------
Lxs <- c(10000.0000,9994.0000,9988.0636,9982.2006,9976.3909,9970.6346,9964.9313,9959.2613,9953.6144,9947.9807,9942.3402,9936.6730,9930.9694,9925.2094,9919.3535,9913.3821,9907.2655,9900.9645,9894.4299,9887.6126,9880.4540,9872.8954,9864.8688,9856.2863,9847.0510,9837.0661,9826.2060,9814.3359,9801.3123,9786.9534,
         9771.0789,9753.4714,9733.8865,9712.0728,9687.7149,9660.5021,9630.0522,9595.9715,9557.8179,9515.1040,9467.2906,9413.8004,9354.0040,9287.2164,9212.7143,9129.7170,9037.3973,8934.8771,8821.2612,8695.6199,8557.0118,8404.4916,8237.1329,8054.0544,7854.4508,7637.6208,7403.0084,7150.2401,6879.1673,6589.9258,
         6282.9803,5959.1680,5619.7577,5266.4604,4901.4789,4527.4960,4147.6708,3765.5998,3385.2479,3010.8395,2646.7416,2297.2976,1966.6499,1658.5545,1376.1906,1121.9889,897.5025,703.3242,539.0643,403.4023,294.2061,208.7060,143.7120,95.8476,61.7733,38.3796,22.9284,13.1359,7.1968,3.7596,1.8669,0.8784,0.3903,0.1632,0.0640,0.0234,
         0.0080,0.0025,0.0007,0.0002,0.0000,0.0000,0.0000,0.0000)

# We want ages 20..90 (71 values). Assuming Lxs[1] = l_0 (age 0):
base_age <- 20
max_age  <- 90
Lxs_x <- Lxs[(base_age+1):(max_age+1)]
Lxs_y <- Lxs[(base_age+1):(max_age+1)]
idx <- function(age) age - base_age + 1
stopifnot(length(Lxs_x) == 71, length(Lxs_y) == 71)

# -----------------------------
# Core actuarial functions (your originals with minimal fixes)
# -----------------------------

## whole life annuity due (advance) on x-table
Wholelifeannuity <- function(age, interestrate){
  v <- 1/(1 + interestrate)
  pos <- idx(age)
  stopifnot(pos >= 1, pos <= length(Lxs_x))
  T <- length(Lxs_x) - pos
  if (T <= 0) return(1)  # only payment at time 0
  survivalprobabilities <- (Lxs_x[(pos+1):(pos+T)] / Lxs_x[pos])
  alldiscountrates <- v^(1:T)
  Output <- sum(c(1, (alldiscountrates * survivalprobabilities)))
  return(Output)
}

## whole life annuity immediate (arrears) on x-table
Wholelifeannuity_arrears <- function(age, interestrate){
  Wholelifeannuity(age, interestrate) - 1
}

## term annuity due (advance) on x-table
Termannuitydue <- function(age, interestrate, term){
  if (term <= 0) return(0)
  pos <- idx(age)
  stopifnot(pos + term <= length(Lxs_x))
  discountfactor <- (Lxs_x[pos + term]/Lxs_x[pos]) * ((1/(1 + interestrate))^term)
  Output <- Wholelifeannuity(age, interestrate) - discountfactor*Wholelifeannuity(age+term, interestrate)
  return(Output)
}

## Assurances (x-table)

## 1) Whole life assurance paid at the end of the year
WholelifeAssurance <- function(age, interestrate){
  1 - ((interestrate/(1+interestrate)) * Wholelifeannuity(age, interestrate))
}

## 2) Whole life assurance paid immediately on death (Woolhouse approx)
WholelifeAssurance_Tx <- function(age, interestrate){
  ((1+interestrate)^0.5) * WholelifeAssurance(age, interestrate)
}

## term assurance EoY (x-table)
Termassurance <- function(age, interestrate, term){
  if (term <= 0) return(0)
  pos <- idx(age)
  stopifnot(pos + term <= length(Lxs_x))
  discountfactor <- (Lxs_x[pos + term]/Lxs_x[pos]) * ((1/(1 + interestrate))^term)
  WholelifeAssurance(age, interestrate) - discountfactor * WholelifeAssurance(age + term, interestrate)
}

## term assurance payable immediately (approx)
Termassurance_Tx <- function(age, interestrate, term){
  ((1+interestrate)^0.5) * Termassurance(age, interestrate, term)
}

## Pure Endowment (x-table)
PureEndownment <- function(age, interestrate, term){
  if (term <= 0) return(0)
  pos <- idx(age)
  stopifnot(pos + term <= length(Lxs_x))
  (Lxs_x[pos + term]/Lxs_x[pos]) * ((1/(1 + interestrate))^term)
}

## Pure Endowment immediate on death (naming kept; same approx factor)
PureEndownment_Tx <- function(age, interestrate, term){
  ((1+interestrate)^0.5) * PureEndownment(age, interestrate, term)
}

## Endowment (EoY) (x-table)
Endownment <- function(age, interestrate, term){
  1 - ((interestrate/(1+interestrate)) * Termannuitydue(age, interestrate, term))
}

## Endowment immediate on death (approx)
Endownment_Tx <- function(age, interestrate, term){
  Termassurance_Tx(age, interestrate, term) + PureEndownment_Tx(age, interestrate, term)
}

## Premiums for x-table

single_premium_TA <- function(age, interestrate, term, initial_exp, claim_exp, SA){
  a <- ((SA * Termassurance(age, interestrate, term)) * (1+claim_exp))
  b <- (1 - initial_exp)
  a/b
}

level_premium_TA <- function(age, interestrate, term, initial_exp, premium_exp, claim_exp, SA){
  a <- ((SA * Termassurance(age, interestrate, term)) * (1+claim_exp))
  b <- Termannuitydue(age, interestrate, term) - initial_exp - (premium_exp * (Termannuitydue(age, interestrate, term) - 1))
  a/b
}

single_premium_PE <- function(age, interestrate, term, initial_exp, claim_exp, SA){
  a <- ((SA * PureEndownment(age, interestrate, term)) * (1+claim_exp))
  b <- (1 - initial_exp)
  a/b
}

level_premium_PE <- function(age, interestrate, term, initial_exp, premium_exp, claim_exp, SA){
  a <- ((SA * PureEndownment(age, interestrate, term)) * (1+claim_exp))
  b <- Termannuitydue(age, interestrate, term) - initial_exp - (premium_exp * (Termannuitydue(age, interestrate, term) - 1))
  a/b
}

single_premium_E <- function(age, interestrate, term, initial_exp, claim_exp, SA){
  a <- ((SA * Endownment(age, interestrate, term)) * (1+claim_exp))
  b <- (1 - initial_exp)
  a/b
}

level_premium_E <- function(age, interestrate, term, initial_exp, premium_exp, claim_exp, SA){
  a <- ((SA * Endownment(age, interestrate, term)) * (1+claim_exp))
  b <- Termannuitydue(age, interestrate, term) - initial_exp - (premium_exp * (Termannuitydue(age, interestrate, term) - 1))
  a/b
}

## ---- y-table duplicates (kept your structure, bound to Lxs_y) ----

Wholelifeannuity_y <- function(age, interestrate){
  v <- 1/(1 + interestrate)
  pos <- idx(age)
  stopifnot(pos >= 1, pos <= length(Lxs_y))
  T <- length(Lxs_y) - pos
  if (T <= 0) return(1)
  survivalprobabilities <- (Lxs_y[(pos+1):(pos+T)] / Lxs_y[pos])
  alldiscountrates <- v^(1:T)
  sum(c(1, (alldiscountrates * survivalprobabilities)))
}

Wholelifeannuity_arrears_y <- function(age, interestrate){
  Wholelifeannuity_y(age, interestrate) - 1
}

Termannuitydue_y <- function(age, interestrate, term){
  if (term <= 0) return(0)
  pos <- idx(age)
  stopifnot(pos + term <= length(Lxs_y))
  discountfactor <- (Lxs_y[pos + term]/Lxs_y[pos]) * ((1/(1 + interestrate))^term)
  Wholelifeannuity_y(age, interestrate) - discountfactor*Wholelifeannuity_y(age+term, interestrate)
}

WholelifeAssurance_y <- function(age, interestrate){
  1 - ((interestrate/(1+interestrate)) * Wholelifeannuity_y(age, interestrate))
}

WholelifeAssurance_Tx_y <- function(age, interestrate){
  ((1+interestrate)^0.5) * WholelifeAssurance_y(age, interestrate)
}

Termassurance_y <- function(age, interestrate, term){
  if (term <= 0) return(0)
  pos <- idx(age)
  stopifnot(pos + term <= length(Lxs_y))
  discountfactor <- (Lxs_y[pos + term]/Lxs_y[pos]) * ((1/(1 + interestrate))^term)
  WholelifeAssurance_y(age, interestrate) - discountfactor*WholelifeAssurance_y(age+term, interestrate)
}

Termassurance_Tx_y <- function(age, interestrate, term){
  ((1+interestrate)^0.5) * Termassurance_y(age, interestrate, term)
}

PureEndownment_y <- function(age, interestrate, term){
  if (term <= 0) return(0)
  pos <- idx(age)
  stopifnot(pos + term <= length(Lxs_y))
  (Lxs_y[pos + term]/Lxs_y[pos]) * ((1/(1 + interestrate))^term)
}

PureEndownment_Tx_y <- function(age, interestrate, term){
  ((1+interestrate)^0.5) * PureEndownment_y(age, interestrate, term)
}

Endownment_y <- function(age, interestrate, term){
  1 - ((interestrate/(1+interestrate)) * Termannuitydue_y(age, interestrate, term))
}

Endownment_Tx_y <- function(age, interestrate, term){
  Termassurance_Tx_y(age, interestrate, term) + PureEndownment_Tx_y(age, interestrate, term)
}

single_premium_TA_y <- function(age, interestrate, term, initial_exp, claim_exp, SA){
  a <- ((SA * Termassurance_y(age, interestrate, term)) * (1+claim_exp))
  b <- (1 - initial_exp)
  a/b
}

level_premium_TA_y <- function(age, interestrate, term, initial_exp, premium_exp, claim_exp, SA){
  a <- ((SA * Termassurance_y(age, interestrate, term)) * (1+claim_exp))
  b <- Termannuitydue_y(age, interestrate, term) - initial_exp - (premium_exp * (Termannuitydue_y(age, interestrate, term) - 1))
  a/b
}

single_premium_PE_y <- function(age, interestrate, term, initial_exp, claim_exp, SA){
  a <- ((SA * PureEndownment_y(age, interestrate, term)) * (1+claim_exp))
  b <- (1 - initial_exp)
  a/b
}

level_premium_PE_y <- function(age, interestrate, term, initial_exp, premium_exp, claim_exp, SA){
  a <- ((SA * PureEndownment_y(age, interestrate, term)) * (1+claim_exp))
  b <- Termannuitydue_y(age, interestrate, term) - initial_exp - (premium_exp * (Termannuitydue_y(age, interestrate, term) - 1))
  a/b
}

single_premium_E_y <- function(age, interestrate, term, initial_exp, claim_exp, SA){
  a <- ((SA * Endownment_y(age, interestrate, term)) * (1+claim_exp))
  b <- (1 - initial_exp)
  a/b
}

level_premium_E_y <- function(age, interestrate, term, initial_exp, premium_exp, claim_exp, SA){
  a <- ((SA * Endownment_y(age, interestrate, term)) * (1+claim_exp))
  b <- Termannuitydue_y(age, interestrate, term) - initial_exp - (premium_exp * (Termannuitydue_y(age, interestrate, term) - 1))
  a/b
}

single_premium_TA_Tx_y <- function(age, interestrate, term, initial_exp, claim_exp, SA){
  a <- ((SA * Termassurance_Tx_y(age, interestrate, term)) * (1+claim_exp))
  b <- (1 - initial_exp)
  a/b
}

level_premium_TA_Tx_y <- function(age, interestrate, term, initial_exp, premium_exp, claim_exp, SA){
  a <- ((SA * Termassurance_Tx_y(age, interestrate, term)) * (1+claim_exp))
  b <- Termannuitydue_y(age, interestrate, term) - initial_exp - (premium_exp * (Termannuitydue_y(age, interestrate, term) - 1))
  a/b
}

single_premium_PE_Tx_y <- function(age, interestrate, term, initial_exp, claim_exp, SA){
  a <- ((SA * PureEndownment_Tx_y(age, interestrate, term)) * (1+claim_exp))
  b <- (1 - initial_exp)
  a/b
}

level_premium_PE_Tx_y <- function(age, interestrate, term, initial_exp, premium_exp, claim_exp, SA){
  a <- ((SA * PureEndownment_Tx_y(age, interestrate, term)) * (1+claim_exp))
  b <- Termannuitydue_y(age, interestrate, term) - initial_exp - (premium_exp * (Termannuitydue_y(age, interestrate, term) - 1))
  a/b
}

single_premium_E_Tx_y <- function(age, interestrate, term, initial_exp, claim_exp, SA){
  a <- ((SA * Endownment_Tx_y(age, interestrate, term)) * (1+claim_exp))
  b <- (1 - initial_exp)
  a/b
}

level_premium_E_Tx_y <- function(age, interestrate, term, initial_exp, premium_exp, claim_exp, SA){
  a <- ((SA * Endownment_Tx_y(age, interestrate, term)) * (1+claim_exp))
  b <- Termannuitydue_y(age, interestrate, term) - initial_exp - (premium_exp * (Termannuitydue_y(age, interestrate, term) - 1))
  a/b
}

# -----------------------------
# Joint-life (kept structure; minimal fixes so it runs)
# -----------------------------

J_WholeAnnuityDue <- function(age_x, age_y, interestrate){
  posx <- idx(age_x); posy <- idx(age_y)
  v <- 1/(1+interestrate)
  Tx <- length(Lxs_x) - posx
  Ty <- length(Lxs_y) - posy
  T  <- min(Tx, Ty)
  if (T <= 0) return(1)
  survivialprobability_x <- (Lxs_x[(posx+1):(posx+T)] / Lxs_x[posx])
  survivialprobability_y <- (Lxs_y[(posy+1):(posy+T)] / Lxs_y[posy])
  p <- v^(1:T)
  sum(c(1, (p * survivialprobability_x * survivialprobability_y)))
}

J_TermAnnuityDue <- function(age_x, age_y, interestrate, term){
  if (term <= 0) return(0)
  posx <- idx(age_x); posy <- idx(age_y)
  stopifnot(posx + term <= length(Lxs_x),
            posy + term <= length(Lxs_y))
  discount <- (Lxs_x[posx+term]*Lxs_y[posy+term]) / (Lxs_x[posx] * Lxs_y[posy]) * ((1/(1+interestrate))^term)
  J_WholeAnnuityDue(age_x, age_y, interestrate) - (discount * J_WholeAnnuityDue(age_x+term, age_y+term, interestrate))
}

rrr <- function(age_x, age_y, interestrate){
  Wholelifeannuity(age_x, interestrate) + Wholelifeannuity_y(age_y, interestrate) - J_WholeAnnuityDue(age_x, age_y, interestrate)
}

# The following three were adjusted only to fix vector-length issues; logic structure kept
we <- function(age_x, age_y, interestrate, term){
  if (term <= 0) return(0)
  posx <- idx(age_x); posy <- idx(age_y)
  stopifnot(posx + term <= length(Lxs_x),
            posy + term <= length(Lxs_y))
  survivialprobability_x <- (Lxs_x[(posx+1):(posx+term)] / Lxs_x[posx])
  deathprobability_y     <- 1 - (Lxs_y[(posy+1):(posy+term)] / Lxs_y[posy])
  v <- 1/(1+interestrate)
  p <- v^(1:term)
  sum(p * survivialprobability_x * deathprobability_y * Wholelifeannuity(age_x+term, interestrate))
}

be <- function(age_x, age_y, interestrate, term){
  if (term <= 0) return(0)
  posx <- idx(age_x); posy <- idx(age_y)
  stopifnot(posx + term <= length(Lxs_x),
            posy + term <= length(Lxs_y))
  deathprobability_x     <- 1 - (Lxs_x[(posx+1):(posx+term)] / Lxs_x[posx])
  survivalprobability_y  <- (Lxs_y[(posy+1):(posy+term)] / Lxs_y[posy])
  v <- 1/(1+interestrate)
  p <- v^(1:term)
  sum(p * deathprobability_x * survivalprobability_y * Wholelifeannuity_y(age_y+term, interestrate))
}

tree <- function(age_x, age_y, interestrate, term){
  if (term <= 0) return(0)
  posx <- idx(age_x); posy <- idx(age_y)
  stopifnot(posx + term <= length(Lxs_x),
            posy + term <= length(Lxs_y))
  survivalprobability_x <- (Lxs_x[(posx+1):(posx+term)] / Lxs_x[posx])
  survivalprobability_y <- (Lxs_y[(posy+1):(posy+term)] / Lxs_y[posy])
  v <- 1/(1+interestrate)
  p <- v^(1:term)
  sum(p * survivalprobability_x * survivalprobability_y * rrr(age_x+term, age_y+term, interestrate))
}

J_TermLastSurvivalContinuos <- function(age_x, age_y, interestrate, term){
  rrr(age_x, age_y, interestrate) - we(age_x, age_y, interestrate, term) - be(age_x, age_y, interestrate, term) - tree(age_x, age_y, interestrate, term) - 0.5
}

J_TermAnnuityContinuos <- function(age_x, age_y, interestrate, term){
  J_TermAnnuityDue(age_x, age_y, interestrate, term) - 0.5
}

TermannuityContinous_x <- function(age, interestrate, term){
  Termannuitydue(age, interestrate, term) - 0.5  # fixed: use x-table
}

Annuity_payable_to_x <- function(age_x, age_y, interestrate, term){
  -( J_TermLastSurvivalContinuos(age_x, age_y, interestrate, term)
     + J_TermAnnuityContinuos(age_x, age_y, interestrate, term)
     - TermannuityContinous_x(age_y, interestrate, term) )
}

single_premium_for_joint <- function(age_x, age_y, interestrate, term, initial_exp, claim_exp, annuity_benefit){
  prem <- (annuity_benefit * (1+claim_exp) * Annuity_payable_to_x(age_x, age_y, interestrate, term)) / (1 - initial_exp)
  prem
}

# -----------------------------
# Shiny App
# -----------------------------
library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Insurance Benefit Valuation Tool"),
  sidebarPanel(
    radioButtons("PolicyHolder", label = "Policy", choices = list("X","Y","XY"), selected = "X"),
    sliderInput("Age_X", "Age of X", value = 30, min = 20, max = 90),
    sliderInput("Age_Y", "Age of Y", value = 30, min = 20, max = 90),
    numericInput("IR", "Interest Rate (in %)", value = 4, min = 0, max = 15),
    radioButtons("BenefitPayment","Insurance Benefit payment", choices = list("End of year of death","Immediately on death"), selected = "End of year of death"),
    radioButtons("Assurance","Insurance Benefit", choices = list("Term Assurance","Pure Endownment","Endownment"), selected = "Pure Endownment"),
    radioButtons("Premium","Premium Payment", choices = list("Single Premium","Level Premium"), selected = "Single Premium"),
    numericInput("AssuredSum","Assured Sum", value = 1, min = 0, max = 1e8),
    numericInput("annuity_benefit","Annuity Benefit", value = 1, min = 0, max = 1e8),
    numericInput("term","Term", value = 4, min = 0, max = 1e5),
    numericInput("claim_exp","Claim Expense (% of Benefit Amount)", value = 1, min = 0, max = 50),
    numericInput("premium_exp","Premium Expense (%) (ignored for single premium)", value = 5, min = 0, max = 50),
    numericInput("initial_exp","Initial Expense (%)", value = 5, min = 0, max = 100)
  ),
  mainPanel(
    h4("Calculated Premium / Value"),
    verbatimTextOutput("OutputText"),
    h4("Sensitivity: Premium vs Interest Rate"),
    plotOutput("Plot", height = "380px")
  )
)

server <- function(input, output) {
  
  output$OutputText <- renderPrint({
    age_X <- input$Age_X
    age_Y <- input$Age_Y
    AssuredSum <- input$AssuredSum
    annuity_benefit <- input$annuity_benefit
    term <- input$term
    InterestRate <- input$IR/100
    claim_exp <- input$claim_exp/100
    initial_exp <- input$initial_exp/100
    premium_exp <- input$premium_exp/100
    
    # one and only one branch should set Premium
    Premium <- NA_real_
    
    if (input$PolicyHolder=="X" && input$BenefitPayment=="End of year of death" && input$Assurance=="Term Assurance" && input$Premium=="Single Premium") {
      Premium <- single_premium_TA(age_X, InterestRate, term, initial_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="X" && input$BenefitPayment=="End of year of death" && input$Assurance=="Term Assurance" && input$Premium=="Level Premium") {
      Premium <- level_premium_TA(age_X, InterestRate, term, initial_exp, premium_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="X" && input$BenefitPayment=="End of year of death" && input$Assurance=="Pure Endownment" && input$Premium=="Single Premium") {
      Premium <- single_premium_PE(age_X, InterestRate, term, initial_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="X" && input$BenefitPayment=="End of year of death" && input$Assurance=="Pure Endownment" && input$Premium=="Level Premium") {
      Premium <- level_premium_PE(age_X, InterestRate, term, initial_exp, premium_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="X" && input$BenefitPayment=="End of year of death" && input$Assurance=="Endownment" && input$Premium=="Single Premium") {
      Premium <- single_premium_E(age_X, InterestRate, term, initial_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="X" && input$BenefitPayment=="End of year of death" && input$Assurance=="Endownment" && input$Premium=="Level Premium") {
      Premium <- level_premium_E(age_X, InterestRate, term, initial_exp, premium_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="X" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Term Assurance" && input$Premium=="Single Premium") {
      Premium <- single_premium_TA_Tx(age_X, InterestRate, term, initial_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="X" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Term Assurance" && input$Premium=="Level Premium") {
      Premium <- level_premium_TA_Tx(age_X, InterestRate, term, initial_exp, premium_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="X" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Pure Endownment" && input$Premium=="Single Premium") {
      Premium <- single_premium_PE_Tx(age_X, InterestRate, term, initial_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="X" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Pure Endownment" && input$Premium=="Level Premium") {
      Premium <- level_premium_PE_Tx(age_X, InterestRate, term, initial_exp, premium_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="X" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Endownment" && input$Premium=="Single Premium") {
      Premium <- single_premium_E_Tx(age_X, InterestRate, term, initial_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="X" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Endownment" && input$Premium=="Level Premium") {
      Premium <- level_premium_E_Tx(age_X, InterestRate, term, initial_exp, premium_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="End of year of death" && input$Assurance=="Term Assurance" && input$Premium=="Single Premium") {
      Premium <- single_premium_TA_y(age_Y, InterestRate, term, initial_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="End of year of death" && input$Assurance=="Term Assurance" && input$Premium=="Level Premium") {
      Premium <- level_premium_TA_y(age_Y, InterestRate, term, initial_exp, premium_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="End of year of death" && input$Assurance=="Pure Endownment" && input$Premium=="Single Premium") {
      Premium <- single_premium_PE_y(age_Y, InterestRate, term, initial_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="End of year of death" && input$Assurance=="Pure Endownment" && input$Premium=="Level Premium") {
      Premium <- level_premium_PE_y(age_Y, InterestRate, term, initial_exp, premium_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="End of year of death" && input$Assurance=="Endownment" && input$Premium=="Single Premium") {
      Premium <- single_premium_E_y(age_Y, InterestRate, term, initial_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="End of year of death" && input$Assurance=="Endownment" && input$Premium=="Level Premium") {
      Premium <- level_premium_E_y(age_Y, InterestRate, term, initial_exp, premium_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Term Assurance" && input$Premium=="Single Premium") {
      Premium <- single_premium_TA_Tx_y(age_Y, InterestRate, term, initial_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Term Assurance" && input$Premium=="Level Premium") {
      Premium <- level_premium_TA_Tx_y(age_Y, InterestRate, term, initial_exp, premium_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Pure Endownment" && input$Premium=="Single Premium") {
      Premium <- single_premium_PE_Tx_y(age_Y, InterestRate, term, initial_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Pure Endownment" && input$Premium=="Level Premium") {
      Premium <- level_premium_PE_Tx_y(age_Y, InterestRate, term, initial_exp, premium_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Endownment" && input$Premium=="Single Premium") {
      Premium <- single_premium_E_Tx_y(age_Y, InterestRate, term, initial_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Endownment" && input$Premium=="Level Premium") {
      Premium <- level_premium_E_Tx_y(age_Y, InterestRate, term, initial_exp, premium_exp, claim_exp, AssuredSum)
    } else if (input$PolicyHolder=="XY") {
      Premium <- single_premium_for_joint(age_X, age_Y, InterestRate, term, initial_exp, claim_exp, annuity_benefit)
    }
    
    Premium
  })
  
  output$Plot <- renderPlot({
    age_X <- input$Age_X
    age_Y <- input$Age_Y
    AssuredSum <- input$AssuredSum
    annuity_benefit <- input$annuity_benefit
    term <- input$term
    claim_exp <- input$claim_exp/100
    initial_exp <- input$initial_exp/100
    premium_exp <- input$premium_exp/100
    
    InterestRates <- seq(0, 0.15, length.out = 60)
    Premiums <- numeric(length(InterestRates))
    
    for (i in seq_along(InterestRates)) {
      ir <- InterestRates[i]
      if (input$PolicyHolder=="X" && input$BenefitPayment=="End of year of death" && input$Assurance=="Term Assurance" && input$Premium=="Single Premium") {
        Premiums[i] <- single_premium_TA(age_X, ir, term, initial_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="X" && input$BenefitPayment=="End of year of death" && input$Assurance=="Term Assurance" && input$Premium=="Level Premium") {
        Premiums[i] <- level_premium_TA(age_X, ir, term, initial_exp, premium_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="X" && input$BenefitPayment=="End of year of death" && input$Assurance=="Pure Endownment" && input$Premium=="Single Premium") {
        Premiums[i] <- single_premium_PE(age_X, ir, term, initial_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="X" && input$BenefitPayment=="End of year of death" && input$Assurance=="Pure Endownment" && input$Premium=="Level Premium") {
        Premiums[i] <- level_premium_PE(age_X, ir, term, initial_exp, premium_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="X" && input$BenefitPayment=="End of year of death" && input$Assurance=="Endownment" && input$Premium=="Single Premium") {
        Premiums[i] <- single_premium_E(age_X, ir, term, initial_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="X" && input$BenefitPayment=="End of year of death" && input$Assurance=="Endownment" && input$Premium=="Level Premium") {
        Premiums[i] <- level_premium_E(age_X, ir, term, initial_exp, premium_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="X" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Term Assurance" && input$Premium=="Single Premium") {
        Premiums[i] <- single_premium_TA_Tx(age_X, ir, term, initial_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="X" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Term Assurance" && input$Premium=="Level Premium") {
        Premiums[i] <- level_premium_TA_Tx(age_X, ir, term, initial_exp, premium_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="X" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Pure Endownment" && input$Premium=="Single Premium") {
        Premiums[i] <- single_premium_PE_Tx(age_X, ir, term, initial_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="X" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Pure Endownment" && input$Premium=="Level Premium") {
        Premiums[i] <- level_premium_PE_Tx(age_X, ir, term, initial_exp, premium_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="X" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Endownment" && input$Premium=="Single Premium") {
        Premiums[i] <- single_premium_E_Tx(age_X, ir, term, initial_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="X" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Endownment" && input$Premium=="Level Premium") {
        Premiums[i] <- level_premium_E_Tx(age_X, ir, term, initial_exp, premium_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="End of year of death" && input$Assurance=="Term Assurance" && input$Premium=="Single Premium") {
        Premiums[i] <- single_premium_TA_y(age_Y, ir, term, initial_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="End of year of death" && input$Assurance=="Term Assurance" && input$Premium=="Level Premium") {
        Premiums[i] <- level_premium_TA_y(age_Y, ir, term, initial_exp, premium_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="End of year of death" && input$Assurance=="Pure Endownment" && input$Premium=="Single Premium") {
        Premiums[i] <- single_premium_PE_y(age_Y, ir, term, initial_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="End of year of death" && input$Assurance=="Pure Endownment" && input$Premium=="Level Premium") {
        Premiums[i] <- level_premium_PE_y(age_Y, ir, term, initial_exp, premium_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="End of year of death" && input$Assurance=="Endownment" && input$Premium=="Single Premium") {
        Premiums[i] <- single_premium_E_y(age_Y, ir, term, initial_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="End of year of death" && input$Assurance=="Endownment" && input$Premium=="Level Premium") {
        Premiums[i] <- level_premium_E_y(age_Y, ir, term, initial_exp, premium_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Term Assurance" && input$Premium=="Single Premium") {
        Premiums[i] <- single_premium_TA_Tx_y(age_Y, ir, term, initial_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Term Assurance" && input$Premium=="Level Premium") {
        Premiums[i] <- level_premium_TA_Tx_y(age_Y, ir, term, initial_exp, premium_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Pure Endownment" && input$Premium=="Single Premium") {
        Premiums[i] <- single_premium_PE_Tx_y(age_Y, ir, term, initial_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Pure Endownment" && input$Premium=="Level Premium") {
        Premiums[i] <- level_premium_PE_Tx_y(age_Y, ir, term, initial_exp, premium_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Endownment" && input$Premium=="Single Premium") {
        Premiums[i] <- single_premium_E_Tx_y(age_Y, ir, term, initial_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="Y" && input$BenefitPayment=="Immediately on death" && input$Assurance=="Endownment" && input$Premium=="Level Premium") {
        Premiums[i] <- level_premium_E_Tx_y(age_Y, ir, term, initial_exp, premium_exp, claim_exp, AssuredSum)
      } else if (input$PolicyHolder=="XY") {
        Premiums[i] <- single_premium_for_joint(age_X, age_Y, ir, term, initial_exp, claim_exp, annuity_benefit)
      }
    }
    
    DataF <- data.frame(InterestRates = InterestRates, Premium = Premiums)
    ggplot(DataF, aes(InterestRates, Premium)) +
      geom_line(linewidth = 1) +
      labs(y = "Premium / Value", x = "Effective interest rate") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui = ui, server = server)
