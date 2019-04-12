library(Runuran)

Population <- 100
Price <- 1
Wage <- 0.3
Interest <- 0.01

#Define the starting allocation:
set.seed(42)
#We assume that Labour and Capital is not divisible and must be bought/sold in whole units.
Labour = round(urchisq(n = Population, df=20, lb = 0, ub = 100))
Capital = round(urchisq(n = Population, df=20, lb = 0, ub = 100))

#To ensure decreasing return to scale, Alpha+Beta<=1.
#Create empty lists to append
Alpha <- c()
Beta <- c()
for (i in 1:Population) {
    #Create Alpha/Beta Pair
    x <- urweibull(n = 1, shape = 2, scale = 0.5, lb = 0, ub = 1)
    y <- urweibull(n = 1, shape = 2, scale = 0.5, lb = 0, ub = 1)
    #if their sum is equal or above 1, resample
    while (x+y>=1) {
      x <- urweibull(1,2,0.5,0,1)
      y <- urweibull(1,2,0.5,0,1)
    }
    #Append
    Alpha <- c(Alpha,x)
    Beta <- c(Beta,y)
  }

Output = Labour^Alpha*Capital^Beta
#the company's original budget is equal to their first output
Budget = Price*(Labour^Alpha*Capital^Beta)

#A company has an optimal amount of Labour and Capital, given by Calculus:
OptimalLabour = (Alpha/(Alpha+Beta))*(Budget/Wage)
OptimalCapital = (Beta/(Alpha+Beta))*(Budget/Interest)

#Current Value Marginal Product Labour & Capital
VMPL = Price*(Alpha*Labour^(Alpha-1)*Capital^Beta)
VMPC = Price*(Beta*Capital^(Beta-1)*Labour^Alpha)

HarbergerTaxRate = 0.01
Maxiter = 5

start <- data.frame("ID"= 1:Population,
            Labour,
           Capital,
           Alpha,
           Beta,
           Output,
           Budget,
           OptimalLabour,
           OptimalCapital,
           VMPL,
           VMPC
           )

#Second-Highest function: needed for Vickrey Auction
max2 <- function(x){
  n <- length(x)
  if (n<=1){
    Secondmax <- max(x)
  } else {
    Secondmax <- sort(x,partial=n-1)[n-1]
  }
  return(Secondmax)
}

#Alternative Sample function: standard sample will throw an error if length(x)==1
sample.vec <- function(x, size = 1) x[sample(length(x), size = size)]

#Start a Vector to store the welfare optimization
Totalwelfare <- c(sum(start$Budget))

Counter <- 0

while (Counter <= Maxiter) {
  
  #Check if the individual can optimize his output
  for (Buyer in 1:nrow(start)){
    #Value is equal to the Marginal Product at Input X times the price
    LabourValue <- start$VMPL[Buyer]
    CapitalValue <- start$VMPC[Buyer]
    #Vickrey Auction: get the second highest value, if there is only one company with higher, price paid is other's price
    #Check if there are any other prices in range
    if (LabourValue == min(start$VMPL)){
      LabourPrice <- LabourValue
    } else {
      LabourPrice <- max2(start[which(VMPL <= LabourValue),"VMPL"])
    }
    
    if (CapitalValue <- min(start$VMPC)){
      CapitalPrice <- CapitalValue
    } else {
      CapitalPrice <-  max2(start[which(VMPC <= CapitalValue),"VMPC"])
    }
    
    for (Input in c("Labour","Capital")){
      
      if (Input=="Labour"){
        MarginalValue <- "VMPL"
        SubstitutionRate <- "Alpha"
        OtherSubstitutionRate <- "Beta"
        OtherInput <- "Capital"
        Inputprice <- LabourPrice
        SubstitutionValue <- start$VMPL[Buyer]
        OptimalInputAmount <- start$OptimalLabour[Buyer]
      } else {
        MarginalValue <- "VMPC"
        SubstitutionRate <- "Beta"
        OtherSubstitutionRate <- "Alpha"
        OtherInput <- "Labour"
        Inputprice <- CapitalPrice
        SubstitutionValue <- start$VMPC[Buyer]
        OptimalInputAmount <- start$OptimalCapital[Buyer]
      }
      #when the agent has the lowest valuation, there is no point in making transactions because they will not be able
      #to get a cheaper price
      if (SubstitutionValue == min(start[[MarginalValue]])) next
      #if there is no one to buy from we jump. Everyone needs at least 1 unit of each, else whole output 0
      if (nrow(subset(start, start[[MarginalValue]] < SubstitutionValue & start[[Input]] > 1))==0) next
      
      #If optimal amount is reached, no action, else buy until reach (or if there is no more on market)
      while (start[[Input]][Buyer] < OptimalInputAmount & #converge to optimal amount
             sum(subset(start, ID != Buyer, select = Input)) > 1 & #there must be something in the market
             nrow(subset(start, start[[MarginalValue]] < SubstitutionValue & start[[Input]] > 1))>0 &
             start$Budget[Buyer] > 0 #also the agent must be able to buy it.
      ) {

        #Randomly select an individual to buy from, individual must have the resource
        Seller <- sample.vec(subset(start, start[[MarginalValue]] < SubstitutionValue & start[[Input]] > 1)$ID,size = 1)
        
        #Simplified: we only allow transactions of 1 unit

        #Make the transaction, update the dataframe

          start$Budget[Buyer] <- start$Budget[Buyer] - Inputprice #Update Buyers Budget
          start[[Input]][Buyer] <- start[[Input]][Buyer] + 1 #Update Buyers Input
          start[[MarginalValue]][Buyer] <- Price*(start[[SubstitutionRate]][Buyer] #Update Marginal Value
                                                  *start[[Input]][Buyer]^(start[[SubstitutionRate]][Buyer]-1)
                                                  *start[[OtherInput]][Buyer]^start[[OtherSubstitutionRate]][Buyer])
          #Seller
          start$Budget[Seller] <- start$Budget[Seller] + Inputprice #Update Sellers Budget
          start[[Input]][Seller] <- start[[Input]][Seller] - 1 #Update Sellers Input
          start[[MarginalValue]][Seller] <- Price*(start[[SubstitutionRate]][Seller] #Update Marginal Value
                                                  *start[[Input]][Seller]^(start[[SubstitutionRate]][Seller]-1)
                                                  *start[[OtherInput]][Seller]^start[[OtherSubstitutionRate]][Seller])
          
          #Update Value and Price
          SubstitutionValue <- start[[MarginalValue]][Buyer]
          
          if (start[[MarginalValue]][Buyer] == min(start[[MarginalValue]])){
            Inputprice <- start[[MarginalValue]][Buyer]
          } else {
            InputPrice <-  max2(subset(start, start[[MarginalValue]] < SubstitutionValue, select = MarginalValue))
          }
      }
    }
    
    #Update the dataframe
    start$Output <- start$Labour^start$Alpha*start$Capital^start$Beta
    start$Tax <- start$Output*Price*HarbergerTaxRate
    
    #After making all the transactions, the state collects tax and redistributes it equally
    RedistributedTax <- sum(start$Tax)/Population
    #Budget Update: New budget is old budget plus output minus tax
    start$Budget <- start$Output*Price-start$Tax+RedistributedTax + start$Budget
    
    #Firms then readjust their Optimal Production Quantity for the next iteration
    start$OptimalLabour = (start$Alpha/(start$Alpha+start$Beta))*(start$Budget/Wage)
    start$OptimalCapital = (start$Beta/(start$Alpha+start$Beta))*(start$Budget/Interest)
    
    #Current Value Marginal Product Labour & Capital
    start$VMPL = Price*(start$Alpha*start$Labour^(start$Alpha-1)*start$Capital^start$Beta)
    start$VMPC = Price*(start$Beta*start$Capital^(start$Beta-1)*start$Labour^start$Alpha)
    
    #Fill in the measurement Vector
    Totalwelfare <- c(Totalwelfare, sum(start$Output))
  }
  Counter <- Counter + 1
}
plot(Totalwelfare)
