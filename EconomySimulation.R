Population <- 100
#An input of any sort, could be data, could be labour. Per cycle
Input <- 8

#Define the starting allocation:
Capital = rchisq(100,df=1)
Labour = rchisq(100,df=1)
#This gives 0 values, but we want values from 1 to 5
Alpha = rchisq(100,df=1)
Beta = rchisq(100,df=1)
HarbergerTaxRate = 0.05
Maxiter = 5

start <- data.frame("ID"= 1:Population,
            Capital,
           Labour,
           Alpha,
           Beta,
           "Output" = Capital^Alpha*Labour^Beta
           )

#Second-Highest function: needed for Vickrey Auction
max2 <- function(x){
  n <- length(x)
  sort(x,partial=n-1)[n-1]
}

#Start a Vector to store the welfare optimization
Totalwelfare <- c(sum(start$Output))
Counter <- 0

while (Counter <= Maxiter) {
  
  #Check if the individual can optimize his output
  for (Buyer in 1:nrow(start)){
    CapitalValue <- start$Alpha[Buyer]
    LabourValue <- start$Beta[Buyer]
    #Vickrey Auction: get the second highest value, if there is only one company with higher, price paid is other's price
    #Check if there are any other prices in range
    if (CapitalValue == min(start$Alpha)){
      CapitalPrice <- CapitalValue
    } else {
      CapitalPrice <- max2(start[which(Alpha <= CapitalValue),"Alpha"])
    }
    
    if (LabourValue <- min(start$Beta)){
      LabourPrice <- LabourValue
    } else {
      LabourPrice <-  max2(start[which(Beta <= LabourValue),"Beta"])
    }
    
    for (Input in c("Capital","Labour")){
      
      
      if (Input=="Capital"){
        SubstitutionRate <- "Alpha"
        Inputprice <- CapitalPrice
        SubstitutionValue <- start$Alpha[Buyer]
      } else {
        SubstitutionRate <- "Beta"
        Inputprice <- LabourPrice
        SubstitutionValue <- start$Beta[Buyer]
      }
      #when the agent has the lowest valuation, there is no point in making transactions because they will not be able
      #to get a cheaper price
      if (SubstitutionValue == min(start[[SubstitutionRate]])) next
      if (nrow(subset(start, Input < SubstitutionValue & Input > 0))) next #if there is no one to buy from we jump
      
      IdealInputRatio <- SubstitutionValue/(start$Alpha[Buyer]+start$Beta[Buyer]) #Given by Cobb Douglas,
      
      #Optimal input amount, given by Cobb-Douglas
      OptimalInputAmount <- (start[[Input]][Buyer]*IdealInputRatio)/Inputprice
      
      #If optimal amount is reached, no action, else buy until reach (or if there is no more on market)
      while (start[[Input]][Buyer] < OptimalInputAmount & #converge to optimal amount
             sum(subset(start, ID != Buyer, select = Input)) > 0 & #there must be something in the market
             start$Capital[Buyer] > 0 #also the agent must be able to buy it.
      ) {
        
        #Randomly select an individual to buy from, individual must have the resource
        Seller <- start[sample(nrow(subset(start, Input < SubstitutionValue & Input > 0)),1),"ID"]
        
        if (length(Seller)==0) break
        
        RequestedAmount <- OptimalInputAmount - start[[Input]][Buyer]
        AffordableAmount <- start$Capital[Buyer]/Inputprice
        AvailableAmount <- start[[Input]][Seller]
        
        #Make the transaction, update the dataframe
        #Two scenarios: buyer has enough cash to buy everything, if not, only buys what he can afford
        if (AffordableAmount > AvailableAmount){
          #The buyer buys everything
          TransactionFee <- Inputprice*AvailableAmount
          
          start$Capital[Buyer] <- start$Capital[Buyer] - TransactionFee #Update Buyers
          start[[Input]][Buyer] <- start[[Input]][Buyer] + AvailableAmount
          start$Capital[Seller] <- start$Capital[Seller] + TransactionFee #Update Seller
          start[[Input]][Seller] <- start[[Input]][Seller] - AvailableAmount
          
        } else {#if the buyer does not have enough cash to buy everything
          
          TransactionFee <- Inputprice*AffordableAmount
          
          start$Capital[Buyer] <- start$Capital[Buyer] - TransactionFee #Update Buyers
          start[[Input]][Buyer] <- start[[Input]][Buyer] + AvailableAmount
          start$Capital[Seller] <- start$Capital[Seller] + TransactionFee #Update Seller
          start[[Input]][Seller] <- start[[Input]][Seller] - AvailableAmount
          
        }
        
      }
    }
    
    #Update the dataframe
    start$TaxCapital <- start$Capital*HarbergerTaxRate
    start$TaxLabour <- start$Labour*HarbergerTaxRate
    start$Capital <- start$Capital - start$TaxCapital
    start$Labour <- start$Labour - start$TaxLabour
    
    #After making all the transactions, the state collects tax and redistributes it equally
    RedistributedCapital <- sum(start$TaxCapital)/Population
    RedistributedLabour <- sum(start$TaxLabour)/Population
    
    start$Capital <- start$Capital + RedistributedCapital
    start$Labour <- start$Labour + RedistributedLabour
    
    #Fill in the measurement Vector
    start$Output <- start$Capital^start$Alpha*start$Labour^start$Beta
    Totalwelfare <- c(Totalwelfare, sum(start$Output))
  }
  Counter <- Counter + 1
}
plot(Totalwelfare)