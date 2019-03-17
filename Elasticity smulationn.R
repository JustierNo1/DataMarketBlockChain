install.packages("Runuran")
library("Runuran", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
install.packages("truncnorm")
library("truncnorm", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

# With truncation


#U<-rtruncnorm(10000000, a=0, b=1, mean = 0.5, sd =0.1)
#U<-sum(U)

# One agent to represent the whole economy
U<-100000

# One representative company
do_once <- function()
{
  # Fraction of the population from whom the platform has users
  u<-urchisq(1,lb=0,ub=1, df = 0.1)

  # Fraction of the total users data to which the platform has access
  fi<-urchisq(1, lb=0,ub=1,df = 0.1)
  
  #Total available data in of this company for production
  D <- (U*u*fi)
  
  #Total production value of the company
  v<-D^(0.1)
}

# Compute for 1M platforms to get heterogeneity in the distribution of data
economy_value_baseline <- replicate(1000, do_once())

#Total production value of the economy
sum(economy_value_baseline)

