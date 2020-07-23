##########################################
# Final project
# Team: Fu Maloney Tran Ventosa
# Data: IBM Watson Marketing Customer Value
# Subject: Auto Insurance Customer Lifetime Value (CLV)
#########################################

# Load Data: Original copy stored in jcventosa github.
AutoCLV <-read.csv("https://raw.githubusercontent.com/jcventosa/IST-707/master/WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv")

# Data Cleaning and Transformation

# Checking structure for data types
str(AutoCLV)
# 9124 records 24 variables
# data types appear to be correctly formatted
# Consider creating bins for CLV, Income, Monthly.Premium.Auto, Total.Claim.Amount and
# Number.of.Policies as additional columns in dataframe.

# Checking for missing or duplicated data
sum(is.na(AutoCLV))
nrow(AutoCLV[duplicated(AutoCLV),])
# no missing or duplicated data found

# Summary check
summary(AutoCLV)
# Initial observation: all good with possible outliers in the numeric data items.
# All column headings appear to be self explanatory with the exceptions of 
# "Response" and "Effective.to.Date".  Will need to clarify definitions. See if 
# "Effective.to.Date" corresponds with "Months.Since.Policy".  Check if "Response"
# is in reference to "Renew.Offer.Type".

# Adding columns for CLV, Income, Monthly Premium and Total Claim bins.

# Cutomer Lifetime Value bins
AutoCLV$CLV_Bins <- cut(AutoCLV$Customer.Lifetime.Value, 
                       breaks = c(0,1999,3999,5999,7999,9999,Inf),
                       labels = c("< $2000", "$2000-$3999","$4000-$5999",
                                  "$6000-$7999", "$8000-$9999", "$10000+"))
# Income bins
AutoCLV$IncomeBin <- cut(AutoCLV$Income, breaks = c(0,14999,29999,44999,59999,74999,Inf),
                    labels = c("< $15000", "$15000-$29999","$30000-$44999",
                               "$45000-$59999", "$60000-$74999", "$75000+"))
# Monthly Premium bins
AutoCLV$MoPremiumBin <- cut(AutoCLV$Monthly.Premium.Auto, 
                            breaks = c(0,74,99,124,149,Inf),
                            labels = c("< $75", "$75-$99","$100-$124","$125-$149","$150+"))

# Total Claim bins
AutoCLV$ClaimBin <- cut(AutoCLV$Total.Claim.Amount, 
                        breaks = c(0,249,499,749,999,Inf),
                        labels = c("< $250", "$250-$499","$500-$749","$750-$999","$1000+"))

# Spot check bin labels match appropriate values
head(AutoCLV[,c(3,25,10,26,13,27,22,28)],30)

# Replace NA in IncomeBin with "< $15000"
AutoCLV$IncomeBin[is.na(AutoCLV$IncomeBin)] <- "< $15000"
sum(is.na(AutoCLV$IncomeBin))
head(AutoCLV[,c(10,26)],15)

# Save revised data to csv file
write.csv(AutoCLV, file = "~/Documents/AutoCLV.csv")
