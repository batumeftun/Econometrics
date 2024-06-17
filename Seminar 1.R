#Load libraries
install.packages("xts")
install.packages("dplyr")
install.packages("quantmod")
library(quantmod)
library(dplyr)
library(xts) 

# Tbill, 3 month
getSymbols("TB3MS", src = "FRED")

# convert to annual observations and convert index to type `yearmon`.
TB3MS <- to.yearly(TB3MS, OHLC=FALSE, drop.time = TRUE)
index(TB3MS) <- zoo::as.yearmon(index(TB3MS))

# Inflation
getSymbols("FPCPITOTLZGUSA", src = "FRED")

# Convert the index to yearmon and shift FRED's Jan 1st to Dec
index(FPCPITOTLZGUSA) <- zoo::as.yearmon(index(FPCPITOTLZGUSA)) + 11/12
# Rename and update column names
inflation <- FPCPITOTLZGUSA
colnames(inflation) <- "inflation"

## Deficit, percent of GDP: Federal outlays - federal receipts
# Download outlays
getSymbols("FYFRGDA188S", src = "FRED")

# Lets move the index from Jan 1st to Dec 30th/31st
index(FYFRGDA188S) <- zoo::as.yearmon(index(FYFRGDA188S)) + 11/12
# Rename and update column names
outlays <- FYFRGDA188S
colnames(outlays) <- "outlays"

# Download receipts
getSymbols("FYONGDA188S", src = "FRED")

# Lets move the index from Jan 1st to Dec 30th/31st
index(FYONGDA188S) <- zoo::as.yearmon(index(FYONGDA188S)) + 11/12
# Rename and update column names
receipts <- FYONGDA188S
colnames(receipts) <- "receipts"
# create deficits from outlays - receipts
# xts objects respect their indexing and outline the future
deficit <- outlays - receipts
colnames(deficit) <- "deficit"

# Merge and remove leading and trailing NAs for a balanced data matrix
intdef_updated <- merge(TB3MS, inflation, deficit)
intdef_updated <- zoo::na.trim(intdef_updated)

# Create lags for inflation and deficit

intdef_updated$inflationlag1 <- dplyr::lag(intdef_updated$inflation, n=1)

intdef_updated$deficitlag1 <- dplyr::lag(intdef_updated$deficit, n=1)

#Plot all
plot(intdef_updated, 
     main = "T-bill (3mo rate), inflationlag1, and deficitlag1 (% of GDP)",
     legend.loc = "topright",)
model <- lm(TB3MS ~ inflation + deficit, data = intdef_updated)
updated_model <- lm(TB3MS ~ inflationlag1 + deficitlag1, data = intdef_updated)

summary(model)
summary(updated_model)
