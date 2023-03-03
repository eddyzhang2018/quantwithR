# Fama MacBeth Regression
  # change to use tidyquant

library(tidyquant)
library(quantmod)

# ASX 20 consitutes

asx.20 <- c("ANZ.AX",
            "ALL.AX",
            "BHP.AX",
            "COL.AX",
            "CBA.AX",
            "CSL.AX",
            "FMG.AX",
            "GMG.AX",
            "MQG.AX",
            "NAB.AX",
            "NCM.AX",
            "RIO.AX",
            "STO.AX",
            "S32.AX",
            "TLS.AX",
            "TCL.AX",
            "WES.AX",
            "WBC.AX",
            "WDS.AX",
            "WOW.AX")

# Grab Financial Data from Yahoo Finance

    # Get Stock Prices for ASX 20 stocks
asx20.stocks.price <- asx.20 %>%
  tq_get(get = "stock.prices",
         from = "2017-12-29",
         to = "2022-12-31") %>%
  group_by(symbol) %>%
  filter(symbol != "COL.AX") # COL.AX does not have the same length of data

   # Get Index Values for ASX 200 index
asx200.index.price <- tq_get("^AXJO", get = "stock.prices",
                               from = "2017-12-29",
                               to = "2022-12-31")
