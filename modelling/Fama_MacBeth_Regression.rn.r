# Fama MacBeth Regression

library(tidyverse)
library(RSQLite)
library(lubridate)
library(sandwich)
library(broom)
library(tidyquant)
library(PerformanceAnalytics)

# house keeping

  # for data frame, use "." in name
  # for row/column in data, use "_" in name


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
            "WOW.AX",
            "^AXJO")

# Grab Financial Data from Yahoo Finance

asx.20.price <- asx.20 %>%
  tq_get(get = "stock.prices",
         from = "2017-12-29",
         to = "2022-12-31") %>%
  group_by(symbol)
  
# Data Preparation

  # Key Dates

stock.last.date <- asx.20.price %>%
                group_by(symbol) %>%
                summarize(last_date = max(date))

stock.first.date <- asx.20.price %>%
                    group_by(symbol) %>%
                    summarize(last_date = min(date))

first.date <- min(stock.first.date$last_date)

last.date <- min(stock.last.date$last_date)

date_5y <- last.date %m-% months(60) 
