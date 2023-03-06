# Fama MacBeth Regression
  # change to use tidyquant
  # Check ASX 20 (excluding Coles)'s risk premium and risk free rate based on single index model (ASX 200)

library(tidyquant)
library(quantmod)
library(ggplot2)
library(tidyverse)
library(lubridate)

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

  # Data Preparation

asx20.stock.returns <- asx20.stocks.price %>%
                       tq_transmute(close, periodReturn, period = "daily", col_rename = "daily_ret") %>%
                       filter(date != '2017-12-29')


asx200.index.returns <- asx200.index.price %>%
                        tq_transmute(close, periodReturn, period = "daily", col_rename = "asx200") %>%
                        filter(date != '2017-12-29')

all.returns <- asx20.stock.returns %>%
               pivot_wider(names_from = symbol, values_from = daily_ret) %>%
               left_join (asx200.index.returns)

all.prices <- asx20.stocks.price %>%
              select(date, symbol, close) %>%
              pivot_wider(names_from = symbol, values_from = close)


first_date <- min(all.prices$date)
last_date <- max(all.prices$date)

# function for calculate returns for the whole period
pd_ret <- function(a){
  x <- a[1]
  y <- a[2]
  opt <- (y/x)^(1/5)-1
  return(opt)
}

whole.periodreturn <- all.prices %>%
                      filter(date == first_date | date == last_date) %>%
                      summarise(across(2:20, pd_ret)) %>%
                      pivot_longer(everything(),names_to = "stock", values_to = "return") %>%
                      left_join(results)

all.returns_date <- all.returns[,1:20] %>%
                    pivot_longer(!date, names_to = "stock", values_to = "return") %>%
                    pivot_wider(names_from = date, values_from = "return") %>%
                    left_join(results)

results <- tibble(stock = character(), beta = numeric())
for (i in names(all.returns[2:20])){
      fit <- lm(get(i) ~ asx200, all.returns)
      results <- add_row(results,stock = i, beta = coefficients(fit)[2])
}

results_2 <- tibble(date = character(), daily_beta = numeric())
for (i in names(all.returns_date[2:1267])){
  fit <- lm(get(i) ~ beta, all.returns_date)
  results_2 <- add_row(results_2, date = i, daily_beta = coefficients(fit)[2])
}

before.FMreg <- lm(return ~ beta, whole.periodreturn)    

std_er <- sd(results_2$daily_beta)/sqrt(dim(results_2)[1]-1)
rbt_t_stat <- mean(results_2$daily_beta)/std_er
p_val <- 2 * pt(q=rbt_t_stat, df = dim(results_2)[1]-2)
