install.packages("tseries")
install.packages("quantmod")
install.packages("Quandl")
install.packages("xts")
install.packages("tidyquant")
install.packages("tidyverse")
install.packages("timetk")
install.packages("broom")
install.packages("glue")
install.packages("readr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("MASS")


## changes - add readr, dplyr, lubridate
## add L to NUL
## is the date column of stk removed somewhere along the way?


library(tseries)
library(quantmod)
library(Quandl)
library(xts)
library(tidyquant)
library(tidyverse)
library(timetk)
library(broom)
library(glue)
library("readr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("MASS")

#Define the date range:
open_date  = "1997-12-31"
close_date = Sys.Date()

#Choose ticker:
ticker_name = "AAPL"

#Load data into the environment:

stk <-data.frame(getSymbols(ticker_name,
                            src='yahoo',
                            reload.Symbols = TRUE,
                            from = open_date,
                            to = close_date,
                            auto.assign = FALSE))
stk$date<-as.Date(rownames(stk),format="%Y-%m-%d")
rownames(stk)<-NULL
View(stk)


temp <- tempfile()
base <-
  "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"

factor <-
  "F-F_Research_Data_5_Factors_2x3_daily"

format<-
  "_CSV.zip"

full_url <-
  glue(base,
       factor,
       format,
       sep ="")

download.file(
  full_url,
  temp,
  quiet = TRUE)

FF5 <-
  read_csv(unz(temp, "F-F_Research_Data_5_Factors_2x3_daily.CSV"),
           skip = 3) %>%
  rename(date = X1) %>%
  mutate_at(vars(-date), as.numeric) %>%
  mutate(date =
           ymd(parse_date_time(date, "%Y%m%d")))
View(FF5)

df <- merge(x=stk, y=FF5, by='date', all = FALSE)

#add an Excess Returns column for returns greater than the risk free rate
df <- mutate(df, 
             Excess_Returns = ((df$AAPL.Close - df$AAPL.Open) / df$AAPL.Open ) - df$RF)

#run the regression
colnames(df)[8] <- "Mkt_RF" #rename the column so it's workable
no_interactions_fit <- lm(Excess_Returns ~ Mkt_RF + SMB + HML + RMW + CMA, data = df)
step1 <- stepAIC(no_interactions_fit, direction = "both")
step1$anova             


interaction_fit <- lm(Excess_Returns ~ (Mkt_RF + SMB + HML + RMW + CMA)^2, data = df)
step2 <- stepAIC(interaction_fit, direction = "both")
step2$anova

#we see that the AIC for the interaction fit is lower and is thus a better fit

hist(step$residuals, breaks = 200)

##NEXT STEPS:
##Change mutate above to make EXCESS RETURNS = %return - RF (%return = (pricechange + dividend)/ openprice)
##Run a 5 factor test to see what effects each one has on the excess return
##plot - histagram of residuals or x = a FF Factor y = excess return or maybe the residuals
