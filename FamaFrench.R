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
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(MASS)

#Define the date range:
open_date  = "1997-12-31"
close_date = Sys.Date()

#Choose ticker:
ticker_name = "AAPL"

#Load historical stock data into the environment:
stk <-data.frame(getSymbols(ticker_name,
                            src='yahoo',
                            reload.Symbols = TRUE,
                            from = open_date,
                            to = close_date,
                            auto.assign = FALSE))

#make the rownames its own "date" column
stk$date<-as.Date(rownames(stk),format="%Y-%m-%d")
rownames(stk)<-NULL
View(stk)

#download the Fama French 5 factor data then read from the csv
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

#combine the stock and Fama French data frames
df <- merge(x=stk, y=FF5, by='date', all = FALSE)

#add an Excess Returns column for returns greater than the risk free rate
df <- mutate(df, 
             Excess_Returns = ((df$AAPL.Close - df$AAPL.Open) / df$AAPL.Open ) - df$RF)

#rename the column so the functions can work with it
#just changing the hyphen to an underscore
colnames(df)[8] <- "Mkt_RF" 

#run the regression without interactions
no_interactions_fit <- lm(Excess_Returns ~ Mkt_RF + SMB + HML + RMW + CMA, data = df)

#look at the betas
no_interactions_fit$coefficients

#view the anova table, we see all factors except for SMB are significant at 
#at least the .05 level. SMB is signicant at the .1 level
summary(no_interactions_fit)

#use the stepAIC function for variable selection to see if we can 
#improve our model
step1 <- stepAIC(no_interactions_fit, direction = "both")

#view the selected model, we see that the final model
#still has all 5 factors and could not be reduced
step1$anova             


#Below I ran the regression with interactions just to help with learning
#how to use R and perform regressions
#With the Fama French 5 Factor Model we just use the risk factors individually
#Otherwise that means each interation is another risk factor that is also priced
#which doesn't make as much sense conceptually and makes the model more complicated
#########
#run the regression with interactions
interaction_fit <- lm(Excess_Returns ~ (Mkt_RF + SMB + HML + RMW + CMA)^2, data = df)

#use the stepAIC function again for variable selection
step2 <- stepAIC(interaction_fit, direction = "both")

#view the selected model and the betas. We see that only 5 of 10 possible interactions
#are included in the final model, in addition to the individual 5 factors 
step2$anova
step2$coefficients

#we see that the AIC for the interaction fit is lower and is thus a better fit
#########

#check to see that the residuals are normally distributed 
hist(no_interactions_fit$residuals, 
     breaks = 100, 
     col = "dark blue", 
     main = "Residuals from Linear Model",
     xlab = "")

#plot the residuals against the returns
plot(x = df$Excess_Returns, 
     y = no_interactions_fit$residuals, 
     main = "Residuals vs. Excess Returns",
     xlab = "Excess Returns",
     ylab = "Residuals")


#create a function for the model to plot residuals
#against fitted values
betas <- no_interactions_fit$coefficients
names(betas) <- NULL

fama_french_model <- function(Mkt_RF, SMB, HML, RMW, CMA){
  fitted_values <- betas[1] + betas[2]*Mkt_RF + betas[3]*SMB +
                   betas[4]*HML + betas[5]*RMW + betas[6]*CMA
}
