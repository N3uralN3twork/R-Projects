#The goal of this project is to retrive stock quotes, like I've done before
#However, I lost the file


#Import the necessary libraries
library(quantmod) #Retrieves the data
library(xlsx)     #Write to an excel file
library(lubridate) #To split dates


#List the stock prices you would like to receive:
Symbols = c("AAPL", "AMZN", "FB", "NFLX", "GOOGL", "NVDA",
            "IBM", "INTC", "MSFT", "QCOM", "TSM", "MS") #Tech stocks

Symbols = sort(Symbols) #Sort in alphabetical order

Companies = c("Apple", "Amazon", "Facebook", "Google", "IBM", "Intel", "Morgan Stanley",
              "Microsoft", "Netflix", "Nvidia", "Qualcomm", "Taiwan Semiconductor")


#Retrive the information
Quotes <- getQuote(Symbols, src = "yahoo")


"Join new columns:"
Quotes = cbind(Companies, Quotes)

Quotes$Weekday = weekdays(as.Date(Quotes$`Trade Time`))


"Create an excel workbook"
#Can change in the future if needed
file = "C:/Users/MatthiasQ.MATTQ/Desktop/R Projects/Quotes.xlsx"


write.xlsx(Quotes, file = file, sheetName = "Quotes",
           col.names = TRUE, row.names = TRUE, append = FALSE)
#This took like 40 minutes to build lol.