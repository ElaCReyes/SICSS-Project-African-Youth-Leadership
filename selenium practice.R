#selenium practice
install.packages("RSelenium")
install.packages("rvest")
install.packages("tidyverse")
library(RSelenium)
library(rvest)
library(tidyverse)

rD <- rsDriver(browser="firefox", port=3545L, verbose=F)
remDr <- rD[["client"]]
remDr$navigate("https://africanleadershipacademy.org/")
Sys.sleep(5) # give the page time to fully load
html <- remDr$getPageSource()[[1]]

print(html)

