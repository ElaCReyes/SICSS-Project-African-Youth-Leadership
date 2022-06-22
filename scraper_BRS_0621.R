library(rvest)
library(selectr)
library(netstat)

getwd()

setwd("C:/Users/bonni/Dropbox/SICSS") #set your wd to the folder where the project list is saved as a csv

progs <- read.csv(file = 'sicss_project_organization_list.csv') #reads the csv into a dataframe called progs

#urls 

n_progs <- length(progs$ID.) #measures the number of observations in the csv

#initialize data frame
text_df <- data.frame(matrix(ncol = 3, nrow = n_progs)) #creates a df with 3 columns and same rows as # of obs.

#change column names 
colnames(text_df) <- c('ID','url', 'page_text')

#put urls and the program ID into the df
text_df$url <- progs$Website
text_df$ID <- progs$ID.


#creates an empty vector to store text for each url
list_of_text <- vector('list', n_progs) #creates an empty list rather than using c() which creates list of things you put between parentheses


i = 1 #sets your index to 1

#for each url in the list, go to the webpage, scrape the text, and add it to the empty list created above, increase index by 1; 
#if the text comes back as "character(0)", then use selenium to scrape it - slower but works
#I set this to only look at urls 1 to 10, which all work, if you try 1:20, you get an error for 12.
for (url in text_df$url[1:10]){
  #read in webpage
  page = read_html(url)
  
  #get the main page loaded
  sections = page %>% 
    html_elements("section")
  
  #Get the text
  text_hold = list(sections %>% 
                     html_text2())
  
  #add text to empty vector
  list_of_text[i] = text_hold
  
  #use selenium if the thing above didn't work
  if (list_of_text[i]=="character(0)")
    rD <- rsDriver(browser="firefox", port=free_port(), verbose=F)
    remDr <- rD[["client"]]
    remDr$navigate(url)
    Sys.sleep(5) # give the page time to fully load
    html <- remDr$getPageSource()[[1]]
    body <- read_html(html) %>% # parse HTML
      html_nodes("p")
    list_of_text[i] <- paste(body, collapse = "</p>")
    
  
  i = i+1
}

#add list of texts to data frame
text_df$page_text = list_of_text

#final data frame organized by url :)
View(text_df)
