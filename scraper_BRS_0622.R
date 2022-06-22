library(rvest)
library(selectr)
library(netstat) #this is where the freeport function came from for selenium
library(RSelenium)

getwd()

setwd("C:/Users/bonni/Dropbox/SICSS") #set your wd to the folder where the project list is saved as a csv

progs <- read.csv(file = 'sicss_project_organization_list.csv') #reads the csv into a dataframe called progs
#View(progs)

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


#for each url first try these steps: 
#1) go to the webpage, scrape the text, and add it to the empty list created above, increase index by 1; 
#2) if the text comes back as "character(0)", then use selenium to scrape it - slower but works
#if there is an error, print the url with the error
# then add 1 to i so it knows where to add the text into the list of lists (list_of_text)

for (url in text_df$url[1:50]){
  #read in webpage
  #delayedAssign("do.next", {next})
  tryCatch({page = read_html(url) #reads html from url and calls it tmp variable "page"
            sections = page %>% #within page, uses html_elements to copy the "sections" which I think is a css thing
              html_elements("section") #
            text_hold = list(sections %>% 
                     html_text2())
            list_of_text[i] = text_hold
            if (list_of_text[i]=="character(0)")
              rD <- rsDriver(browser="firefox", port=free_port(), verbose=F)
              remDr <- rD[["client"]]
              remDr$navigate(url)
              Sys.sleep(5) # give the page time to fully load
              html <- remDr$getPageSource()[[1]]
              body <- read_html(html) %>% # parse HTML
                html_nodes("p")
              list_of_text[[i]] <- paste(body, collapse = "</p>") #this collapses the html into a single line so it can be put into the df
  }, error = function(e) {print(url)}) #prints the urls that are no accessible
  i = i + 1  #increases the index to appropriately fill the list of lists ==in the right row
  
}

#need to add an error message into the page_text column

#add list of texts to data frame
text_df$page_text = list_of_text

#final data frame organized by url :)
View(text_df)

