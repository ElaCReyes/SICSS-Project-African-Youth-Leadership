library(rvest)
library(selectr)
library(netstat) #this is where the freeport function came from for selenium
library(RSelenium)

#For Selenium, I used this tutorial: http://joshuamccrain.com/tutorials/web_scraping_R_selenium.html
#for most of the rest of the code I used Ben M's code and then random stack overflows and made stuff up myself

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

#i = 214 #note 6.22 10:34pm - it was stalling apparently on 213 - restarted at 214

#you can try doing the loop below as is, or you can add [0:100] or some other ranage after text_df$url 
#in the first line to segment it - this worked better for me to avoid timing out

for (url in text_df$url){
  #read in webpage
  #delayedAssign("do.next", {next})
  tryCatch({page = read_html(url) #reads html from url and calls it tmp variable "page"
            sections = page %>% 
              html_elements("section") #within page, uses html_elements to copy the "sections" which I think is a css thing
            text_hold = list(sections %>% 
                     html_text2()) #create a list of the sections, using html_text2, scrape the text into a tmp var called text_hold
            list_of_text[i] = text_hold #write the text_hold into the list of lists at position i
            if (list_of_text[i]=="character(0)") #if the read_html function returns only the text "character(0)" then we use selenium to scrape instead
              rD <- rsDriver(browser="firefox", port=free_port(), verbose=F) #initializes a remote browser using firefox
              remDr <- rD[["client"]] #another step to setup the remote driver
              remDr$navigate(url) #navigate the driver to your url - should open firefox and go to the url
              Sys.sleep(5) # give the page time to fully load
              html <- remDr$getPageSource()[[1]] #getpagesource is the html that you couldn't get form read_html above
              body <- read_html(html) %>% # parse HTML, write all objects starting with "<p>" from the html into an object called body
                html_nodes("p")
              list_of_text[[i]] <- paste(body, collapse = "</p>") #this collapses the body into a single line so it can be put into the df
  }, error = function(e) {print(url)}) #prints the urls that are not accessible
  i = i + 1  #increases the index to appropriately fill the list of lists ==in the right row
  
}

text_df$page_text

list_of_text[[i]] <- paste(body, collapse = "</p>") #this collapses the body into a single line so it can be put into the df


#need to add an error message into the page_text column

#add list of texts to data frame
text_df$page_text = list_of_text

#final data frame organized by url :)
View(text_df)


temp <- text_df



#df = as.matrix(text_df)
#write.csv(df,"C:/Users/bonni/Dropbox/SICSS/scraped_data.csv", row.names = FALSE)

Null <- sum(text_df$page_text == "NULL") + sum(text_df$page_text == "character(0)") #count how many are missing info

print(Null)