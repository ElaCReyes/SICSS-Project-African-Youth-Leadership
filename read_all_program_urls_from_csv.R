library(rvest)
library(selectr)
getwd()

path <- ("C:/Users/bonni/Dropbox/SICCS/") #put the path to the csv file here
setwd(path)

progs <- read.csv(file = 'sicss_project_organization_list.csv')
View(progs)

#urls 

n_progs <- length(progs$ID.)
print(n_progs)

#initialize data frame
text_df <- data.frame(matrix(ncol = 3, nrow = n_progs))

View(text_df)
#change column names
colnames(text_df) <- c('ID','url', 'page_text')

#put urls in
text_df$url <- progs$Website
text_df$ID <- progs$ID.


#empty vector to store text
list_of_text <- vector('list', n_progs) #creates an empty list rather than using c() which creates list of things you put between parentheses
View(list_of_text)

i = 1
#for each url in the list, go to the webpage, scrape the text, and add it to the empty list created above.
for (url in text_df$url){
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
  
  i = i+1
}

View(page)
#add list of texts to data frame
text_df$page_text = list_of_text

#final data frame organized by url :)
View(text_df)


