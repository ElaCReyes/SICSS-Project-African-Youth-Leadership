##This is all copied from Ela's code, so none of it is original!
# to get the data needed, you can use the code "Scraper_BRS_0622.R"
# once completed, you should have a dataframe called "text_df"
# We will work with text_df but I changed it's name to temp to avoid confusion, see first line of code below pacman

pacman::p_load(
  tidyverse,
  pdftools, 
  tesseract,
  tidytext, 
  plyr,
  stringr, #to work with string data
  stringi, #to work with string data
  tm, #for text analysis
  stm, 
  lda,
  topicmodels, #topic models
  reshape2 #needed for creating the topic model graphs
)

temp <- text_df
n_progs <- length(text_df$ID) #measures the number of observations in the csv


# GREP - tbh not sure this worked on the webscraped data?
temp$data_clean <- gsub("\n|\r|\t|\v|\f|•|·|~|-|[[:punct:]]
                                |ÿ| ÿ |  ÿ|ÿ  |ÿ5|ÿÿ|4ÿ|http
                                ", "", temp$page_text)


# Tokenizing - using a new df called "temptok" to tokenize
#next three lines create the df
temptok <- data.frame(matrix(ncol = 2, nrow = n_progs)) #creates a df with 2 columns and same rows as # of obs.
colnames(temptok) <- c('ID','clean_text')
temptok$ID <- temp$ID

#cleaning the text
temptok$clean_text <-temp$data_clean
temptok$clean_text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", temptok$clean_text) # Remove 1-2 letter words
temptok$clean_text <- gsub("^ +| +$|( ) +", "\\1", temptok$clean_text) # Remove excessive spacing
temptok$clean_text <- gsub('[0-9]+', '', temptok$clean_text) #remove digits
temptok$clean_text <- gsub(pattern = ('\\\\'), replacement = '', x = temptok$clean_text, fixed = F) 
#print(temptok$clean_text[2])

#create a new list called tokenize with all the words in the cleaned text listed
tokenize <- temptok %>%
  select(clean_text) %>% #Checke whether it works only using TextData. It works w/o File variable
  unnest_tokens("word", clean_text)

head(tokenize) #see what it looks like


#Count Words but we still have the stopwords
tokenize %>% 
  group_by(word) %>% 
  count() %>%
  arrange(desc(freq))

#Remove stopwords with tidytext
data(stop_words) #Load this data of tidytext
tokenize <- tokenize %>%
  anti_join(stop_words) 

#I noticed that so many of the words were nonsense html words, so I created a new list of them, but it's not exhausive
#the list of words to remove is called nonsense, which I then remove below:
nonsense <- c("span", "class", "style", "strong", "href", "https", "px", "wpcontent", "uploads", "datacontrast", "target", "width", "img", "color", "src", "bcx", "scxw", "_blank", "height", "amp", "dataadtagsvisited", "bold", "color_", "auto", "display", "textalign" ,"alt", "option", "http", "fontsize", "svg", "eng", "giz", "engb", "lang", "type", "fontsize:px", "div", "rel", "normaltextrun", "textrun", "xml:lang")
nonsense_df <- data.frame(matrix(ncol = 1, nrow = length(nonsense))) #creates a df with 2 columns and same rows as # of obs.
colnames(nonsense_df) <- c('word')
nonsense_df$word <- nonsense
View(nonsense_df)

#remove the nonsense words
tokenize <- tokenize %>%
  anti_join(nonsense_df) 


# Count again to see the difference
tokenize %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(desc(freq)) 

#write.csv(tokenize, "ScrapedDataTokenized.csv")


# So I used the tm() package

#Creates corpus
TidyOrgCorpus <- Corpus(VectorSource(as.vector(tokenize$word))) 
#Creates Document Term Matrix
OrgCorpus_DTM <- DocumentTermMatrix(TidyOrgCorpus, 
                                    control = list(wordLengths = c(2, Inf)))
#To see how the DTM looks like
inspect(OrgCorpus_DTM[1:25,3:8])
#The one below shoes the corpus into a tibble. I'm actually not sure what this is for
tidy(OrgCorpus_DTM)


Topwordstokenize <- 
  tokenize %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(desc(freq)) 


#Aggregated Duplicates. 
TidyCorpusRawCleanAggregated <- aggregate(freq~word, #formual of values ~ grouping
                                          data = Topwordstokenize , #data set
                                          FUN = sum) #function to apply to the formula



#Check Top Words now
TidyCorpusRawCleanAggregated %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(desc(freq)) 

#creates bar plot
TidyCorpusRawCleanAggregated %>% 
  arrange(desc(freq)) %>% 
  slice(1:20) %>% 
  ggplot(mapping = aes(x = reorder(word, -freq), y = freq, fill = word)) + #adding the mapping is better to avoid errors
  geom_bar(stat = "identity") + #You need the stat = "identity" to show geom_bar that you already have the counts in y. 
  theme_minimal() + #To delete the grey background
  theme(axis.title.x = element_text(angle = 0, vjust = 10, hjust=10)) + #not working pretty
  theme(plot.title = element_text(hjust = 0.5, size=18))+
  ylab("Frequency")+
  xlab("")+
  ggtitle("Most Frequent Words in Webscraped Text")+
  guides(fill="none")



###############################################################
################### Topic Modeling k =10 ######################
###############################################################

OrgTopicModel<- LDA(OrgCorpus_DTM, #The DTM Matrix
                    k=10, #The number of clusters you want
                    control = list(seed = 321))

OrgTopics <- tidy(OrgTopicModel, #The object just above. The LDA_VEM object
                  matrix = "beta" #so that it extracts "beta" element from the LDA_VEM  object
)

# Bar graphs that describe the top terms for each topic:
# Yes, you need the "beta" is one of the elements of the LDA_VEM object

#First you need to create the data for the bra graph!
OrgTopTerms <- 
  OrgTopics %>% #Take OrgTopics
  group_by(topic) %>%  #gropu by variable topic that is 1-10 becasue we had k=10
  top_n(7, beta) %>%  #take the top 10 based on the beta variable
  ungroup() %>% #This one I'm not sure
  arrange(topic, -beta)

#Now You create the Bar graph

OrgTopTerms %>% #Take this object and then
  mutate(term = reorder(term, beta)) %>% #in variable term, reorder the terms based on beta value
  ggplot(mapping = aes(y = term, x = beta, fill = factor(topic))) + #fill based on topic variable and treat that variable as a factor
  geom_col(show.legend = FALSE) + #Until here it will show a horizontal bar graph
  facet_wrap(~ topic, scales = "free") +  # it divides into ten different bar graphs based on the topic
  ylab("Topics")+
  xlab("Probability of Term to be Assigned to Topic")+
  ggtitle("Webscrape Text Topic Modeling k = 10")

###############################################################
################### Topic Modeling k = 5 ######################
###############################################################

OrgTopicModelK5<- LDA(OrgCorpus_DTM, #The DTM Matrix
                      k=5, #The number of clusters you want
                      control = list(seed = 321))

OrgTopicsK5 <- tidy(OrgTopicModelK5, #The object just above. The LDA_VEM object
                    matrix = "beta" #so that it extracts "beta" element from the LDA_VEM  object
)

# Bar graphs that describe the top terms for each topic:
# Yes, you need the "beta" is one of the elements of the LDA_VEM object

#First you need to create the data for the bra graph!
OrgTopTermsK5 <- 
  OrgTopicsK5 %>% #Take OrgTopics
  group_by(topic) %>%  #gropu by variable topic that is 1-10 becasue we had k=10
  top_n(7, beta) %>%  #take the top 10 based on the beta variable
  ungroup() %>% #This one I'm not sure
  arrange(topic, -beta)

#Now You create the Bar graph

OrgTopTermsK5 %>% #Take this object and then
  mutate(term = reorder(term, beta)) %>% #in variable term, reorder the terms based on beta value
  ggplot(mapping = aes(y = term, x = beta, fill = factor(topic))) + #fill based on topic variable and treat that variable as a factor
  geom_col(show.legend = FALSE) + #Until here it will show a horizontal bar graph
  facet_wrap(~ topic, scales = "free") +  # it divides into ten different bar graphs based on the topic
  ylab("Topics")+
  xlab("Probability of Term to be Assigned to Topic")+
  ggtitle("Webscrape Text Topic Modeling k = 5")

###############################################################
################### Topic Modeling k = 7 ######################
###############################################################

OrgTopicModelK7<- LDA(OrgCorpus_DTM, #The DTM Matrix
                      k=7, #The number of clusters you want
                      control = list(seed = 321))

OrgTopicsK7 <- tidy(OrgTopicModelK7, #The object just above. The LDA_VEM object
                    matrix = "beta" #so that it extracts "beta" element from the LDA_VEM  object
)

# Bar graphs that describe the top terms for each topic:
# Yes, you need the "beta" is one of the elements of the LDA_VEM object

#First you need to create the data for the bar graph!
OrgTopTermsK7 <- 
  OrgTopicsK7 %>% #Take OrgTopics
  group_by(topic) %>%  #group by variable topic that is 1-10 becasue we had k=10
  top_n(7, beta) %>%  #take the top 10 based on the beta variable
  ungroup() %>% #This one I'm not sure
  arrange(topic, -beta)

#Now You create the Bar graph

OrgTopTermsK7 %>% #Take this object and then
  mutate(term = reorder(term, beta)) %>% #in variable term, reorder the terms based on beta value
  ggplot(mapping = aes(y = term, x = beta, fill = factor(topic))) + #fill based on topic variable and treat that variable as a factor
  geom_col(show.legend = FALSE) + #Until here it will show a horizontal bar graph
  facet_wrap(~ topic, scales = "free") +  # it divides into ten different bar graphs based on the topic
  ylab("Topics")+
  xlab("Probability of Term to be Assigned to Topic")+
  ggtitle("Webscrape Text Topic Modeling k = 7")



###############################################################
################### Topic Modeling k = 13 ######################
###############################################################

OrgTopicModelK13<- LDA(OrgCorpus_DTM, #The DTM Matrix
                      k=13, #The number of clusters you want
                      control = list(seed = 321))

OrgTopicsK13 <- tidy(OrgTopicModelK13, #The object just above. The LDA_VEM object
                    matrix = "beta" #so that it extracts "beta" element from the LDA_VEM  object
)

# Bar graphs that describe the top terms for each topic:
# Yes, you need the "beta" is one of the elements of the LDA_VEM object

#First you need to create the data for the bar graph!
OrgTopTermsK13 <- 
  OrgTopicsK13 %>% #Take OrgTopics
  group_by(topic) %>%  #group by variable topic that is 1-10 becasue we had k=10
  top_n(13, beta) %>%  #take the top 10 based on the beta variable
  ungroup() %>% #This one I'm not sure
  arrange(topic, -beta)

#Now You create the Bar graph

OrgTopTermsK13 %>% #Take this object and then
  mutate(term = reorder(term, beta)) %>% #in variable term, reorder the terms based on beta value
  ggplot(mapping = aes(y = term, x = beta, fill = factor(topic))) + #fill based on topic variable and treat that variable as a factor
  geom_col(show.legend = FALSE) + #Until here it will show a horizontal bar graph
  facet_wrap(~ topic, scales = "free") +  # it divides into ten different bar graphs based on the topic
  ylab("Topics")+
  xlab("Probability of Term to be Assigned to Topic")+
  ggtitle("Webscrape Text Topic Modeling k = 13")