#load packages
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
  topicmodels #topic models
)

##############################################################
###################### Text from PDF #########################
##############################################################

setwd("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data")
getwd()

#Test 1: https://alexluscombe.ca/blog/getting-your-.pdfs-into-r/
#Vector of pdf file names 
file_vector <- list.files(path = "/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data")

results <- vlapply(file_vector, pdf_text)
#Error in poppler_pdf_text(loadfile(pdf), opw, upw) : PDF parsing failure.

#Test 1: https://stackoverflow.com/questions/21445659/use-r-to-convert-pdf-files-to-text-files-for-text-mining

#folder with pdfs
dest <- "/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data"
# make a vector of PDF file names
myfiles <- list.files(path = dest, pattern = "pdf", full.names = TRUE)
myfiles[1]

print(myfiles)

##### Checking if each PDF them works (First 10)#####
# 1. AYL0S23_Bridge2Rwanda.pdf 
# DOES NOT WORK - These are images. 
AYL0S23_Bridge2Rwanda.pdf <- pdf_text("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data/AYL0S23_Bridge2Rwanda.pdf")

# 2. AYLS001_ALA.pdf 
# WORKS
pdf_text("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data/AYLS001_ALA.pdf")

# 3. AYLS004_AfrikaTikkun.pdf
# WORKS
pdf_text("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data/AYLS004_AfrikaTikkun.pdf")

#4. AYLS035_AndelaTechnicalLeadershipProgram.pdf
# DOES NOT WORK
pdf_text("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data/AYLS035_AndelaTechnicalLeadershipProgram.pdf")

#5. AYLS045EmergingLeadersFoundation.pdf
# WORKS
pdf_text("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data/AYLS045EmergingLeadersFoundation.pdf")

#6. AYLS059_1BillionAfrica.pdf
# DOES NOT WORK
pdf_text("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data/AYLS059_1BillionAfrica.pdf")
print(AYL0S23_Bridge2Rwanda.pdf)

#7. AYLS092_OpenSocietyCivilSocietyLeadershipAwards_USA.pdf
# WORKS
pdf_text("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data/AYLS092_OpenSocietyCivilSocietyLeadershipAwards_USA.pdf")

#8. AYLS093_CommonwealthScholarship.pdf
#WORKS
pdf_text("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data/AYLS093_CommonwealthScholarship.pdf")

#9. AYLS136_KectilGeneration.pdf
#WORKS
pdf_text("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data/AYLS136_KectilGeneration.pdf")

#10. AYLS143_AfricanYouthCouncilonClimateChange.pdf
#WORKS
pdf_text("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data/AYLS143_AfricanYouthCouncilonClimateChange.pdf")


##### End Check One by One ### 
#############################################

#There was a corrupt file!!! AYLS038_ColumbaLeadership.pdf

corpus_raw <- data.frame("org" = c(),"text" = c())

#  Test 2 Loop was unsuccessful because it was rewriting data

for (i in 1:length(myfiles)){
  x <- pdf_text(myfiles[i], opw = "", upw = "") 
  document <- data.frame("org" = gsub(x =myfiles[i],pattern = ".pdf", replacement = ""), 
             "text" = x, stringsAsFactors = FALSE)
  colnames(document) <- c("org", "text")
  corpus_raw <- rbind(corpus_raw,document)
}
print(x)

#Test 3
# Loop was unsuccessful because it was rewriting data
for (i in 1:length(myfiles)){
  rawtext <- pdf_text(myfiles[i], opw = "", upw = "") 
  document <- data.frame( #Creates new data frame
                        "org" = gsub(x =myfiles[i],pattern = ".pdf", replacement = ""), #This uses the gsub function of the regular expressions in R
                         "text" = x, stringsAsFactors = FALSE
                        )
  colnames(document) <- c("org", "text")
  corpus_raw <- rbind(corpus_raw,document)
}

# Test 4 I'm using the code in this website
#https://stackoverflow.com/questions/59519032/how-do-i-get-my-loop-on-pdf-text-only-to-read-all-the-files

myfiles_list <- list() #Created this object to put the output of the loop

for (i in 1:length(myfiles)) {
  print(i) #to make sure the loop is working
  myfiles_list[myfiles[[i]]] <- pdf_text(myfiles[i]) %>% # This add the output of pdf_text() to the list object. 
  tibble() # txt = . I believe this converts to a tibble (another way to say data.frame) and makes the output be txt format
}

View(myfiles_list)

#Test 5 I'm using Bonnie's suggestions 
# The previous loop created a row per each page. Becasue it was tibble. T
# This one creates one list per document separated by a comma. 
MyFilesList <- list() #Created this object to put the output of the loop
i = 1
for (i in 1:length(myfiles)) {
  print(i) #to make sure the loop is working
  MyFilesList[[myfiles[i]]] <- pdf_text(myfiles[i])  %>% #This adds the output of pdf_text() to the list object.
    list(txt = .) #I believe this converts to a tibble (another way to say data.frame) and makes the output be txt format
} #The only change is tibble() to list(txt = .)

#Saving doc
getwd()
#This does not work anymore
#Do not run again. This is only for demosntation purposes on how it went wrong. 
#write.csv(corpus_raw, file = "corpus_raw_failed.csv", row.names = FALSE)

##############################################
###########  Trial with own pdfs #############
##############################################

dest_latinx <- "/Users/Ela 1/Documents/LDT Mac/1. Research_2/Latinx in Academia/Dr. Masta readings"
myfiles_latinx <- list.files(path = dest_latinx, pattern = "pdf", full.names = TRUE)
print(myfiles_latinx)

for (i in 1:length(myfiles_latinx)){
  y <- pdf_text(myfiles_latinx[i], opw = "", upw = "")
}

print(y)

#Trial 2 to find the problem
#I'm trying this code 
# https://stackoverflow.com/questions/59519032/how-do-i-get-my-loop-on-pdf-text-only-to-read-all-the-files 
# I bleive this code actually works 
files <- list.files(pattern = "pdf$")
files_pdfs <- list()
myfiles_latinx_list <- list()


for (i in 1:length(myfiles_latinx))
{
  print(i)
  myfiles_latinx_list[[myfiles_latinx[i]]] <- pdf_text(myfiles_latinx[i]) %>% 
    tibble(txt = .)   # %>% 
    #unnest_tokens(word, txt) # This function tokenizes so I'm not using it until I have the two column output. 
}

#This was to inspect each of the elements 
z <-myfiles_latinx_list[[8]]
View(z)

print(myfiles_latinx_list)

##############################################################
#################### List to Data Frame #######################
##############################################################

# I'm going to transform the nested data (list) into a regular data frame. 

# Test 1: This code was based on the Test 5 of the automation. 
# It results in the two column BUT per page!!! 
# 963 rows....
#https://stackoverflow.com/questions/4227223/convert-a-list-to-a-data-frame
library (plyr)
#Process
#Extract data from list of lists
Step1 <- ldply(myfiles_list, data.frame)

# Rename columns
#https://statisticsglobe.com/r-error-cant-rename-columns-that-dont-exist
Step2 <-  plyr::rename(Step1, c(".id" = "File",  
                                "X..i.." = "TextData"))

#Combined
CorpusRaw <- myfiles_list  %>% 
               ldply(data.frame) %>% 
                 plyr::rename(., c(".id" = "File", #Need to use plyr:: to avoid issues with dplyr package
                                   "X..i.." = "TextData")) 


#Do not Run again
#write_csv(CorpusRaw, "CorpusRaw.csv")

# Test 2: Using Bonnie's work. 
CorpusRawT2<-data.frame(matrix(ncol=2, nrow=25)) #Create a data frame T2 is because is Test 2
colnames(CorpusRawT2) <- c('File', 'TextData') #Change names

CorpusRawT2$File <- myfiles #Add names of paths to column files

CorpusRawT2$TextData <- MyFilesList #put the list created by the loop into the df column text

#The loop below takes the lists per each document from the step above (CorpusRawT2$TextData)
# and collapses them using the paste() function
i = 1
for (x in CorpusRawT2$TextData){
  CorpusRawT2$temp[i] <- paste(x, collapse = "") #I had to delete the comma from the collapse 
  i = i + 1
}

#Change name of third variable
colnames(CorpusRawT2) <- c('File', 'TextData', 'TextDataCollapsed') 
#Select Only useful variable leaving list out
CorpusRawT2Collapsed <-  CorpusRawT2 %>%   
  select(File, TextDataCollapsed) 

class(CorpusRawT2Collapsed)
dim(CorpusRawT2Collapsed)

#write.csv(CorpusRawT2Collapsed, "CorpusRawT2Collapsed.csv")

##############################################################
############# Basic Text with Chris Bail Instructions ########
##############################################################

#Take 1: 
# GREP
CorpusRawClean <- CorpusRaw #Create a new one to have the middle point
CorpusRawClean$TextData <- gsub("\n|\r|\t|\v|\f|•|·|~|-|[[:punct:]]
                                |ÿ| ÿ |  ÿ|ÿ  |ÿ5|ÿÿ|4ÿ|http
                                ", "", CorpusRawClean$TextData)

# Tokenize
TokenizedCorpusRawClean <- CorpusRawClean %>%
  select(File,TextData) %>% #Checke whether it works only using TextData. It works w/o File variable
  unnest_tokens("word", TextData)
head(TokenizedCorpusRawClean)


#Count Words but we still have the stopwords
TokenizedCorpusRawClean %>% 
  group_by(word) %>% 
    count() %>%
      arrange(desc(freq))

# word freq
# 1             and 1385
# 2              to 1232
# 3             the 1147
# 4              in  947

#Remove stopwords with tidytext
data(stop_words) #Load this data of tidytext
View(stop_words)
TidyCorpusRawClean <- TokenizedCorpusRawClean %>%
  anti_join(stop_words) 

#RemoveDigits
TidyCorpusRawClean <- TidyCorpusRawClean[-grep("\\b\\d+\\b", TidyCorpusRawClean$word),]

# Count again to see the difference
TidyCorpusRawClean %>% 
  group_by(word) %>% 
    count() %>% 
      arrange(desc(freq)) 
 

#With  Numbers
# word freq
# 1               8  422
# 2       education  359
# 3               9  340
# 4        refugees  340
# 5               1  301
# 6           youth  300

#Without Numbers
# word freq
# 1                     education  286
# 2                         youth  249
# 3                         youth  241
# 4                      refugees  237
# 5                       african  200

#Created a CSV 
#write.csv(TidyCorpusRawClean, "TidyCorpusRawClean.csv")

#Take 2:
# GREP
CorpusRawT2CollapsedClean <- CorpusRawT2Collapsed #Create a new one to have the middle point
CorpusRawT2CollapsedClean$TextDataCollapsed <- gsub("\n|\r|\t|\v|\f|•|·|~|-|[[:punct:]]
                                |ÿ| ÿ |  ÿ|ÿ  |ÿ5|ÿÿ|4ÿ|http
                                ", "", CorpusRawT2CollapsedClean$TextDataCollapsed)



print(CorpusRawT2CollapsedClean$TextDataCollapsed)

##############################################################
#################### The Document Term Matrix ################
##############################################################

# I COULD NOT MAKE IT! Using the TidyText 
# So I used the tm() package

#Creates corpus
TidyOrgCorpus <- Corpus(VectorSource(as.vector(TidyCorpusRawClean$word))) 
#Creates Document Term Matrix
OrgCorpus_DTM <- DocumentTermMatrix(TidyOrgCorpus, 
                                    control = list(wordLengths = c(2, Inf)))
#To see how the DTM looks like
inspect(OrgCorpus_DTM[1:25,3:8])
#The one below shoes the corpus into a tibble. I'm actually not sure what this is for
tidy(OrgCorpus_DTM)

##############################################################
######################## Word Counting ####################### 
##############################################################

#For word counting you need the TidyCorpusRawClean 
TopWordsTidyCorpusRawClean <- 
  TidyCorpusRawClean %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(desc(freq)) 

#But we have duplicates so we need to aggregated the duplicates.
#e.g., youth appears in two different rows

#This website helped
# https://stackoverflow.com/questions/10180132/consolidate-duplicate-rows
#Aggregated Duplicates. 
TidyCorpusRawCleanAggregated <- aggregate(freq~word, #formual of values ~ grouping
                                          data = TopWordsTidyCorpusRawClean , #data set
                                          FUN = sum) #function to apply to the formula




#Do not run again
#write.csv(TidyCorpusRawCleanAggregated, "TidyCorpusRawCleanAggregated.csv")

#Check Top Words now
TidyCorpusRawCleanAggregated %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(desc(freq)) 

TidyCorpusRawCleanAggregated %>% 
  arrange(desc(freq)) %>% 
  slice(1:20) %>% 
    ggplot(mapping = aes(x = reorder(word, -freq), y = freq, fill = word)) + #adding the mapping is better to avoid errors
      geom_bar(stat = "identity") + #You need the stat = "identity" to show geom_bar that you already have the counts in y. 
        theme_minimal() + #To delete the grey background
          theme(axis.title = element_text(angle = 90, hjust = 1, size = 13)) + #not working pretty
            theme(plot.title = element_text(hjust = 0.5, size=18))+
              ylab("Frequency")+
                xlab("")+
                  ggtitle("Most Frequent Words in PDF Text")+
                      guides(fill="none")
  

#theme(strip.text.x = element_text(angle = 45)) 

###############################################################
################### LDA Topic Modeling k =10 ##################
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
  ggtitle("PDF Text Topic Modeling k = 10")

###############################################################
################### LDA Topic Modeling k = 5 ##################
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
  ggtitle("PDF Text Topic Modeling k = 5")

###############################################################
################### LDA Topic Modeling k = 7 ##################
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
  ggtitle("PDF Text Topic Modeling k = 7")


###############################################################
################### LDA Topic Modeling k = 13 ##################
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
  top_n(7, beta) %>%  #take the top 10 based on the beta variable
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
  ggtitle("PDF Text Topic Modeling k = 13")


###############################################################
################### Structural Topic Modeling #################
###############################################################

