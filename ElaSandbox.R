#load packages
pacman::p_load(
  tidyverse,
  pdftools, 
  tesseract,
  tidytext, 
  plyr,
  stringr,
  stringi,
  tm
)

##############################################################
###################### Text from PDF #########################
##############################################################

setwd("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data")
getwd()

#Trial 1: https://alexluscombe.ca/blog/getting-your-.pdfs-into-r/
#Vector of pdf file names 
file_vector <- list.files(path = "/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data")

results <- vlapply(file_vector, pdf_text)
#Error in poppler_pdf_text(loadfile(pdf), opw, upw) : PDF parsing failure.

#Trial 2: https://stackoverflow.com/questions/21445659/use-r-to-convert-pdf-files-to-text-files-for-text-mining

#folder with pdfs
dest <- "/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data"
# make a vector of PDF file names
myfiles <- list.files(path = dest, pattern = "pdf", full.names = TRUE)
myfiles[1]

print(myfiles)

##### Checking if each of them works#####
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

#  Trial 3 Loop was unsuccessful becasue it was rewriting data

for (i in 1:length(myfiles)){
  x <- pdf_text(myfiles[i], opw = "", upw = "") 
  document <- data.frame("org" = gsub(x =myfiles[i],pattern = ".pdf", replacement = ""), 
             "text" = x, stringsAsFactors = FALSE)
  colnames(document) <- c("org", "text")
  corpus_raw <- rbind(corpus_raw,document)
}
print(x)

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

# Trial 4 I'm using the code in this website
#https://stackoverflow.com/questions/59519032/how-do-i-get-my-loop-on-pdf-text-only-to-read-all-the-files

myfiles_list <- list() #Created this object to put the output of the loop

for (i in 1:length(myfiles)) {
  print(i) #to make sure the loop is working
  myfiles_list[myfiles[[i]]] <- pdf_text(myfiles[i]) %>% # This add the output of pdf_text() to the list object. 
  tibble() # txt = . I believe this converts to a tibble (another way to say data.frame) and makes the output be txt format
}

View(myfiles_list)




#Saving doc
getwd()
#This does not work anymore
#Do not run anymore. This is only for demosntation purposes on how it went wrong. 
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
#################### Two Column Corpus #######################
##############################################################

# I'm going to transform the nested data into a regular data frame. 


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
                 plyr::rename(., c(".id" = "File", #Need to use plyr:: to avoid issyes with dplyr package
                                   "X..i.." = "TextData")) 

#Do not Run again
#write_csv(CorpusRaw, "CorpusRaw.csv")











#######################################################
############# Corpus with Chris Bail Instructions #####
#######################################################

# GREP
CorpusRawClean <- CorpusRaw #Create a new one to have the middle point
CorpusRawClean$TextData <- gsub("\n|\r|\t|\v|\f|•|·|~|-|[[:punct:]]", "", CorpusRawClean$TextData)

# Creating  a Corpus
library(tm)
OrgsCorpus <- Corpus(VectorSource(as.vector(CorpusRawClean$TextData))) 
OrgsCorpus

# TidyText

