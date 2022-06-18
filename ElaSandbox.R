#load packages
pacman::p_load(
  tidyverse,
  pdftools, 
  tesseract
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

pdf_text("/Users/Ela 1/Documents/LDT Mac/7. ProfessionalDevelopment/SICSS_Project/SICSS-Project-African-Youth-Leadership/Webscraped data/AYLS143_AfricanYouthCouncilonClimateChange.pdf")

#There was a corrupt file!!! AYLS038_ColumbaLeadership.pdf

corpus_raw <- data.frame("org" = c(),"text" = c())

#Successful Loop 1 !!!
for (i in 1:length(myfiles)){
  x <- pdf_text(myfiles[i], opw = "", upw = "") 
  document <- data.frame("org" = gsub(x =myfiles[i],pattern = ".pdf", replacement = ""), 
             "text" = x, stringsAsFactors = FALSE)
  colnames(document) <- c("org", "text")
  corpus_raw <- rbind(corpus_raw,document)
}
print(x)

#Successful Loop 2 organizing names 
for (i in 1:length(myfiles)){
  rawtext <- pdf_text(myfiles[i], opw = "", upw = "") 
  document <- data.frame( #Creates new data frame
                        "org" = gsub(x =myfiles[i],pattern = ".pdf", replacement = ""), #This uses the gsub function of the regular expressions in R
                         "text" = x, stringsAsFactors = FALSE
                        )
  colnames(document) <- c("org", "text")
  corpus_raw <- rbind(corpus_raw,document)
}


#Trial with own pdfs
dest_latinx <- "/Users/Ela 1/Documents/LDT Mac/1. Research_2/Latinx in Academia/Dr. Masta readings"
myfiles_latinx <- list.files(path = dest_latinx, pattern = "pdf", full.names = TRUE)
print(myfiles_latinx)

for (i in 1:length(myfiles_latinx)){
  y <- pdf_text(myfiles_latinx[i], opw = "", upw = "")
}

print(y)

##############################################################
#################### Cleaning Corpus #########################
##############################################################



