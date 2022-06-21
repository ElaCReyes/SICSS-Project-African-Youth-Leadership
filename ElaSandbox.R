#load packages
pacman::p_load(
  tidyverse,
  pdftools, 
  tesseract,
  tidytext
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

#Successful Loop 1 !!!

for (i in 1:length(myfiles)){
  x <- pdf_text(myfiles[i], opw = "", upw = "") 
  document <- data.frame("org" = gsub(x =myfiles[i],pattern = ".pdf", replacement = ""), 
             "text" = x, stringsAsFactors = FALSE)
  colnames(document) <- c("org", "text")
  corpus_raw <- rbind(corpus_raw,document)
}
print(x)



####Successful Loop 2 organizing names!!!!! 
for (i in 1:length(myfiles)){
  rawtext <- pdf_text(myfiles[i], opw = "", upw = "") 
  document <- data.frame( #Creates new data frame
                        "org" = gsub(x =myfiles[i],pattern = ".pdf", replacement = ""), #This uses the gsub function of the regular expressions in R
                         "text" = x, stringsAsFactors = FALSE
                        )
  colnames(document) <- c("org", "text")
  corpus_raw <- rbind(corpus_raw,document)
}




#Saving doc
getwd()
#write.csv(corpus_raw, file = "corpus_raw_trial.csv", row.names = FALSE)

#Trial with own pdfs
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

# I want to use the function unite(). 
# First, I'm going to use the function pivot_wider().
# Then, I'll use pivot_wider


# I learned that I have problems in my first code :''''(

uniquetrial <- unique(corpus_raw$text)

corpus_raw %>% 
    pivot_wider(names_from = org, values_from = text)



corpus_raw %>%
  dplyr::group_by(org) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)



id_cols = FilePDF, id_expand = TextData, 

fish_encounters
fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen)




