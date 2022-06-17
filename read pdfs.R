install.packages("pdftools")
install.packages("tesseract")
install.packages("purrr")
library(pdftools)
library(tesseract)
library(purrr)

getwd()
path <- "C:/Users/bonni/Desktop/practicepdfs"
setwd(path)

files <- list.files(path, pattern = ".pdf") 
View(files)


s_pdf_text <- safely(pdf_text) # helps catch errors

y<-walk(files, ~{                                     # iterate over the files
  
  res <- s_pdf_text(.x)                                # try to read it in
  if (!is.null(res$result)) {                          # if successful
    
    message(sprintf("Processing [%s]", .x))
    
    txt_file <- sprintf("%stxt", sub("pdf$", "", .x))  # make a new filename
    
    unlist(res$result) %>%                             # cld be > 1 pg (which makes a list)
      tolower() %>%                                    
      paste0(collapse="\n") %>%                        # make one big text block with line breaks
      cat(file=txt_file)                               # write it out
    
  } else {                                             # if not successful
    message(sprintf("Failure converting [%s]", .x))    # show a message
  }
  
})


setwd(path)

txt_files = list.files(path=path, pattern="*.txt") 
# Read the files in, assuming comma separator
txt_files_df <- lapply(txt_files, function(x) {read.table(file = x, header = T, sep =",")})
# Combine them
combined_df <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 


data <- 0

files <- list.files(path, pattern = ".txt")
View(files)

for (f in files) {
  
  tempData = scan( f, what="character")
  
  data <- c(data,tempData)    
  
} 

head(data)