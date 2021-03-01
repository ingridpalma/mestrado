#Load library
library(pdftools)
keyword<-read.table("./Keywords/keywords key terms.txt",header = TRUE,stringsAsFactors = FALSE)
# Download PDF
raw_list<-scan("./PDF/PDF.txt",what=character())
#Filter link
raw_list<-grep("https:*", raw_list, value = TRUE)
# Quitar los que no son pdf
raw_list<-grep("*.pdf", raw_list, value = TRUE)
# Descargar files
for (url in raw_list){ download.file(url, destfile = paste0("./PDF/",basename(url)), mode = "wb") }

archivos<-list.files("./PDF/",pattern = "*.pdf",full.names = TRUE)
out<-as.list(rep(0,nrow(keyword)))
names(out)<-keyword$keywords


txt<-pdf_text(archivos[i])
# deleting punctuations
txt<-gsub("[[:punct:][:blank:]]+", " ", txt)
# deleting trailing space
txt<-gsub("\\n"," ", txt)
txt<-gsub("\\r"," ", txt)
split <- strsplit(txt," ")[[i]]     
#comparing split with keyword   
state <- match(split, keyword)    
#if it matches, get the position of each keyword  
state <- which(!is.na(state))    
#extract the keyword based on the position  
state_split <- split[state]    
