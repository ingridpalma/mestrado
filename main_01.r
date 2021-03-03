####1. Carregar a library####
library(pdftools)
library(ggplot2)
library(tidyverse)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(quanteda)
library(textrecipes)
library(tokenizers)
library(dplyr)
library(tidytext)
library(janeaustenr)

#1.1 setwd("C:/Users/nando/Dropbox/MRX/Amavisse")
keyword<-read.table("./Keywords/keywords key terms.txt",header = TRUE,stringsAsFactors = FALSE)
#1.2 Download PDF
raw_list<-scan("./PDF/PDF.txt",what=character())
#1.3 Filtrar link
raw_list<-grep("https:*", raw_list, value = TRUE)
#1.4 Tirar os que nÃ£o sÃ£o pdf
raw_list<-grep("*.pdf", raw_list, value = TRUE)
#1.5 Descarregar os files
for (url in raw_list){ download.file(url, destfile = paste0("./PDF/",basename(url)), mode = "wb") }

archivos<-list.files("./PDF/",pattern = "*.pdf",full.names = TRUE)

anos<-c(2030,2006,2008,2005,2007,2019,2018,2017,2009,2010,2011,2012,2013,2014,2015,2016,2050)

pdf_files<-list()
for (i in 1:length(archivos)){    
  txt<-pdf_text(archivos[i])
  pdf_files[[i]] <-paste0(unlist(txt),collapse =" ")
  
}

names(pdf_files)<-anos

datos<-data.frame(archivo=anos,texto=unlist(pdf_files))

datos<-as_tibble(datos)
####2. Limpeza de texto e tokenizaÃ§Ã£o####
limpiar_tokenizar <- function(texto){
  #2.1 Ordem da limpeza não é arbitrário
  # todo o texto é convertido para minúsculas
  nuevo_texto <- tolower(texto)
  #2.2 Remoção de páginas da web (palavras que começam com "http." Em uma linha
  # por qualquer coisa diferente de um espaço)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  #2.3 Remoção de sinais de pontuação
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  #2.4 Eliminação de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  #2.5 Eliminacão dos espacios em branco multiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  #2.6 Tokenização por palavras individuais
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  #2.7 Remoção de tokens com comprimento <2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  
  nuevo_texto<-paste(nuevo_texto,collapse = " ")
  return(nuevo_texto)
}


datos<-datos%>% mutate(texto = map(.x = texto,
                                   .f = limpiar_tokenizar))




datos<-datos %>% unnest_tokens(texto_tokenizado, texto, token = "ngrams", n = 10, n_min = 1)






#2.8 Tokenizar KEYWORDS 
key_words<-list()
for (j in 1:nrow(keyword)){    
  txt<-keyword[j,]
  # deletando pontuaÃ§Ãµes
  key_words[[j]] <- limpiar_tokenizar(txt)
  
}

####3. AnÃ¡lise ExploratÃ³ria####
datos_tidy <- datos 
datos_tidy <- datos_tidy %>% rename(token = texto_tokenizado)

#3.2 Tirar Stop words e filtrar as keywords

lista_stopwords <-stopwords("portuguese")   
datos_tidy <- datos_tidy %>% filter(!(token %in% lista_stopwords))

datos_tidy <- datos_tidy %>% filter((token %in% unique(unlist(key_words))))


#3.3 Frequencia de palavras 

datos_tidy %>% group_by(archivo) %>% summarise(n = n())

datos_tidy %>%  ggplot(aes(x = archivo)) + geom_bar() + coord_flip() + theme_bw() 


#3.4 Palavras diferentes usadas por arquivo
datos_tidy %>% select(archivo, token) %>% distinct() %>%  group_by(archivo) %>%
  summarise(palabras_distintas = n()) 
datos_tidy %>% select(archivo, token) %>% distinct() %>%
  ggplot(aes(x = archivo)) + geom_bar() + coord_flip() + theme_bw()

#3.5 Palavras mais usadas pelo usuÃ¡rio
datos_tidy %>% group_by(archivo, token) %>% count(token) %>% group_by(archivo) %>%
  top_n(10, n) %>% arrange(archivo, desc(n)) %>% print(n=30)


####4. word cloud####




wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 400, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}

##Pimer grupo
grupo1<-c(2030,2050)

png(filename = "2030_2050.png")
df_grouped<- datos_tidy %>% filter(archivo%in%grupo1)

df_grouped <- df_grouped  %>% group_by( token) %>% count(token) %>%
    mutate(frecuencia = n / n()) %>% arrange(desc(frecuencia))
wordcloud_custom(grupo1,df=df_grouped)
dev.off()

#todos
png(filename = "todos_anos.png")
df_grouped<- datos_tidy
df_grouped <- df_grouped  %>% group_by( token) %>% count(token) %>%
  mutate(frecuencia = n / n()) %>% arrange(desc(frecuencia))
wordcloud_custom(grupo1,df=df_grouped)
dev.off()

#todos separados
df_grouped <- datos_tidy  %>% group_by(archivo, token) %>% count(token) %>%
  group_by(archivo) %>% mutate(frecuencia = n / n()) %>%
  arrange(archivo, desc(frecuencia)) %>% nest()

for(i in 1:nrow(df_grouped))
{
  png(filename = paste0(df_grouped$archivo[i],".png"))
  wordcloud_custom(grupo=df_grouped$archivo[i],df=df_grouped$data[i][[1]])
  dev.off()
}


