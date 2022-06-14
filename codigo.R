


if(!require("rstudioapi")) {install.packages("rstudioapi"); library(rstudioapi)}
if(!require("openxlsx")) {install.packages("openxlsx"); library(openxlsx)}
if(!require("plotly")) {install.packages("plotly"); library(plotly)}
if(!require("BBmisc")) {install.packages("BBmisc"); library(BBmisc)}
if(!require("tidyr")) {install.packages("tidyr"); library(tidyr)}
# 
folder <- strsplit(getActiveDocumentContext()$path, "/")[[1]]
basedir_aux <- paste0(paste(folder[1:(length(folder)-1)], collapse="/"), "/")
setwd(basedir_aux)


source(file = 'ahptosis.R')


dados <- read.xlsx("Data.xlsx")
criterias <- read.xlsx("criteria.xlsx")
nomes <- read.xlsx("nomes.xlsx")
nomes <- nomes$nomes

criterias <- criterias[c(nomes)]
criteria <- as.matrix(criterias)


dados_cl <- dados[dados$Licenca==TRUE,]
dados_Sl <- dados[dados$Licenca==FALSE,]


# positivo, negativo, recursos, atualizacao
minmax<-c("max", "min", "max","min")


# sem licenca  ----------------------
decision <- dados_Sl[ c(nomes) ]
#decision <- as.data.frame(scale(decision))
decision <- as.matrix(decision)
rownames(decision) <- dados_Sl$codigo


# Com licenca
result1 <- ahptopsis2n(decision=decision, criteria=criteria, minmax=minmax)

r1 <- result1[[3]]
r1<-r1[order(r1$ranking),]
n_top <- nrow(r1)



r1_plot <- r1[c(1, round(n_top/2, digits = 0), n_top),]
selecionados <- rownames(r1_plot)
db_plot <- dados_Sl[dados_Sl$codigo %in% selecionados, ]
rank_plot <- r1
rank_plot$codigo <- rownames(r1)

Ranking_completo <- r1
Ranking_completo$codigo <- rownames(r1)
Ranking_completo <- merge(dados_Sl, Ranking_completo, by = "codigo" )

db_plot <- merge(db_plot, rank_plot, by = "codigo" )


#db_plot$Positivo <- scale(db_plot$Positivo)
#db_plot$Negativo <- scale(db_plot$Negativo)
#db_plot$Recursos <- scale(db_plot$Recursos)
db_plot$Atualizacao <- normalize(db_plot$Atualizacao, method = "standardize", range = c(0, 5), margin = 1L, on.constant = "quiet")
db_plot$codigo <- paste0(db_plot$ranking, ' - ', db_plot$codigo)
db_plot<-db_plot[order(db_plot$ranking),]
db_plot$Alternativas <- NULL
db_plot$Licenca <- NULL
db_plot$ranking <- NULL
db_plot$values <- NULL
vars <-  unique(db_plot$codigo)
cols <- colnames(db_plot)
db_plotc <- db_plot
db_plot <- gather(db_plot, "criterio", "valor", cols[2:length(cols)] )



fig <- plot_ly(
  type = 'scatterpolar',
  # mode = 'lines',
  fill = 'toself'
) 

for( v in length(vars):1) {
  
  name <- vars[v]
  
  db_plot_t <- db_plot[db_plot$codigo==name,]
  
  fig <- fig %>%
    add_trace(
      r = db_plot_t$valor,
      theta = db_plot_t$criterio,
      name = db_plot_t$codigo
    ) 
}


db_plot1 <- db_plot[db_plot$codigo=="1 - BD54",]
db_plot2 <- db_plot[db_plot$codigo=="12 - BD66",]
db_plot3 <- db_plot[db_plot$codigo=="26 - BD55",]

library(plotly)

fig <- plot_ly(
  type = 'scatterpolar',
  # mode = 'lines',
  fill = 'toself'
) 

fig <- fig %>%
  add_trace(
    r = db_plot3$valor,
    theta = db_plot3$criterio,
    name = db_plot3$codigo
  ) 

fig <- fig %>%
  add_trace(
    r = db_plot1$valor,
    theta = db_plot1$criterio,
    name = db_plot1$codigo
  ) 

fig <- fig %>%
  add_trace(
    r = db_plot2$valor,
    theta = db_plot2$criterio,
    name = db_plot2$codigo
  ) 



fig
htmlwidgets::saveWidget(as_widget(fig), "radar_SL.html")

View(Ranking_completo)


db_plotc <- Ranking_completo
db_plotc$Atualizacao <- normalize(db_plotc$Atualizacao, method = "range", range = c(0, 50), margin = 1L, on.constant = "quiet")
db_plotc$Positivo <- normalize(db_plotc$Positivo, method = "range", range = c(0, 50), margin = 1L, on.constant = "quiet")
db_plotc$Negativo <- normalize(db_plotc$Negativo, method = "range", range = c(0, 50), margin = 1L, on.constant = "quiet")
db_plotc$Recursos <- normalize(db_plotc$Recursos, method = "range", range = c(0, 50), margin = 1L, on.constant = "quiet")
db_plotc<-db_plotc[order(db_plotc$ranking),]
db_plotc <- as.matrix(db_plotc[c(nomes)])

pdf("heat_SL.pdf",         # File name
    width = 8, height = 7, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk" ,   # Color model (cmyk is required for most publications)
    paper = "A4")  

heatmap(db_plotc, scale="none",
        margins = c(7,5),
        cexRow= 1,
        cexCol = 1.5)
dev.off() 

htmlwidgets::saveWidget(as_widget(heat), "heat_SL.html")

write.xlsx(Ranking_completo, "Resultados_SL.xlsx")

# ---------------------------------


# Com licenca -------------------------


decision <- dados_cl[ c(nomes) ]
#decision <- as.data.frame(scale(decision))
decision <- as.matrix(decision)
rownames(decision) <- dados_cl$codigo


# Com licenca
result1 <- ahptopsis2n(decision=decision, criteria=criteria, minmax=minmax)

r1 <- result1[[3]]
r1<-r1[order(r1$ranking),]
n_top <- nrow(r1)



r1_plot <- r1[c(1, round(n_top/2, digits = 0), n_top),]
selecionados <- rownames(r1_plot)
db_plot <- dados_cl[dados_cl$codigo %in% selecionados, ]
rank_plot <- r1
rank_plot$codigo <- rownames(r1)

Ranking_completo <- r1
Ranking_completo$codigo <- rownames(r1)
Ranking_completo <- merge(dados_cl, Ranking_completo, by = "codigo" )

db_plot <- merge(db_plot, rank_plot, by = "codigo" )


# Normalizacao 
db_plot$Atualizacao <- normalize(db_plot$Atualizacao, method = "standardize", range = c(0, 5), margin = 1L, on.constant = "quiet")
db_plot$Recursos <- normalize(db_plot$Recursos, method = "standardize", range = c(0, 5), margin = 1L, on.constant = "quiet")

db_plot$codigo <- paste0(db_plot$ranking, ' - ', db_plot$codigo)
db_plot<-db_plot[order(db_plot$ranking),]
db_plot$Alternativas <- NULL
db_plot$Licenca <- NULL
db_plot$ranking <- NULL
db_plot$values <- NULL
vars <-  unique(db_plot$codigo)
cols <- colnames(db_plot)
db_plot <- gather(db_plot, "criterio", "valor", cols[2:length(cols)] )



fig <- plot_ly(
  type = 'scatterpolar',
  # mode = 'lines',
  fill = 'toself'
) 

for( v in length(vars):1) {
  
  name <- vars[v]
  
  db_plot_t <- db_plot[db_plot$codigo==name,]
  
  fig <- fig %>%
    add_trace(
      r = db_plot_t$valor,
      theta = db_plot_t$criterio,
      name = db_plot_t$codigo
    ) 
}

fig

htmlwidgets::saveWidget(as_widget(fig), "radar_CL.html")

db_plot1 <- db_plot[db_plot$codigo=="1 - BD54",]
db_plot2 <- db_plot[db_plot$codigo=="12 - BD66",]
db_plot3 <- db_plot[db_plot$codigo=="26 - BD55",]

library(plotly)

fig <- plot_ly(
  type = 'scatterpolar',
  # mode = 'lines',
  fill = 'toself'
) 

fig <- fig %>%
  add_trace(
    r = db_plot3$valor,
    theta = db_plot3$criterio,
    name = db_plot3$codigo
  ) 

fig <- fig %>%
  add_trace(
    r = db_plot1$valor,
    theta = db_plot1$criterio,
    name = db_plot1$codigo
  ) 

fig <- fig %>%
  add_trace(
    r = db_plot2$valor,
    theta = db_plot2$criterio,
    name = db_plot2$codigo
  ) 



db_plotc <- Ranking_completo
db_plotc$Atualizacao <- normalize(db_plotc$Atualizacao, method = "range", range = c(0, 50), margin = 1L, on.constant = "quiet")
db_plotc$Positivo <- normalize(db_plotc$Positivo, method = "range", range = c(0, 50), margin = 1L, on.constant = "quiet")
db_plotc$Negativo <- normalize(db_plotc$Negativo, method = "range", range = c(0, 50), margin = 1L, on.constant = "quiet")
db_plotc$Recursos <- normalize(db_plotc$Recursos, method = "range", range = c(0, 50), margin = 1L, on.constant = "quiet")
db_plotc<-db_plotc[order(db_plotc$ranking),]
db_plotc <- as.matrix(db_plotc[c(nomes)])

pdf("heat_CL.pdf",         # File name
    width = 8, height = 7, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk" ,   # Color model (cmyk is required for most publications)
    paper = "A4")  


heatmap(db_plotc, scale="none",
        margins = c(7,5),
        cexRow= 1,
        cexCol = 1.5)
dev.off() 


write.xlsx(Ranking_completo, "Resultados_CL.xlsx")
# -------------------------

View(Ranking_completo)