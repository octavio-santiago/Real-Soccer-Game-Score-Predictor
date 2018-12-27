#Octavio Santiago
library(h2o)
library(plyr)
h2o.init()
library(dplyr)

#create GLM model
mydata <- read.csv("CartolaFC Predict/Datasets/2017_partidas.csv", header=TRUE, 
                   sep=",")

matchResults <- data.frame("resultado" = ifelse(mydata$placar_oficial_mandante > mydata$placar_oficial_visitante,"v", ifelse(mydata$placar_oficial_mandante == mydata$placar_oficial_visitante,"e","d")))
scouts <- read.csv("CartolaFC Predict/Datasets/2017_scouts.csv", header=TRUE, 
                   sep=",")

#somar as pontuacoes dos jogadores por area no campo a cada rodada
#se rodada for igual, clube for igual, e statusid for de defesa, coluna def soma pontos, preco, variacao, media
#gerar os dados de defesa,ataque e tec de cada time
t0 <- data.frame("rodada"=0 ,"clube_id"=0,"cDefPonto"=0,"cDefPreco"=0,"cDefVar"=0,"cDefMedia"=0,
                 "cAtaPonto"=0,"cAtaPreco"=0,"cAtaVar"=0,"cAtaMedia"=0,
                 "cTecPonto"=0,"cTecPreco"=0,"cTecVar"=0,"cTecMedia"=0)
for(i in 1:19){
  for(j in 262:373){
    t1 <- filter(scouts,rodada_id==i & clube_id==j & (posicao_id == 1 |posicao_id == 2 |posicao_id == 3))
    t2 <- filter(scouts,rodada_id==i & clube_id==j & (posicao_id == 4 |posicao_id == 5))
    t3 <- filter(scouts,rodada_id==i & clube_id==j & (posicao_id == 6))
    t6 <- data.frame("rodada"=t1$rodada_id[1] ,"clube_id"=t1$clube_id[1],"cDefPonto"=sum(t1$pontos_num),"cDefPreco"=sum(t1$preco_num),"cDefVar"=sum(t1$variacao_num),"cDefMedia"=sum(t1$media_num),
                     "cAtaPonto"=sum(t2$pontos_num),"cAtaPreco"=sum(t2$preco_num),"cAtaVar"=sum(t2$variacao_num),"cAtaMedia"=sum(t2$media_num),
                     "cTecPonto"=sum(t3$pontos_num),"cTecPreco"=sum(t3$preco_num),"cTecVar"=sum(t3$variacao_num),"cTecMedia"=sum(t3$media_num))
    t0<-rbind(t0,t6)
  }
  
}
teste <- filter(t0,!is.na(rodada))

#pegar os stats para as equipes em cada partida
b0 <- data.frame("rodada"=0,"clube_casa_id"=0,"casaDef"= 0,"casaDefMedia"= 0,"casaDefPreco"=0,"casaDefVar"=0,
                 "casaAtk"= 0,"casaAtkMedia"= 0,"casaAtkPreco"=0,"casaAtkVar"=0,
                 "casaTec"= 0,"casaTecMedia"= 0,"casaTecPreco"=0,"casaTecVar"=0,
                 "visitanteDef"= 0,"visitanteDefMedia"= 0,"visitanteDefPreco"=0,"visitanteDefVar"=0,
                 "visitanteAtk"= 0,"visitanteAtkMedia"= 0,"visitanteAtkPreco"=0,"visitanteAtkVar"=0,
                 "visitanteTec"= 0,"visitanteTecMedia"= 0,"visitanteTecPreco"=0,"visitanteTecVar"=0)
dataset <- mydata
for(i in 1:19){
  b1 <- filter(mydata,rodada_id==i)
  for(j in 1:length(b1$clube_casa_id)){
    casa <- filter(teste,rodada==i,clube_id==b1$clube_casa_id[j])
    visitante <-filter(teste,rodada==i,clube_id==b1$clube_visitante_id[j])
    b5 <- data.frame("rodada"=b1$rodada[j],"clube_casa_id"=b1$clube_casa_id[j],"casaDef"= casa$cDefPonto,"casaDefMedia"= casa$cDefMedia,"casaDefPreco"=casa$cDefPreco,"casaDefVar"=casa$cDefVar,
                     "casaAtk"= casa$cAtaPonto,"casaAtkMedia"= casa$cAtaMedia,"casaAtkPreco"=casa$cAtaPreco,"casaAtkVar"=casa$cAtaVar,
                     "casaTec"= casa$cTecPonto,"casaTecMedia"= casa$cTecMedia,"casaTecPreco"=casa$cTecPreco,"casaTecVar"=casa$cTecVar,
                     "visitanteDef"= visitante$cDefPonto,"visitanteDefMedia"= visitante$cDefMedia,"visitanteDefPreco"=visitante$cDefPreco,"visitanteDefVar"=visitante$cDefVar,
                     "visitanteAtk"= visitante$cAtaPonto,"visitanteAtkMedia"= visitante$cAtaMedia,"visitanteAtkPreco"=visitante$cAtaPreco,"visitanteAtkVar"=visitante$cAtaVar,
                     "visitanteTec"= visitante$cTecPonto,"visitanteTecMedia"= visitante$cTecMedia,"visitanteTecPreco"=visitante$cTecPreco,"visitanteTecVar"=visitante$cTecVar
    )
    b0<-rbind(b0,b5)
  }
}

#fazer um join com a tabela original
datasetFinal <- merge(mydata,b0,by.x=c("rodada_id","clube_casa_id"),by.y=c("rodada","clube_casa_id"))

#combine os datasets
matchResults <- data.frame("resultado" = ifelse(datasetFinal$placar_oficial_mandante > datasetFinal$placar_oficial_visitante,"v", ifelse(datasetFinal$placar_oficial_mandante == datasetFinal$placar_oficial_visitante,"e","d")))
matchResults <- as.h2o(matchResults)
#partidas <- h2o.cbind(partidas, matchResults)
partidas <- as.h2o(datasetFinal)
partidas <- h2o.cbind(partidas, matchResults)
# convert response column to a factor
partidas["resultado"] <- as.factor(partidas["resultado"])

# set the predictor names and the response column name
predictors <- c("clube_casa_posicao","clube_visitante_posicao","clube_casa_id","clube_visitante_id",
                "casaDef","casaDefMedia", "casaDefPreco", "casaDefVar", "casaAtk","casaAtkMedia","casaAtkPreco", "casaAtkVar", "casaTec",
                "casaTecMedia", "casaTecPreco","casaTecVar", "visitanteDef", "visitanteDefMedia", "visitanteDefPreco", "visitanteDefVar",
                "visitanteAtk", "visitanteAtkMedia", "visitanteAtkPreco", "visitanteAtkVar", "visitanteTec", "visitanteTecMedia", "visitanteTecPreco",
                "visitanteTecVar")
response <- "resultado"

# split into train and validation sets
partidas.split <- h2o.splitFrame(data = partidas,ratios = 0.80, seed = 1234)
train <- partidas.split[[1]]
valid <- partidas.split[[2]]

#modelo GLM
partidas_glm <- h2o.glm(family= "multinomial", x= predictors, y=response, training_frame=train)

h2o.confusionMatrix(partidas_glm)
h2o.performance(partidas_glm, newdata = valid, train = FALSE, valid = TRUE,
                xval = FALSE)
h2o.scoreHistory(partidas_glm)
h2o.varimp(partidas_glm)

#predict
predict(partidas_glm,valid)
x <- data.frame("clube_casa_posicao"=2,"clube_visitante_posicao"=16,"clube_casa_id"=262,"clube_visitante_id"=265,
                "casaDef"=37.7,"casaDefMedia"=37.7, "casaDefPreco"=0, "casaDefVar"=0, "casaAtk"=37.6,"casaAtkMedia"=37.6,"casaAtkPreco"=0, "casaAtkVar"=0, "casaTec"=6.33,
                "casaTecMedia"=6.33, "casaTecPreco"=0,"casaTecVar"=0, "visitanteDef"=38.7, "visitanteDefMedia"=38.7, "visitanteDefPreco"=0, "visitanteDefVar"=0,
                "visitanteAtk"=40.4, "visitanteAtkMedia"=40.4, "visitanteAtkPreco"=0, "visitanteAtkVar"=0, "visitanteTec"=7.19, "visitanteTecMedia"=7.19, "visitanteTecPreco"=0,
                "visitanteTecVar"=0)
#x <- data.frame("clube_casa_posicao" = c(2), "clube_visitante_posicao" = c(16), "clube_casa_id" = c(262),"clube_visitante_id"=c(267))
newdata <- as.h2o(x)
predict(partidas_glm, newdata)


#montando o dataset de 2016 para treino e teste
#create GLM model
mydata <- read.csv("CartolaFC Predict/Real-Soccer-Game-Score-Predictor/Datasets/2016_partidas.csv", header=TRUE, 
                   sep=",")

matchResults <- data.frame("resultado" = ifelse(mydata$placar_oficial_mandante > mydata$placar_oficial_visitante,"v", ifelse(mydata$placar_oficial_mandante == mydata$placar_oficial_visitante,"e","d")))
scouts <- read.csv("CartolaFC Predict/Real-Soccer-Game-Score-Predictor/Datasets/2016_scouts.csv", header=TRUE, 
                   sep=",")

#somar as pontuacoes dos jogadores por area no campo a cada rodada
#se rodada for igual, clube for igual, e statusid for de defesa, coluna def soma pontos, preco, variacao, media
#gerar os dados de defesa,ataque e tec de cada time
t0 <- data.frame("rodada"=0 ,"clube_id"=0,"cDefPonto"=0,"cDefPreco"=0,"cDefVar"=0,"cDefMedia"=0,
                 "cAtaPonto"=0,"cAtaPreco"=0,"cAtaVar"=0,"cAtaMedia"=0,
                 "cTecPonto"=0,"cTecPreco"=0,"cTecVar"=0,"cTecMedia"=0)
for(i in 1:38){
  print(i)
  for(j in 262:373){
    rodadaTimes <- filter(scouts,rodada_id==i & clube_id==j)
    #print(rodadaTimes)
    if(length(rodadaTimes$atleta_id)!=0){
      t1 <- filter(scouts,rodada_id==i & clube_id==j & (posicao_id == 1 |posicao_id == 2 |posicao_id == 3))
      t2 <- filter(scouts,rodada_id==i & clube_id==j & (posicao_id == 4 |posicao_id == 5))
      t3 <- filter(scouts,rodada_id==i & clube_id==j & (posicao_id == 6))
      t6 <- data.frame("rodada"=t1$rodada_id[1] ,"clube_id"=t1$clube_id[1],"cDefPonto"=sum(t1$pontos_num),"cDefPreco"=sum(t1$preco_num),"cDefVar"=sum(t1$variacao_num),"cDefMedia"=sum(t1$media_num),
                       "cAtaPonto"=sum(t2$pontos_num),"cAtaPreco"=sum(t2$preco_num),"cAtaVar"=sum(t2$variacao_num),"cAtaMedia"=sum(t2$media_num),
                       "cTecPonto"=sum(t3$pontos_num),"cTecPreco"=sum(t3$preco_num),"cTecVar"=sum(t3$variacao_num),"cTecMedia"=sum(t3$media_num))
      t0<-rbind(t0,t6)
    }
    
  }
  
}
teste <- t0
#teste <- filter(t0,!is.na(rodada))

#pegar os stats para as equipes em cada partida
b0 <- data.frame("rodada"=0,"clube_casa_id"=0,"casaDef"= 0,"casaDefMedia"= 0,"casaDefPreco"=0,"casaDefVar"=0,
                 "casaAtk"= 0,"casaAtkMedia"= 0,"casaAtkPreco"=0,"casaAtkVar"=0,
                 "casaTec"= 0,"casaTecMedia"= 0,"casaTecPreco"=0,"casaTecVar"=0,
                 "visitanteDef"= 0,"visitanteDefMedia"= 0,"visitanteDefPreco"=0,"visitanteDefVar"=0,
                 "visitanteAtk"= 0,"visitanteAtkMedia"= 0,"visitanteAtkPreco"=0,"visitanteAtkVar"=0,
                 "visitanteTec"= 0,"visitanteTecMedia"= 0,"visitanteTecPreco"=0,"visitanteTecVar"=0)
dataset <- mydata
for(i in 1:38){
  b1 <- filter(mydata,rodada==i)
  for(j in 1:length(b1$clube_casa_id)){
    #print(j)
    casa <- filter(teste,rodada==i,clube_id==b1$clube_casa_id[j])
    visitante <-filter(teste,rodada==i,clube_id==b1$clube_visitante_id[j])
    b5 <- data.frame("rodada"=b1$rodada[j],"clube_casa_id"=b1$clube_casa_id[j],"casaDef"= casa$cDefPonto,"casaDefMedia"= casa$cDefMedia,"casaDefPreco"=casa$cDefPreco,"casaDefVar"=casa$cDefVar,
                     "casaAtk"= casa$cAtaPonto,"casaAtkMedia"= casa$cAtaMedia,"casaAtkPreco"=casa$cAtaPreco,"casaAtkVar"=casa$cAtaVar,
                     "casaTec"= casa$cTecPonto,"casaTecMedia"= casa$cTecMedia,"casaTecPreco"=casa$cTecPreco,"casaTecVar"=casa$cTecVar,
                     "visitanteDef"= visitante$cDefPonto,"visitanteDefMedia"= visitante$cDefMedia,"visitanteDefPreco"=visitante$cDefPreco,"visitanteDefVar"=visitante$cDefVar,
                     "visitanteAtk"= visitante$cAtaPonto,"visitanteAtkMedia"= visitante$cAtaMedia,"visitanteAtkPreco"=visitante$cAtaPreco,"visitanteAtkVar"=visitante$cAtaVar,
                     "visitanteTec"= visitante$cTecPonto,"visitanteTecMedia"= visitante$cTecMedia,"visitanteTecPreco"=visitante$cTecPreco,"visitanteTecVar"=visitante$cTecVar
    )
    b0<-rbind(b0,b5)
  }
}

#fazer um join com a tabela original
datasetFinal2 <- merge(mydata,b0,by.x=c("rodada","clube_casa_id"),by.y=c("rodada","clube_casa_id"))

#combine os datasets
matchResults <- data.frame("resultado" = ifelse(datasetFinal2$placar_oficial_mandante > datasetFinal2$placar_oficial_visitante,"v", ifelse(datasetFinal2$placar_oficial_mandante == datasetFinal2$placar_oficial_visitante,"e","d")))
matchResults2 <- as.h2o(matchResults)
#partidas <- h2o.cbind(partidas, matchResults)
partidas2 <- as.h2o(datasetFinal2)
partidas2 <- h2o.cbind(partidas2, matchResults2)
# convert response column to a factor
partidas2["resultado"] <- as.factor(partidas2["resultado"])

#predizendo e validando com dados de 2016
predict(partidas_glm, partidas2)
h2o.performance(partidas_glm, newdata = partidas2, train = FALSE, valid = TRUE,
                xval = FALSE)
