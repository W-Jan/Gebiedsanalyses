library(RODBC)
library(ggplot2)
library(plyr)
library(stringr)
library(dplyr)
#library (reshape2)

# setwd("//Inbodata/indata/Projects/PRJ_Referentiewaarden/160_Dataverwerking&Resultaten_opgekuist/OverzichtRapportage/RWerk/Biotischecriteria/WorkingDirectory/update201610/niche")

# setwd("C:/Users/webja/Google Drive/Grenswaarden/updata201610_thuis")
Thuis <- "Nee"
if (Thuis == "Nee"){
  setwd("C:/Users/jan_wouters/GoogleDrive/Gebiedsanalysen/R/temp")
  dbWatinaToepassing <- "C:/Users/jan_wouters/GoogleDrive/Grondwater/Toepassingen/watina_toepassing.mdb"
  verbinding <- odbcConnectAccess2007(dbWatinaToepassing)
}else {
  setwd("C:/Users/webja/GoogleDrive/Gebiedsanalysen/R/temp")
  dbWatinaToepassing <- "C:/Users/webja/GoogleDrive/Grondwater/Toepassingen/watina_toepassing.mdb"
  verbinding <- odbcConnect("WatinaToepassing", uid = "", pwd = "")
}

Dataset <- sqlFetch(verbinding, "tbl_WatinaDeelzones") 
dataselectie <- subset(Dataset, Gem_Totaal <=0 & !is.na(GemAmplitude_Totaal) )
dataselectie$Deelzone <- factor(dataselectie$Deelzone)
str(Dataset)
close(verbinding)

basisstatistiek <- function (j, VarWaarde, MinAantal){
  
  aantal <- nrow(j)
  if (aantal >=MinAantal){
    meetwaarden <- j[,c(VarWaarde)]
    gem <- mean(meetwaarden, na.rm = TRUE)
    med <- median(meetwaarden, na.rm = TRUE)
    sdev <- sd(meetwaarden, na.rm = TRUE)
    min <- min(meetwaarden, na.rm = TRUE)
    max <- max(meetwaarden, na.rm = TRUE)
    q10 <- quantile (meetwaarden, 0.1, na.rm = TRUE, type =8)
    q90 <- quantile (meetwaarden, 0.9, na.rm = TRUE, type =8)
    q25 <- quantile (meetwaarden, 0.25, na.rm = TRUE, type =8)
    q75 <- quantile (meetwaarden, 0.75, na.rm = TRUE, type =8)    
    
  } else {
    gem <- NA
    med <- NA
    sdev <- NA
    min <- NA
    max <- NA
    q10 <- NA
    q90 <- NA    
    q25 <- NA
    q75 <- NA
  }
  c(Aantal = aantal, Gemiddelde = gem, Mediaan = med, Standaardafw = sdev, Minimum = min, Maximum = max, q10, q90, q25, q75)
}

samenvatting <- ddply (dataselectie, .(Deelzone), basisstatistiek, VarWaarde = "Gem_Totaal", MinAantal = 3)
t <- dataselectie[order(dataselectie$Gem_Totaal),]

poging2 <- dataselectie  %>% arrange(Deelzone, Gem_Totaal) %>% group_by(Deelzone) %>% mutate(rank = row_number(), percrank=rank(Gem_Totaal)/length(Gem_Totaal))

deelzones <- unique(poging2$Deelzone)
ranking_meetpunten  <- data.frame(Deelzone= unique(poging2$Deelzone), Rang = 1)

for( i in seq(from=2, to= 6)){
  t <- data.frame(Deelzone= unique(poging2$Deelzone), Rang =i)
  ranking_meetpunten <- rbind (ranking_meetpunten,t)
}

ranking_meetpunten$Meetpunt <- "-"
rang <- c(0, 0.2, 0.4, 0.6, 0.8, 1)

#per deelzone

for (i in deelzones){

  verschil <- rep(1,6)

  deelzone <- as.character(i)    
  t <- subset(poging2, Deelzone == deelzone )

  for (j in seq_len(6)){

      tverschil <- c(1)
      for(meetpunt in seq_len(nrow(t))){
                
        tverschil <- abs(t$percrank[meetpunt] - rang[j])

        if(tverschil< verschil[j]) {
          verschil[j] <- tverschil  
          ranking_meetpunten[ranking_meetpunten$Deelzone %in% c(deelzone) & ranking_meetpunten$Rang == j,]$Meetpunt <- as.character(t$Meetpunt[meetpunt])


        } else {
          
        }
      }
    }
  
}
# check <- subset(poging2, GebiedCode =="ZON")
write.csv2(ranking_meetpunten,"Ranking_meetpunten_per_deelzone.csv")
if (Thuis == "Nee"){
  verbinding <- odbcConnectAccess2007(dbWatinaToepassing)
}else {
  verbinding <- odbcConnect("WatinaToepassing", uid = "", pwd = "")
}
sqlSave(verbinding, ranking_meetpunten, "tbl_ranking_meetpunten", safer = FALSE )
# sqlSave(verbinding, boxdata, "boxplot", safer = FALSE )
close(verbinding)
