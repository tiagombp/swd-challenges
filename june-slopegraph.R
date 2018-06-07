library(tidyverse)
library(readxl)
library(scales)
library(ggrepel)
library(ipeaData)

setwd("C:/Users/tiago.pereira/GitHub/swd-challenges")
data <- read_excel("despesasBrasil2008a2017.xlsx", skip = 9)

# lê série do PIB do IPEA

pibs <- ipeadata("BM12_PIBAC12")
pibs <- pibs[MES=="12" & ANO %in% (2008:2017)]$VALVALOR
pibs <- c(pibs,6559940) # acrescentando 2017, que ainda não estava lá

names(pibs) <- as.character(c(2008:2017))

colnames(data)<- c("Categoria", names(pibs))

# dividindo pelo PIB

for (i in 2:length(data)) {
  data[,i] <- data[,i] / (pibs[colnames(data[,i])]*1000000)
}

# colnames(data)[1]<-"Categoria"
# data$`2008` <- data$`DEZ/2008` / (pibs["2008"]*1000000)
# data$`2017` <- data$`DEZ/2017` / (pibs["2017"]*1000000)

data_semdiv <- data[1:8,c("Categoria", "2008", "2017")]

data_semdiv$Categories <- c("Personnel","Healthcare Costs","Education Costs","Welfare Costs","Administrative Costs","Social Benefits","Investments","Transfers to States and Municipalities")

labels_2008 <- paste(data_semdiv$Categories,percent(data_semdiv$`2008`)," ",sep="   ")
labels_2017 <- paste(" ",percent(data_semdiv$`2017`), data_semdiv$Categories,sep="   ")


library(extrafont)
#font_import()
loadfonts(device = "win")

theme_slope <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Roboto Condensed Light"), #Cambria? #"Source Sans Pro"
      title = element_text(colour = "gray25", face = "bold", family = "Roboto"),
      plot.subtitle = element_text(face = "bold", family = "Roboto", color = "#B90000"),
      plot.caption = element_text(colour = "gray50", face = "plain", family = "Roboto Condensed Light"),
      strip.background = element_rect(fill = "gray95", color = "gray95"),
      panel.background = element_blank(),
      panel.grid=element_blank(),
      axis.ticks=element_blank(),
      axis.text=element_blank(),
      panel.border=element_blank(),
      legend.position = "none"
    )
}

distancia <- 3
valor_maximo <- max(data_semdiv$`2008`,data_semdiv$`2017`)
destaques <- c("Benefícios Previdenciários RGPS")

ggplot(data_semdiv) +
  geom_segment(
    aes(
      x = 0,
      xend = distancia,
      y = `2008`,
      yend = `2017`), 
    lineend = "round",
    color = ifelse(data_semdiv$Categoria %in% destaques,"#B90000", "#5a5a5a"),
    size = ifelse(data_semdiv$Categoria %in% destaques, 1, 0.5))+
  geom_text(label="2008", x=0, y=(1.03*(valor_maximo)), hjust = "center", size=4, family = "Roboto") +
  geom_text(label="2017", x=distancia, y=(1.03*valor_maximo), hjust= "center", size=4, family = "Roboto") +
  geom_text(
    aes(x = 0,
        y = ifelse(Categoria == "Custeio de Educação",0.50*`2008`,`2008`),
        label = labels_2008,
        hjust = 1,
        family = "Roboto Condensed Light"),
    color = ifelse(data_semdiv$Categoria %in% destaques,"#B90000", "#5a5a5a"),
    size = 3.5) +
  geom_text(
    aes(x = distancia,
        y = ifelse(Categoria == "Custeio de Educação",0.85*`2017`,
                   ifelse(Categoria == "Investimentos", 1.07*`2017`, `2017`)),
        label = labels_2017,
        hjust = 0,
        family = "Roboto Condensed Light"),
    color = ifelse(data_semdiv$Categoria %in% destaques,"#B90000", "#5a5a5a"),
    size = 3.5) +
  xlim(0-distancia,distancia+distancia) +
  ylim(0,1.03*valor_maximo) +
  labs(
    x = NULL,
    y = NULL,
    title = "Expenditure of the Brazilian Federal Government as a percentage of the GDP",
    subtitle = "SOCIAL BENEFITS ON THE RISE",
    caption = "Source: Siafi / National Treasury of Brazil",
    color = "",
    size = ''
  ) +
  theme_slope()
