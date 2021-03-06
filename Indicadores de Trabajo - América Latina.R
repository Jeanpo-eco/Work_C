#Code made by Jean Pierre Oliveros for employment indicators in Latin America

#Load the packages
library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

#Choose directory and download the data
setwd("C:/Users/wilme/Desktop/CAF/Indicadores de Trabajo")
destfile = paste(getwd( ), "Indicadores de Trabajo - Am�rica Latina.xlsx", sep = "/")
download.file("https://github.com/Jeanpo-eco/Work_C/raw/main/Indicadores%20de%20Trabajo%20-%20Am%C3%A9rica%20Latina%20v3.xlsx", destfile, mode = "wb", quiet = TRUE)
remove(destfile)
destfile = paste(getwd( ), "Referencias.txt", sep = "/")
download.file("https://raw.githubusercontent.com/Jeanpo-eco/Work_C/main/Referencias.txt", destfile, mode = "wb", quiet = TRUE)
remove(destfile)
destfile = paste(getwd( ), "Glosario.txt", sep = "/")
download.file("https://raw.githubusercontent.com/Jeanpo-eco/Work_C/main/Glosario%20de%20T%C3%A9rminos.txt", destfile, mode = "wb", quiet = TRUE)
remove(destfile)


#Load the data for each country
Argentina <- read_excel("Indicadores de Trabajo - Am�rica Latina.xlsx", sheet = "Argentina (T)", range = "A7:Q16") 
Bolivia <- read_excel("Indicadores de Trabajo - Am�rica Latina.xlsx", sheet = "Bolivia (T)", range = "A7:Q16")
Brasil <- read_excel("Indicadores de Trabajo - Am�rica Latina.xlsx", sheet = "Brasil (T)", range = "A7:Q16")
Chile <- read_excel("Indicadores de Trabajo - Am�rica Latina.xlsx", sheet = "Chile (T)", range = "A7:Q16")
Colombia <- read_excel("Indicadores de Trabajo - Am�rica Latina.xlsx", sheet = "Colombia (T)", range = "A7:Q16")
Costa_Rica <- read_excel("Indicadores de Trabajo - Am�rica Latina.xlsx", sheet = "Costa Rica (T)", range = "A7:Q16")
Ecuador <- read_excel("Indicadores de Trabajo - Am�rica Latina.xlsx", sheet = "Ecuador (T)", range = "A7:Q16")
Mexico <- read_excel("Indicadores de Trabajo - Am�rica Latina.xlsx", sheet = "M�xico (T)", range = "A7:Q16")
Peru <- read_excel("Indicadores de Trabajo - Am�rica Latina.xlsx", sheet = "Per� (T)", range = "A7:Q16")
Uruguay <- read_excel("Indicadores de Trabajo - Am�rica Latina.xlsx", sheet = "Uruguay (T)", range = "A7:Q16")

#Fix each country
Argentina = Argentina %>% mutate(Argentina,
                                 Pa�s= "Argentina")
Bolivia = Bolivia %>% mutate(Bolivia,
                                 Pa�s= "Bolivia")
Brasil = Brasil %>% mutate(Brasil,
                             Pa�s= "Brasil")
Chile = Chile %>% mutate(Chile,
                             Pa�s= "Chile")
Colombia = Colombia %>% mutate(Colombia,
                             Pa�s= "Colombia")
Costa_Rica = Costa_Rica %>% mutate(Costa_Rica,
                             Pa�s= "Costa Rica")
Ecuador = Ecuador %>% mutate(Ecuador,
                             Pa�s= "Ecuador")
Mexico = Mexico %>% mutate(Mexico,
                             Pa�s= "M�xico")
Peru = Peru %>% mutate(Peru,
                             Pa�s= "Per�")
Uruguay = Uruguay %>% mutate(Uruguay,
                             Pa�s= "Uruguay")
#Append countries in the same object

Base = bind_rows(Argentina, Bolivia, Brasil, Chile, Colombia, Costa_Rica,
                 Ecuador, Mexico, Peru, Uruguay)
#Tidy the Base data and remove unnecesary data
colnames(Base)[1:6] = c("Trimestre", "Tasa Desempleo", "Desempleo Var %", 
                        "Tasa de Informalidad", "Informalidad Var %",
                        "Total Participaci�n")
Base = Base[, c(18, 1, 2:17)]
remove(Argentina, Bolivia, Brasil, Chile, Colombia, Costa_Rica,
       Ecuador, Mexico, Peru, Uruguay)

  #Divide in each category
Unemployment = Base[, c(1:4)]
Informal = Base[, c(1:2, 5:6)]
Participation_Total = Base[, c(1,2,7)]
Participation_Sexo = Base[, c(1,2,8:9)]
Participation_Edad = Base[, c(1,2,10:15)]
Participation_Educacion = Base[, c(1,2,16:18)]
  #Tidy Participation
Participation_Sexo = gather(Participation_Sexo, 3:4, key = "Tipo de Participaci�n", value = "Tasa de Participaci�n")
Participation_Edad = gather(Participation_Edad, 3:8, key = "Tipo de Participaci�n", value = "Tasa de Participaci�n")
Participation_Educacion = gather(Participation_Educacion, 3:5, key = "Tipo de Participaci�n", value = "Tasa de Participaci�n")
Participation_Total = gather(Participation_Total, 3, key = "Tipo de Participaci�n", value = "Tasa de Participaci�n" )
Participation = rbind(Participation_Total, Participation_Sexo, Participation_Educacion, Participation_Edad)
#Plots

  #Informal Jobs

    #Todos Juntos Absolutos
ggplot(data = Informal, aes(Trimestre, `Tasa de Informalidad`, group = Pa�s, color = Pa�s))+
  geom_line(aes(Trimestre, `Tasa de Informalidad`), size = 1)+
  geom_point(aes(Trimestre, `Tasa de Informalidad`))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Tasa de Informalidad")
ggsave("Tasa de Informalidad Total.png")
    #Por Grupos Absolutos
ggplot(data = Informal, aes(Trimestre, `Tasa de Informalidad`, group = Pa�s, color = Pa�s))+
  geom_line(aes(Trimestre, `Tasa de Informalidad`), size = 1)+
  geom_point(aes(Trimestre, `Tasa de Informalidad`))+
  facet_wrap(~Pa�s)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")+
  labs(title = "Tasa de Informalidad por Pa�s")
ggsave("Tasa de Informalidad por Pa�s.png")
    #Por Grupos Var %
ggplot(data = Informal, aes(Trimestre, `Informalidad Var %`, group = Pa�s, color = Pa�s))+
  geom_line(aes(Trimestre, `Informalidad Var %`), size = 1)+
  geom_point(aes(Trimestre, `Informalidad Var %`))+
  geom_hline(yintercept=0, linetype="dashed", alpha = 0.5)+
  facet_wrap(~Pa�s)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")+
  labs(title = "Var % de Informalidad")
ggsave("Variaci�n de Informalidad por Pa�s.png")

  #Participation
    #Por grupos
ggplot(data = Participation, aes(Trimestre, `Tasa de Participaci�n`, group = `Tipo de Participaci�n`, color = `Tipo de Participaci�n`))+
  geom_line(aes(Trimestre, `Tasa de Participaci�n`), size = 1)+
  geom_point(aes(Trimestre, `Tasa de Participaci�n`))+
  facet_wrap(~ Pa�s)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Tasa de Participaci�n")
ggsave("Tasa de Participaci�n por Pa�s.png")
    #Total
ggplot(data = Participation_Total, aes(Trimestre, `Tasa de Participaci�n`, group = `Tipo de Participaci�n`, color = `Tipo de Participaci�n`))+
  geom_line(aes(Trimestre, `Tasa de Participaci�n`), size = 1)+
  geom_point(aes(Trimestre, `Tasa de Participaci�n`))+
  facet_wrap(~ Pa�s)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")+
  labs(title = "Tasa de Participaci�n Total")
ggsave("Tasa de Participaci�n Total por Pa�s.png")
    #Por Sexo
ggplot(data = Participation_Sexo, aes(Trimestre, `Tasa de Participaci�n`, group = `Tipo de Participaci�n`, color = `Tipo de Participaci�n`))+
  geom_line(aes(Trimestre, `Tasa de Participaci�n`), size = 1)+
  geom_point(aes(Trimestre, `Tasa de Participaci�n`))+
  facet_wrap(~ Pa�s)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Tasa de Participaci�n por sexo")
ggsave("Tasa de Participaci�n Sexo por Pa�s.png")
    #Por Edad
ggplot(data = Participation_Edad, aes(Trimestre, `Tasa de Participaci�n`, group = `Tipo de Participaci�n`, color = `Tipo de Participaci�n`))+
  geom_line(aes(Trimestre, `Tasa de Participaci�n`), size = 1)+
  geom_point(aes(Trimestre, `Tasa de Participaci�n`))+
  facet_wrap(~ Pa�s)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Tasa de Participaci�n por Edad")
ggsave("Tasa de Participaci�n Edad por Pa�s.png")
    #Por Educaci�n
ggplot(data = Participation_Educacion, aes(Trimestre, `Tasa de Participaci�n`, group = `Tipo de Participaci�n`, color = `Tipo de Participaci�n`))+
  geom_line(aes(Trimestre, `Tasa de Participaci�n`), size = 1)+
  geom_point(aes(Trimestre, `Tasa de Participaci�n`))+
  facet_wrap(~ Pa�s)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Tasa de Participaci�n por Educaci�n")
ggsave("Tasa de Participaci�n Educaci�n por Pa�s.png")
  #Unemployment
ggplot(data = Unemployment, aes(Trimestre, `Tasa Desempleo`, group = Pa�s, color = Pa�s))+
  geom_line(aes(Trimestre, `Tasa Desempleo`), size = 1)+
  geom_point(aes(Trimestre, `Tasa Desempleo`))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Tasa de Desempleo")
ggsave("Tasa de Desempleo Total.png")
#Por Grupos Absolutos
ggplot(data = Unemployment, aes(Trimestre, `Tasa Desempleo`, group = Pa�s, color = Pa�s))+
  geom_line(aes(Trimestre, `Tasa Desempleo`), size = 1)+
  geom_point(aes(Trimestre, `Tasa Desempleo`))+
  facet_wrap(~Pa�s)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")+
  labs(title = "Tasa de Desempleo por Pa�s")
ggsave("Tasa de Desempleo por Pa�s.png")
#Por Grupos Var %
ggplot(data = Unemployment, aes(Trimestre, `Desempleo Var %`, group = Pa�s, color = Pa�s))+
  geom_line(aes(Trimestre, `Desempleo Var %`), size = 1)+
  geom_point(aes(Trimestre, `Desempleo Var %`))+
  geom_hline(yintercept=0, linetype="dashed", alpha = 0.5)+
  facet_wrap(~Pa�s)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")+
  labs(title = "Var % de Desempleo")
ggsave("Variaci�n de Desempleo por Pa�s.png")
  
  
  
  
      