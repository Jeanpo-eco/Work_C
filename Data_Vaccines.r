#The Following code allows to download and clean the data about vaccination around the world
    #Source of the data: Mathieu, E., Ritchie, H., Ortiz-Ospina, E. et al. A global database of COVID-19 vaccinations. Nat Hum Behav (2021). https://doi.org/10.1038/s41562-021-01122-8

library(readr)
library(knitr)
library(openxlsx)
#Get and clean data
csv_path = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
csv_file = read.csv(csv_path)
venezuela = subset(csv_file, location == "Venezuela")
vzla_daily_vac = venezuela[ ,c(3,9)]

#Check the data
last_date = tail(vzla_daily_vac, n = 1)
last_date = last_date[ , 1]
setwd("C:/Users/jeanp/Downloads/Borrable/Bases de Datos")
current_data = read.xlsx("vaccines_vzla.xlsx")
check = tail(current_data, n = 1)
check = check[ ,1]
if (last_date != check){
  write.xlsx(vzla_daily_vac, "vaccines_vzla.xlsx", overwrite = TRUE)
  print("Data Updated")
}else{print("Not updated")}

