library("readODS")
library("tidyverse")
#install.packages('reshape')
library("reshape")

setwd("~/Pablo/PABLO/2021/R/Vaccination_Spain")

today_date <- format(Sys.time(), "%Y%m%d")
today_url <- paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Informe_Comunicacion_",today_date,".ods")
name_url <- paste0("Informe_Comunicacion_",today_date,".ods")

today_url <- paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Informe_Comunicacion_20211220.ods")
name_url <- paste0("Informe_Comunicacion_20211220.ods")

download.file(today_url,name_url)

df_spain <- read_ods(name_url)
df_spain_1dosis <- read_ods(name_url,3)
df_spain_2dosis <- read_ods(name_url,4)
df_spain_1dosis <- df_spain_1dosis[c(1,20)]
df_spain_2dosis <- df_spain_2dosis[c(1,20)]
names(df_spain_1dosis)[1:2] <- c("Region","perc_vaccinated")
names(df_spain_2dosis)[1:2] <- c("Region","perc_vaccinated")
head(df_spain_1dosis)
#sapply(df_spain_1dosis, class) 

df_spain_1dosis_region <- filter(df_spain_1dosis, Region %in% c("Total España", "Madrid", "Cataluña"))
df_spain_2dosis_region <- filter(df_spain_2dosis, Region %in% c("Total España", "Madrid", "Cataluña"))
df_spain_2dosis_region
df_spain_1dosis_region <- cbind(Dose = 1, Week=34, df_spain_1dosis_region)
df_spain_2dosis_region <- cbind(Dose = 2, Week=34, df_spain_2dosis_region)
df_spain <- rbind(df_spain_1dosis_region,df_spain_2dosis_region)
head(df_spain)
sapply(df_spain, class) 
df_spain$Dose <- as.factor(df_spain$Dose)
df_spain$`perc_vaccinated` <- as.numeric(df_spain$`perc_vaccinated`)
df_spain$`perc_vaccinated` <- df_spain$`perc_vaccinated`*100
df_spain$`perc_vaccinated` <- round(df_spain$`perc_vaccinated`,1)
head(df_spain)


Date_spain <- seq(as.Date("2021/4/1"), by = "week", length.out = 22)
today_date_spain <- format(Date_spain, "%Y%m%d")
today_date_spain
for (i in today_date_spain) {
  today_url <- paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Informe_Comunicacion_",i,".ods")
  name_url <- paste0("Informe_Comunicacion_",i,".ods")
  download.file(today_url,name_url)
}

# first data from 1st April!!

for (i in today_date_spain) {
  name_url <- paste0("Informe_Comunicacion_",i,".ods")
  nam <- paste("df_spain_1dosis_", i, sep = "")
  assign(nam, read_ods(name_url,3))
  nam2 <- paste("df_spain_2dosis_", i, sep = "")
  assign(nam2, read_ods(name_url,4))
}







vaccine_stacked <- ggplot(df_spain, aes(fill=Dose, y=perc_vaccinated, x=Region)) + 
  geom_bar(position=position_dodge(), stat="identity")+
  #geom_text(aes(label = format((Percentage_vac*100), nsmall=1)), vjust = -0.2)+
  ggtitle("% of Population Vaccinated per Region in Spain and two largest regions")+
  xlab("Region")+
  geom_text(
    data=df_spain %>% filter(Dose=="1"), # Filter data first
    aes(label=perc_vaccinated, vjust = -1, hjust=1.5))+
  geom_text(
    data=df_spain %>% filter(Dose=="2"), # Filter data first
    aes(label=perc_vaccinated, vjust = -0.5, hjust=-0.7))+
  #geom_label( 
   # data=df_spain %>% filter(Dose=="2"), # Filter data first
    #aes(label=perc_vaccinated))+
  ylab("% of population vaccinated")+
  ylim(0, 100)+
  theme(legend.position = "top")#+
  #theme(axis.text.x = element_text(angle = 90))
vaccine_stacked
ggsave(filename = paste0("Vaccination_stacked_",today_date,".png"), width = 640/72, height = 450/72)


# 3 regions small multiples
vaccine_stacked_sm <- ggplot(df_spain, aes(fill=Dose,y=perc_vaccinated, x=Region)) + 
  geom_bar(position=position_dodge(), stat="identity")+
  #geom_text(aes(label = format((Percentage_vac*100), nsmall=1)), vjust = -0.2)+
  ggtitle("% of Population Vaccinated per Dose in Spain and two largest regions")+
  xlab("Region and Dose range")+
  ylab("% of population vaccinated")+
  ylim(0, 100)+
  facet_wrap(~Dose) +
  theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90))
vaccine_stacked_sm
ggsave(filename = paste0("Vaccination_stacked_smallMultiples",today_date,".png"), width = 640/72, height = 450/72)

