library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)

# Codigo para generar gráficos de las encuestas de google

setwd("C:/Github-DER/EEM Jurel")

dat <- read_excel("Datos-Taller1/Respuestas Taller 1.xlsx")

col_ini<-3 #primera columna a graficar
col_end<-dim(dat)[2]-2 #ultima columna a graficar
col_exept<-c(39, 40) #columna a omitir entre incio y fin. No omitir= NA

vect_plot<-seq(col_ini, col_end, by=1)
vect_plot <- vect_plot[! vect_plot %in% col_exept]

for(i in vect_plot){

summ_na <- dat[,i] %>%
  filter(!is.na(dat[,i]))

summ <- summ_na %>%
  group_by(summ_na[,1]) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)*100) %>% 
  arrange(desc(freq))

colnames(summ)<-c("resp", "count_r", "per")

p<-ggplot(data=summ, aes(x=resp, y=count_r)) +
  geom_bar(stat="identity", fill="steelblue")+
  coord_flip()+
  geom_text(aes(label=paste(count_r, " (", per, " %)", sep="")), hjust=-.1, size=4, color="black")+
  xlab("")+
  ylim(0, max(summ$count_r)+5)+
  ylab("respuestas")+
  ggtitle(label = str_wrap(colnames(dat[,i]), 100), 
          subtitle = paste(sum(summ$count_r), "respuestas"))+
  theme_bw()+
  theme(plot.margin = margin(r=40, t=10, b=10))

ggsave(filename=paste0("Plots-Taller1/", "plot_pregunta", i,".png"), 
       units = "cm", width = 35, height = 25, dpi=400, bg="white")  
}

