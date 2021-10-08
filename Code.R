library(tidytuesdayR)
library(tidyverse)
library(geofacet)
library(ggrepel)
library(reshape2)
install.packages("RColorBrewer")
library(RColorBrewer)
tuesdata <- tidytuesdayR::tt_load(2021, week = 41)

tuesdata$nurses->nurses

glimpse(nurses)

nurses%>%select(State,Year,`Annual Salary Median`)%>%
  distinct(State,Year,.keep_all = TRUE)->salary
salary%>%group_by(State,Year)%>%
  arrange(State,Year)->salary
salary
salary$Salary<-(salary$Salary/1000)
colnames(salary)<-c("State","Year","Salary")

ggplot(salary, aes(Year,State,fill=Salary))+
  geom_tile(aes(width=0.9, height=0.9), size=2)+
  scale_fill_distiller(palette="RdBu", direction = 1,limits=c(20, 120), breaks=seq(20,120,by=20),expand = c(0, 0))+
  theme_minimal()+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(breaks=c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,
                                                2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020))+
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_blank())+
  labs(title="HOW HAS THE SALARY OF THE US NURSES CHANGED OVER THE YEARS?",
       subtitle = "The median salary (in thousand dollars) of the nurses for each US State has been plotted for the last two decades (1998 to 2020)",
       caption = "Data: TidyTuesday|Design: @annapurani93")+
  theme(plot.title = element_text(face = "bold",size = 24, colour = "white", hjust=0.5),
        plot.subtitle = element_text(size = 16, colour = "white", hjust=0.5),
        plot.caption = element_text(size=10,colour = "white"))+
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"))+
    theme(axis.text = element_text(color="white",size=18))+
  theme(axis.text.y = element_text(margin = margin(r =-45)))+
  theme(legend.text = element_text(color="white"),
        legend.position = "top")+
  labs(fill = "Annual Median Salary (in thousand dollars)")+
    theme(legend.title = element_text(color = "white", face="bold",size = 10))+
    theme(legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(3, 'cm'))+
    guides(fill = guide_colorbar(title.vjust = 0.9))->tile

tile
ggsave("tile2.png",tile,width=30,height = 40)
