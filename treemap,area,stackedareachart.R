#importing libraries
library(ggplot2)
library(treemapify) 
library(tidyverse)
library(plyr)
library(reshape2)
library(ggplotify)
library("readxl")
library(ggExtra)
library(dplyr)
library(scales)
library(ggrepel)
#reading data to a data frame
df<-read_excel('food_sales.xlsx')
head(df)
names(df)[names(df) == "profit_$"] <- "profit"
df$label <- paste(df$item, df$profit, sep = ", ")
# treemap
ggplot(df, aes(area = profit, fill = meal, label=label,  subgroup = meal)) +
  geom_treemap(color = 'white') +
  geom_treemap_text( colour = "white", place = "centre",size=13,
                    grow = FALSE)+
  geom_treemap_subgroup_border(colour = "white", size = 2) +
  ggtitle("\nDAILY MEAL SALES ($)\n")+
  theme(plot.title = element_text(color = "dimgray"))+
  scale_fill_manual(values = c("skyblue","orange"))+
  theme(legend.title = element_blank()) 

#reading data to a data frame
df1<-read_excel('printer_sales.xlsx')
head(df1)
#area chart
ggplot(df1, aes(x=year, y=printer_sale, colour='skyblue')) + 
  xlab('\nYear') + ylab('Printer sales($)\n')+
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=1) +
  theme_bw()+  removeGrid()+
  theme(panel.border = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),axis.text=element_text(size=12),axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),  axis.title.y=element_text(size=15, colour ='dimgray'), axis.title.x=element_text(size=14, colour = 'dimgray'))+
  theme(axis.line = element_blank())+
  ggtitle("\nANNUAL PRINTER SALES")+
  theme(plot.title = element_text(color = "dimgray"))


#reading data to a dataframe
df2<-read_excel("us-population-by-age.xls")
head(df2)
names(df2)[names(df2) == "...1"] <- "year"
names(df2)[names(df2) == "Under 5"] <- "A"
names(df2)[names(df2) == "5 to 19"] <- "B"
names(df2)[names(df2) == "20 to 44"] <- "C"
names(df2)[names(df2) == "45 to 64"] <- "D"
names(df2)[names(df2) == "65+"] <- "E"
head(df2)
df2 <- df2[-c(17, 18, 19), ]
df2 <- melt(df2,id.vars="year",measure.vars=c("A","B","C", "D", "E"))
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#F0E442", "#0072B2")
df2 <-df2 %>% group_by( year, variable) %>%
  summarise(n = sum(value)) %>%
  mutate(percentage = n / sum(n))
#stacked area chart
ggplot(df2, aes(x=year, y=percentage, group = variable, fill=variable))+
  geom_area(alpha=0.6 , size=1, colour="dimgray", position = position_stack(reverse = T))+
  ggtitle("\nAGING POPULATION\n")+
  labs(x = '\nYear', y = 'Population distribution\n')+
  scale_fill_manual(values=cbPalette, labels=c("Under 5","5 to 19","20 to 44", "45 to 64", "65+"))+scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text=element_text(size=12),axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),  axis.title.y=element_text(size=15, colour ='dimgray'), axis.title.x=element_text(size=14, colour = 'dimgray'))+
  theme(plot.title = element_text(color = "dimgray"))+
  scale_x_discrete(breaks=c(1860, 1880,1900, 1920, 1940, 1960, 1980, 2000, 2005))+ 
  theme(legend.title = element_blank())
  
  
