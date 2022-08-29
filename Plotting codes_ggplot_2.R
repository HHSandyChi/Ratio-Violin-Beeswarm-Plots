setwd("Documents/UCL/MSc_Project/R/")
sgetwd()
library(dplyr)
library(EnvStats)
library(data.table)

filelist <- list.files(pattern = "*.csv")
input <- lapply(filelist, read.csv)

## Calculate ratio ##
df <- rbindlist(input)

odds_even <- seq_len(nrow(df))%%2
measure_df <- df[odds_even == 1,]
measure_df[,1] <- 1:nrow(measure_df)
measure_df <- measure_df[,-c(2:5)]
colnames(measure_df)[2] <- "measure"

total_df <- df[odds_even == 0,]
total_df[,1] <- 1:nrow(total_df)
total_df <- total_df[,-c(2:5)]
colnames(total_df)[2] <- "total"

bind_df <- left_join(measure_df, total_df)
bind_df$ratio <- bind_df$measure/bind_df$total
write.csv(bind_df, file = "../scrambled_noheart_ratio.csv")



### Data Sorting ###
inj_heart <- read.csv("guide_heart_ratio.csv", header = TRUE)
inj_heart$group <- as.character(1)
colnames(inj)[5:6] <- c("ratio","group")
inj <- inj[,-c(6:16)]

inj_noheart <- read.csv("guide_noheart_ratio.csv", header = TRUE)
inj_noheart$group <- as.character(2)
colnames(inj_heart)[5:6] <- c("ratio","group")

scr_heart<- read.csv("scrambled_heart_ratio.csv", header = TRUE)
scr_heart$group <- as.character(3)
colnames(scr_heart)[5:6] <- c("ratio","group")

scr_noheart<- read.csv("scrambled_noheart_ratio.csv", header = TRUE)
scr_noheart$group <- as.character(4)
colnames(scr_heart)[5:6] <- c("ratio","group")

data <- rbind(inj_heart, inj_noheart, scr_heart, scr_noheart)
data[,6]<-factor(data[,6], levels=c("1","2", "3", "4"))

## Plotting with ggplot2##
install.packages("ggplot2")
install.packages("ggbeeswarm")
install.packages("ggpubr")
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(data.table)
library(EnvStats)


grouplab <- c("grk5 knock-out", "Scrambled") #Create the group label
mycomparison <- list(c("1","2"))

# !!Preview the plot first before running tiff
# !!Make sure codes start with tiff() and end with dev.off()
tiff(file = "Plot_p1_kw.tiff", res= 300, width = 10, height = 5, units = "in")
  
ggplot(data, aes(x= group, y= ratio))+ 
  geom_beeswarm(groupOnX = TRUE, cex = 0.8, size = 0.3)+ #groupOnX enable grouping; cex: space between dots; size: dot sizes
  #scale_x_discrete(label= grouplab)+ #label the group 
  scale_y_continuous(limits = range(0,1.1))+ 
  geom_boxplot(alpha = 0, lwd = 0.3, width=0.2)+ #alpha: transparency; lwd:line width; width: box width
  coord_flip()+ #horizontal
  xlab("")+ # Delete the title
  ylab("Migrated distance ratio")+ #Rename title
  theme(axis.text.x = element_text(size = 12, colour = "black"), 
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.line.x.bottom = element_line(color = "black", size = 0.5),
        axis.line.y.left = element_line(colour = "black", size = 0.5),
        axis.title.x = element_text(size = 15),
        aspect.ratio = 2/4,
        panel.background = element_rect(fill = "transparent", ),
        panel.grid.major.x = element_line(colour = "grey", size = 0.3),
        panel.grid.minor.x = element_line(color = "grey", size = 0.3)
        )+
  stat_compare_means(comparisons = mycomparison, tip.length = 0, step.increase = 0.2, 
                     label = "p.signif"
                     )+
  stat_n_text(y.pos = 0, hjust = "left")

dev.off()
mycolor <- c("#339966","0066CC" ,"CC99FF")
mycomparison <- list(c("1","2"),
                     c("1","3"),
                     c("2","4"),
                     c("3","4")
                     )
grouplab <- c("Chemokine flood", "Physiological level","Chemokine flood", "Physiological level") #Create the group label

tiff(file = "Plot_1200.tiff", res= 300, width = 10, height = 5, units = "in")

ggplot(data, aes(x= group, y= ratio, fill= group))+ 
  geom_violin(trim = FALSE)+
  scale_fill_manual(values = c("#FF9999","#FF9999", "#3399FF","#3399FF"))+
  scale_x_discrete(label= grouplab)+ #label the group 
  scale_y_continuous(limits = range(0,1.1))+
  geom_boxplot(alpha = 0, lwd = 0.3, width=0.3)+ #alpha: transparency; lwd:line width; width: box width
  coord_flip()+ #horizontal
  xlab("")+ # Delete the title
  ylab("Migrated distance ratio")+ #Rename title
  theme(axis.text.x = element_text(size = 12, colour = "black"), 
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.line.x.bottom = element_line(color = "black", size = 0.5),
        axis.line.y.left = element_line(colour = "black", size = 0.5),
        axis.title.x = element_text(size = 15),
        aspect.ratio = 1/2.5,
        panel.background = element_rect(fill = "transparent", ),
        panel.grid.major.x = element_line(colour = "grey", size = 0.3),
        panel.grid.minor.x = element_line(color = "grey", size = 0.3),
        legend.position="none"
  )+
  stat_compare_means(comparisons = mycomparison, 
                     tip.length = 0, 
                     step.increase = 0.08,
                     label.y = c(0.94, 0.98, 1, 0.96), 
                     label = "p.signif",
                     hide.ns = FALSE,
                     size = 3
                    )+
  #stat_compare_means(label = NULL,label.y.npc = "bottom",label.x.npc = "right",hjust = -0.1, vjust = -2)+
  stat_n_text(y.pos = 0, hjust = "left")

dev.off()




## Statistics ##
pairwise.wilcox.test(data[,1], data[,2],p.adjust.method="none") #two groups, non-parametric
kruskal.test() #multiple group, non-parametric
