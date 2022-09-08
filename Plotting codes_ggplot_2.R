#set your working directory#
setwd("")
#get your current working directory#
getwd("")
#install packages#
install.packages("ggplot2")
install.packages("ggbeeswarm")
install.packages("ggpubr")
install.packages("EnvStats")
#activate packages#
library(dplyr)
library(EnvStats)
library(data.table)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)


##If you want to calculate ratios in several csv files in the same directory, creating a filelist and process them in batch may save you a century## 
#Create filelist in your current directory, the pattern of file names must be "*.csv"#
filelist <- list.files(pattern = "*.csv")
#Read csv file list as dataframe list#
input <- lapply(filelist, read.csv)

##============ Calculate ratio ============##
## For each measurement csv files ##
#Bind all the dataframes in the list by row#
df <- rbindlist(input)
#Migrated distance data is in odds rows, seperate these rows as another dataframe#
odds_even <- seq_len(nrow(df))%%2
measure_df <- df[odds_even == 1,]
#Number the measure_df (also the number of fish you get) in first column
measure_df[,1] <- 1:nrow(measure_df)
#Keep only number and distance length columns#
measure_df <- measure_df[,-c(2:5)]
#Rename the column#
colnames(measure_df)[2] <- "measure"

#Total length data is in even rows, do the same steps as migrated distance data#
total_df <- df[odds_even == 0,]
total_df[,1] <- 1:nrow(total_df)
total_df <- total_df[,-c(2:5)]
colnames(total_df)[2] <- "total"

#Bind measure_df and total_df by the number column#
bind_df <- left_join(measure_df, total_df)
#Calculate migrated distance ratio#
bind_df$ratio <- bind_df$measure/bind_df$total
#Save the dataframe as csv file under the directory#
#!!!!!!Remember to change the file name!!!!!!#
write.csv(bind_df, file = "./*_ratio.csv")
#Or save the dataframe as rda file under the directory#
save(bind_df, file = "./*_ratio.rda")

##============ Bind all injected (or scrambled) measurement dfs as one injected df (or scrambled df) ============## 
df_1 <- read.csv("./*_ratio.csv")
df_2 <- read.csv("./*_ratio.csv")
yourdf <- rbind(df_1, df_2)

##============ Group by experimental conditions ============##
#!!!!!!Remember to change "yourdf" to your dataframe names!!!!!!#

#------- 1: injected; 2:Scrambled -------#
yourdf$group <- as.character(1)
#Rename columns#
colnames(yourdf)[5:6] <- c("ratio","group")
#------- Repeat these two steps for scrambled data by changing as.character(1) to as.character(2) & df names -------#

#Bind injected and scrambled data by rows#
#!!!!!!Remember to change "inj_df" & "scr_df" to your dataframe names!!!!!!#
data <- rbind(inj_df, scr_df)

#Save as rda file#
save(data, file = "./*_forplot.rda")

##============ Beeswarm/box plot ============##
#!!!!!!Use R studio for preview and save the plot!!!!!!#
#Create the group label#
grouplab <- c("grk5 knock-out", "Scrambled") 

#Plot#
ggplot(data, aes(x= group, y= ratio))+ 
  geom_beeswarm(groupOnX = TRUE, cex = 0.8, size = 0.3)+ #groupOnX enable grouping; cex: space between dots; size: dot sizes
  scale_x_discrete(label= grouplab)+ #label the group 
  scale_y_continuous(limits = range(0,1.1))+ #Set limitaion to y axis
  geom_boxplot(alpha = 0, lwd = 0.3, width=0.2)+ #alpha: transparency; lwd:line width; width: box width
  coord_flip()+ #horizontal
  xlab("")+ # Delete the title
  ylab("Migrated distance ratio")+ #Rename title
  theme(axis.text.x = element_text(size = 12, colour = "black"), #Axis text size
        axis.text.y = element_text(size = 12, colour = "black"), #Axis text size
        axis.line.x.bottom = element_line(color = "black", size = 0.5), #Axis line style
        axis.line.y.left = element_line(colour = "black", size = 0.5), #Axis line style
        axis.title.x = element_text(size = 15), #X axis tile text size
        aspect.ratio = 2/4, #aspect ratio
        panel.background = element_rect(fill = "transparent", ), #Plot background
        panel.grid.major.x = element_line(colour = "grey", size = 0.3), #Grid line (major)
        panel.grid.minor.x = element_line(color = "grey", size = 0.3) #Grid line (minor)
        )+
  stat_compare_means(comparisons = c("1","2"), #Statistics
                     tip.length = 0, 
                     step.increase = 0.2, 
                     label = "p.signif" #Show p-value as asterisks or ns
                     )+
  stat_n_text(y.pos = 0, hjust = "left")#Statistics text position

#!!!!!! Preview and save as pdf in R studio by clicking "Plots > Export > Save as PDF" bottom !!!!!!#
#!!!!!! Usually save the pdf as 4.0*8.0 !!!!!!#


##============ Violin plot ============##
#!!!!!!Use R studio for preview and save the plot!!!!!!#
#Create the group label#
grouplab <- c("Chemokine flood", "Physiological level") 

ggplot(data, aes(x= group, y= ratio, fill= group))+ 
  geom_violin(trim = FALSE)+
  scale_fill_manual(values = c("#FF9999", "#3399FF"))+ #Specify colors to each group
  scale_x_discrete(label= grouplab)+ #label the group 
  scale_y_continuous(limits = range(0,1.1))+ #Set limitaion to y axis
  geom_boxplot(alpha = 0, lwd = 0.3, width=0.3)+ #alpha: transparency; lwd:line width; width: box width
  coord_flip()+ #horizontal
  xlab("")+ # Delete the title
  ylab("Migrated distance ratio")+ #Rename title
  theme(axis.text.x = element_text(size = 12, colour = "black"), #Axis text size
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.line.x.bottom = element_line(color = "black", size = 0.5), #Axis line style
        axis.line.y.left = element_line(colour = "black", size = 0.5),
        axis.title.x = element_text(size = 15), #X axis tile text size
        aspect.ratio = 1/2.5,
        panel.background = element_rect(fill = "transparent", ),
        panel.grid.major.x = element_line(colour = "grey", size = 0.3),
        panel.grid.minor.x = element_line(color = "grey", size = 0.3),
        legend.position="none"
  )+
  stat_compare_means(comparisons = c("1","2"), 
                     tip.length = 0, 
                     step.increase = 0.08,
                     label.y = c(0.94, 0.98, 1, 0.96), #Optional: specify the position of labels
                     label = "p.signif",
                     hide.ns = FALSE,
                     size = 3
                    )+
  stat_n_text(y.pos = 0, hjust = "left")
#!!!!!! Preview and save as pdf in R studio by clicking "Plots > Export > Save as PDF" bottom !!!!!!#
#!!!!!! Usually save the pdf as 4.0*8.0 !!!!!!#

