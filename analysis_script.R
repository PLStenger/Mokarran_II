# R Script for the Mokarran Protection Society analysis - for Andrea Kunovsky internship
# By Pierre-Louis STENGER, if questions, mail me at Pierrelouis.stenger@gmail.com

setwd("~/Documents/Mokarran Project/2021 Stage_MPS_Andrea_KUNOVSKY/12 - analyses pour publication")

library(readxl)
library(tidyverse) 
library(ggplot2)
library(multcomp)
library(ggpubr)
library(fmsb)

######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
## Data from Mokarran campaign I (Mok I) 2019 - 2020
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################

###########################################################################
# Data import
m <- read_excel("MPS_Database_MWI_presence_absence_postD.xlsm") 
head(m)
dat <- data.frame(m)
head(dat)
str(dat)
###########################################################################

###########################################################################
# Data adaptation
dat$Dive_Id <- as.factor(dat$Dive_Id)
dat$Presence <- as.factor(dat$Presence)
dat$Site <- as.factor(dat$Site)
dat$Current <- as.factor(dat$Current)
dat$T..22.m <- as.numeric(dat$T..22.m)
dat$T..57.m <- as.numeric(dat$T..57.m)
dat$Moon_Age <- as.factor(dat$Moon_Age)
dat$Day_Phase <- as.factor(dat$Day_Phase)
dat$Sex <- as.factor(dat$Sex)
dat$Hour <- as.factor(dat$Hour)
dat$Tidal_coefficient <- as.factor(dat$Tidal_coefficient)
###########################################################################

# Checking
unique(dat$Presence)

dat_clean <- dat[!is.na(dat$Presence),]
head(dat_clean)


###########################################################################
###########################################################################
# GLM analysis
###########################################################################
###########################################################################

# https://marieetienne.github.io/ModStat/glm.html
glm_mok_01 <- glm(Presence ~ Moon_Age , family=binomial, data=dat_clean) 
glm_mok_02 <- glm(Presence ~ 1 , family=binomial, data=dat_clean) 

anova(glm_mok_01, glm_mok_02, test = 'Chisq')



######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
## Data from Mokarran campaign II (Mok II) 2020 - 2021
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################


###########################################################################
# Data import
m <- read_excel("MPS_Database_MWII_presence_absence_postD.xlsm") 
head(m)
dat <- data.frame(m)
head(dat)
str(dat)
###########################################################################

###########################################################################
# Data adaptation
dat$Dive_Id <- as.factor(dat$Dive_Id)
dat$Presence <- as.factor(dat$Presence)
dat$Site <- as.factor(dat$Site)
dat$Current <- as.factor(dat$Current)
dat$T..22.m <- as.numeric(dat$T..22.m)
dat$T..57.m <- as.numeric(dat$T..57.m)
dat$Moon_Age <- as.factor(dat$Moon_Age)
dat$Day_Phase <- as.factor(dat$Day_Phase)
dat$Sex <- as.factor(dat$Sex)
dat$Hour <- as.factor(dat$Hour)
dat$Tidal_coefficient <- as.factor(dat$Tidal_coefficient)
###########################################################################

# Checking
unique(dat$Presence)
length(unique(dat$Tidal_coefficient))

dat_clean <- dat[!is.na(dat$Presence),]
head(dat_clean)

###########################################################################
###########################################################################
# GLM analysis
###########################################################################
###########################################################################

# https://marieetienne.github.io/ModStat/glm.html
glm_mok_01 <- glm(Presence ~ Moon_Age , family=binomial, data=dat_clean) 
glm_mok_02 <- glm(Presence ~ 1 , family=binomial, data=dat_clean) 

anova(glm_mok_01, glm_mok_02, test = 'Chisq')


glm_mok_01 <- glm(Presence ~ Tidal_coefficient , family=binomial, data=dat_clean) 
glm_mok_02 <- glm(Presence ~ 1 , family=binomial, data=dat_clean) 

anova(glm_mok_01, glm_mok_02, test = 'Chisq')


###########################################################################
###########################################################################
# Temperature
###########################################################################
###########################################################################
###########################################################################


summary(dat$T..22.m)
length(na.omit(dat$T..22.m))

summary(dat$T..57.m)
length(na.omit(dat$T..57.m))

ggplot(data=subset(dat, !is.na(T..22.m)), aes(x=Presence, y=T..22.m, color=Presence)) +
  geom_boxplot(outlier.shape = NA) + 
  ylab("Temperature (°C)") +
  ylim(26, 29) +
  ggtitle("22 meters depth") +
  theme_minimal() + 
  stat_compare_means() + scale_color_manual(values=c("firebrick3", "dodgerblue2"))


ggplot(data=subset(dat, !is.na(T..57.m)), aes(x=Presence, y=T..57.m, color=Presence)) +
  geom_boxplot(outlier.shape = NA) + 
  ylab("Temperature (°C)") +
  ylim(26, 29) +
  ggtitle("57 meters depth") +
  theme_minimal() + 
  stat_compare_means() + scale_color_manual(values=c("firebrick3", "dodgerblue2"))


###########################################################################
###########################################################################
# Tidal coefficient
###########################################################################
###########################################################################

summary(dat[ which(dat$Presence==1), ]$Tidal_coefficient)
summary(as.numeric(as.character(dat[ which(dat$Presence==1), ]$Tidal_coefficient)))
summary(as.numeric(as.character(dat[ which(dat$Presence==0), ]$Tidal_coefficient)))

str(dat$Tidal_coefficient)

as.numeric(as.character(dat$Tidal_coefficient))

summary(as.numeric(as.character(dat$Tidal_coefficient)))
length(na.omit(dat$T..22.m))

summary(dat$T..57.m)
length(na.omit(dat$T..57.m))

ggplot(data=subset(dat, !is.na(Tidal_coefficient)), aes(x=Presence, y=as.numeric(as.character(Tidal_coefficient)), color=Presence)) +
  geom_boxplot(outlier.shape = NA) + 
  ylab("Tidal coefficient") +
  #ylim(26, 29) +
  theme_minimal() + 
  stat_compare_means() + scale_color_manual(values=c("firebrick3", "dodgerblue2"))


###########################################################################
###########################################################################
# Moon Age
###########################################################################
###########################################################################

summary(dat[ which(dat$Presence==1), ]$Moon_Age)
summary(dat[ which(dat$Presence==0), ]$Moon_Age)
summary(dat$Moon_Age)


##Radar plot
data2 <-data.frame(dat$Presence, dat$Moon_Age) 
colnames(data2) <- c("Presence", "Moon_Age")

data3_no <-data2[ which(data2$Presence=='0'), ]
data3_no <- data3_no

data3_no$Presence <- sub("0", "1", data3_no$Presence)
dat_summarise_no <- data3_no %>% 
  group_by(Moon_Age) %>% 
  summarise(value = sum(as.numeric(as.character(Presence))))
#data4_no <- count(data3_no, "Moon_Age")

data3_yes <-data2[ which(data2$Presence=='1'), ]
#data4_yes <- count(data3_yes, "Moon_Age")

dat_summarise_yes <- data3_yes %>% 
  group_by(Moon_Age) %>% 
  summarise(value = sum(as.numeric(as.character(Presence))))

phase <- data.frame(c(0:29))
colnames(phase) <- c("phase")
head(phase)

data_m <- merge(phase, dat_summarise_no, by.x="phase", by.y="Moon_Age", all = TRUE)
head(data_m)

data_02 <- merge(data_m, dat_summarise_yes, by.x="phase", by.y="Moon_Age", all = TRUE)
colnames(data_02) <- c("phase", "no", "yes")
head(data_02)

data_02[is.na(data_02)] <- 0

data_03 <- t(data_02) # Transpose the data

colnames(data_03) = data_03[1, ] 
data_03 = data_03[-1, ]        

head(data_03)
str(data_03)

data_04 <- rbind(rep(30,30) , rep(0,30), data_03)
data_05 <- data.frame(data_04)
colnames(data_05) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29")
str(data_05)
head(data_05)

# Color vector
colors_border=c(rgb(205/255, 38/255, 38/255 ,0.9) ,  rgb(30/255,144/255,255/255,0.9))
colors_in=c(rgb(205/255, 38/255, 38/255,0.4) , rgb(30/255,144/255,255/255,0.4))

# plot with default options:
radarchart( data_05  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# Add a legend

legend(x=1, y=1.5, legend = rownames(data_05[-c(1,2),]), bty = "n", pch=20 , col=colors_border, text.col = "black", cex=1.2, pt.cex=3)




ggplot(data=subset(dat, !is.na(Moon_Age)), aes(x=Presence, y=as.numeric(as.character(Moon_Age)), color=Presence)) +
  geom_boxplot(outlier.shape = NA) + 
  ylab("Moon Age") +
  #ylim(26, 29) +
  theme_minimal() + 
  stat_compare_means() + scale_color_manual(values=c("firebrick3", "dodgerblue2"))

ggplot(data=subset(dat, !is.na(Moon_Age)), aes(x=Presence, y=Moon_Age, color=Presence)) +
  geom_boxplot() + 
  ylab("Moon Age") +
  #ylim(26, 29) +
  theme_minimal() + 
  stat_compare_means() + scale_color_manual(values=c("firebrick3", "dodgerblue2"))

## Graph
m_Presence=dat[ which(dat$Presence==1), ]
m_Presence$Presence <- as.numeric(m_Presence$Presence)
ggplot(m_Presence, aes(x=Moon_Age, y=Presence))+geom_bar(stat="identity", fill="dodgerblue2")
m_Absence=dat[ which(dat$Presence==0), ]
m_Absence$Presence <- as.numeric(m_Absence$Presence)
ggplot(m_Absence, aes(x=Moon_Age, y=Presence))+geom_bar(stat="identity", fill="firebrick3")

## Percentages
ggplot(data=subset(dat, !is.na(Moon_Age)), aes(x=Moon_Age)) +
  geom_bar(aes(fill = Presence), position = "fill") +
  theme_classic() +
  scale_fill_manual(values = c("firebrick3", "dodgerblue2")) +
  coord_flip() + theme(legend.position='none') +
  ylab("Observation success/failure (in percent)") +
  xlab("Moon age") +
  scale_y_continuous(sec.axis = dup_axis())   

## Tests
shapiro.test(data_02$no) # p-value = 0.008132 (<0.05) 
shapiro.test(data_02$yes) # p-value = 0.00493 (<0.05) 
wilcox.test(data_02$no, data_02$yes) # p-value = 5.721e-06 
chisq.test(data_02$yes)
chisq.test(data_02$no)

###########################################################################
###########################################################################
# Aetobatus ocellatus data
###########################################################################
###########################################################################

head(dat)

osce <- read.table("oscellatus.txt", header=T, sep='\t')
head(osce)
str(osce)
osce$Dive_ID <- as.factor(osce$Dive_ID)
osce$Presence_O <- as.numeric(osce$Presence_O)
osce$Amount <- as.numeric(osce$Amount)



data <- merge(dat, osce, by.x="Dive_Id", by.y="Dive_ID", all.x=T)
head(data)
str(data)

#data[is.na(data)] <- 0

data$Presence <- as.numeric(as.character(data$Presence))
data$Presence_O <- as.numeric(data$Presence_O)

data$month <- format(as.Date(data$Date), "%m-%d") 
dat_mok_month <- data %>% group_by(Presence, month) %>% summarise(Presence = sum(Presence))
dat_osce_month <- data %>% group_by(Presence_O, month) %>% summarise(Presence_O = sum(Presence_O))

dat_mok_month.agg <- aggregate(cbind(Presence) ~ month, data = dat_mok_month, FUN = sum) 
dat_osce_month.agg <- aggregate(cbind(Presence_O) ~ month, data = dat_osce_month, FUN = sum)

# Check
dat_mok_month[ which(dat_mok_month$month=='01-01'), ] 
dat_mok_month.agg[ which(dat_mok_month.agg$month=='01-01'), ] # ok
# Check
dat_osce_month[ which(dat_osce_month$month=='01-01'), ] 
dat_osce_month.agg[ which(dat_osce_month.agg$month=='01-01'), ] # ok

# Two data frame fusion :
dat_all_month <- merge(dat_osce_month.agg, dat_mok_month.agg, by="month")
my_title <- expression(paste("Temporal distribution of both ", italic("S. mokarran"), " individuals and ", italic("A. oscellatus"), " occurence"))
color_osce <- expression((italic("A. oscellatus")))
color_mok <- expression(paste(italic("S. mokarran")))
# Reorder X axis
dat_all_month$month <- factor(dat_all_month$month, levels=c("12-19", "12-20",
                                                            "12-21", "12-22", "12-23", "12-24", "12-25", "12-26", "12-27", "12-28", "12-29", "12-30", "12-31", "01-01", "01-02", "01-03", "01-04", "01-05", "01-06", "01-07", "01-08", "01-09", "01-10", "01-11", "01-12","01-13", "01-14", "01-15", "01-16", "01-17", "01-18", "01-19", "01-20", "01-21", "01-22", "01-23", "01-24", "01-25", "01-26", "01-27", "01-28", "01-29", "01-30", "01-31", "02-01", "02-02", "02-03", "02-04", "02-05", "02-06", "02-07", "02-08", "02-09", "02-10", "02-11", "02-12", "02-13", "02-14", "02-15", "02-16", "02-17", "02-18", "02-19", "02-20", "02-21", "02-22", "02-23", "02-24", "02-25", "02-26", "02-27", "02-28", "03-01", "03-02", "03-03", "03-04"))
dat_all_month$month <- factor(dat_all_month$month, levels=c("12-19", "12-20",
                                                            "12-21", "12-22", "12-23", "12-24", "12-25", "12-26", "12-27", "12-28", "12-29", "12-30", "12-31", "01-01", "01-02", "01-03", "01-04", "01-05", "01-06", "01-07", "01-08", "01-09", "01-10", "01-11", "01-12","01-13", "01-14", "01-15", "01-16", "01-17", "01-18", "01-19", "01-20", "01-21", "01-22", "01-23", "01-24", "01-25", "01-26", "01-27", "01-28", "01-29", "01-30", "01-31", "02-01", "02-02", "02-03", "02-04", "02-05", "02-06", "02-07", "02-08", "02-09", "02-10", "02-11", "02-12", "02-13", "02-14", "02-15", "02-16", "02-17", "02-18", "02-19", "02-20", "02-21", "02-22", "02-23", "02-24", "02-25", "02-26", "02-27", "02-28", "03-01", "03-02", "03-03", "03-04"))

#install.packages("mdthemes")
library(mdthemes)
library(ggtext)



ggplot(dat_all_month, aes(month, group = 1)) +
  geom_line(aes(y = Presence_O, color = "Osce"), color = "dodgerblue4") +
  geom_line(aes(y = Presence, color = "Mok"), color = "dimgrey") +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.text = element_markdown(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = my_title, x = "Month", y = "Occurence") 

kruskal.test(Presence ~ Presence_O, data = dat_all_month)



# Oscellatus individuals :


# dat_osce_month <- data %>% group_by(Amount, month) %>% summarise(Amount = sum(Amount))
# dat_mok_month.agg <- aggregate(cbind(Presence) ~ month, data = dat_mok_month, FUN = sum) 
# dat_osce_month.agg <- aggregate(cbind(Amount) ~ month, data = dat_osce_month, FUN = sum)
# # Two data frame fusion
# dat_all_month <- merge(dat_osce_month.agg, dat_mok_month.agg, by="month") 
# head(dat_all_month)
# str(dat_all_month)
# my_title <- expression(paste("Spatial distribution of both ", italic("S. mokarran"), " individuals and ", italic("A. oscellatus"), " individuals"))
# color_osce <- expression((italic("A. oscellatus")))
# color_mok <- expression(paste(italic("S. mokarran")))
# # Reorder X axis
# dat_all_month$month <- factor(dat_all_month$month, levels=c("12-19", "12-20",
#                                                             "12-21", "12-22", "12-23", "12-24", "12-25", "12-26", "12-27", "12-28", "12-29", "12-30", "12-31", "01-01", "01-02", "01-03", "01-04", "01-05", "01-06", "01-07", "01-08", "01-09", "01-10", "01-11", "01-12", "01-13", "01-14", "01-15", "01-16", "01-17", "01-18", "01-19", "01-20", "01-21", "01-22", "01-23", "01-24", "01-25","01-26", "01-27", "01-28", "01-29", "01-30", "01-31", "02-01", "02-02", "02-03", "02-04", "02-05", "02-06", "02-07", "02-08", "02-09", "02-10", "02-11", "02-12", "02-13", "02-14", "02-15", "02-16", "02-17", "02-18", "02-19", "02-20", "02-21", "02-22", "02-23", "02-24", "02-25", "02-26", "02-27", "02-28", "03-01", "03-02", "03-03", "03-04"))
# ggplot(dat_all_month, aes(month, group = 1)) +
#   geom_line(aes(y = Amount, colour = "*A. oscellatus*")) +
#   geom_line(aes(y = Presence, colour = "*S. mokarran*")) +
#   # mdthemes::md_theme_classic() +
#   theme(axis.text.x=element_text(angle=90, hjust=1), legend.text = element_markdown(),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(title = my_title, x = "Month", y = "Occurence")
# 
# 
