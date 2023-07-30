

library(rvest)
library(tidyverse)
library(dplyr)

football = read_html("https://www.espn.in/football/team/squad/_/id/86/esp.real_madrid")

ft = html_nodes(football, css=".Table__TD")
ft
result <-data.frame(html_table(football, header = TRUE)[[2]])


write.csv(result,"D:\\result.csv")
View(result)
# pre-processing 

result[result == '--'] <- NA
View(result)
sum(is.na(result))

result <- na.omit(result)


# Smooth noisy data
result$HT <- sub("[[:space:]].*", "", result$HT)

result$WT <- sub("[[:space:]].*", "", result$WT)
result$Name <-gsub("[1-50]","",as.character(result$Name))

result$G <- as.numeric(result$G)
result$A <- as.numeric(result$A)

result$Prof <- result$G + result$A


result$age_group <- cut(result$Age, breaks = c(0, 25, 40, 100), labels = c("Young", "Middle", "old"))
result


# data transformation
result$POS <- factor(result$POS, ordered = TRUE)

result$HT <- as.numeric(result$HT)
result$WT <- as.numeric(result$WT)

result$NAT <- factor(result$NAT, ordered = TRUE)

result$age_group <- factor(result$age_group,
                      levels =c("Young", "Middle", "old"),labels=c("Young Campaigner","Senior Campaigner","Old Campaigner"))

#rename some columns
colnames(result)[5] <- "Weight(kg)"
colnames(result)[9] <- "Goal"
colnames(result)[10] <- "Assists"
colnames(result)[6] <- "Nation"
colnames(result)[15] <- "Yellow Card"
colnames(result)[16] <- "Red Card"
colnames(result)[17] <- "performace"

#data reduction
result <- subset(result, select = -c(ST))
result <- subset(result, select = -c(SH))


View(result)

#Data Discretization
###Mean

MeanAge <- mean(result$Age)
MeanAge

###########################################

# Check the data type of my_object
class(result$HT)

# If the data type is not numeric or logical, convert it to a numeric or logical object
result$HT <- as.numeric(result$HT)

# Check for missing values in my_object
is.na(result$HT)

# If there are missing values, remove them using na.omit()
result$HT <- na.omit(result$HT)

# Calculate the mean of my_object
mean(result$HT)

####################################

# Check the data type of my_object
class(result$Weight)

# If the data type is not numeric or logical, convert it to a numeric or logical object
result$WT <- as.numeric(result$Weight)

# Check for missing values in my_object
is.na(result$Weight)

# If there are missing values, remove them using na.omit()
result$WT <- na.omit(result$Weight)

# Calculate the mean of my_object
mean(result$Weight)

################################
#Median

l <- sort(result$FC)
l <-median(l)
l

median(result$FA)


###############################
#Mode:

mode <- function(x){
  unique_values <- unique(x)
  table <- tabulate(match(x, unique_values))
  unique_values[table == max(table)]
}

mode(result$Nation)

##########################

#range
rgoal <- max(result$Goal) - min(result$Goal)
rgoal

result$APP <- as.numeric(result$APP)

rapp <- max(result$APP) - min(result$APP)
rapp

result$FC <- as.numeric(result$FC)

rfoulc <- max(result$FC)- min(result$FC)
rfoulc

result$FA <- as.numeric(result$FA)

rfouls <- max(result$FA)- min(result$FA)
rfouls

#######################

#Quartile & Percentile:

quantile(result$Age, prob = c(0.0,0.25,0.50, 0.75 , 0.100))
quantile(result$Weight.kg., prob = c(0.0,0.25,0.50, 0.75 , 0.100))
quantile(result$ Yellow.Card)

############################################################

#Interquartile Range:

IQR(result$Age)

#####################################

#Variance:

var(result$Age)
var(result$HT)
var(result$Weight)

#####################################

#Standard Deviation:
  
sd(result$Age)
sd(result$HT)
sd(result$Weight)

#####################################

#Normal Distribution:

x = rnorm(result$Age, mean = mean(result$Age), sd=
               sd(result$Age))
hist(x)

z = rnorm(result$Goal, mean = mean(result$Goal),sd = sd(result$Goal) )
hist(z)

y = dnorm(result$APP , mean = mean(result$APP), sd= sd(result$APP))
plot(result$APP,y)
###################################3

#Data Visualization:
library(ggplot2)
#1)	First lets draw a scatter plot of Appearance vs Goal for each team,

ggplot(result, aes(x = APP, y= Goal, shape = POS,color=POS, linetype = POS))+
geom_point(alpha = 0.7)+
geom_smooth(method =lm, se= FALSE)+
scale_x_continuous(breaks = seq(0,150,20))+
scale_y_continuous(breaks = seq(0,150,20))+
scale_color_manual(values = c("red","green","blue"))+
facet_wrap(~age_group)

#2)	Now we see a scatter plot for Defenders Appearance vs Fouls Committed,

ggplot(result, aes(x = APP, y= FC, shape = POS,color=POS, linetype = POS))+
  geom_point(alpha = 0.7)+
  geom_smooth(method =lm, se= FALSE)+
  scale_x_continuous(breaks = seq(0,150,20))+
  scale_y_continuous(breaks = seq(0,150,20))+
  scale_color_manual(values = c("red","green","blue"))+
  facet_wrap(~age_group)

#3)	Next, we try to measure and analyze the age categories that the players belong to:

library(ggpie)
library(dplyr)

result %>% ggpie(group_key = "age_group",count_type = "full", label_type = "circle",
               label_info = "ratio", label_pos = "out", nudge_x = 10)

#4)	Furthermore, we try to identify the most number of players from and individual country.

result %>% ggpie(group_key = "Nation",count_type = "full", label_type = "circle",
               label_info = "ratio", label_pos = "out", nudge_x = 10)

#5)	Now the most important part of the visualization. We need to see the contribution of the forwards for their respective teams.

library(ggplot2)
ggplot(result,aes(x=performace, fill=age_group))+
  geom_bar()+
  labs(title = "Contribution Of Forwards", x ="performance", y="Frequency")+
  coord_flip()

#6)	Now we run a comparison between Barca and Real’s two most prolific players, CRISTIANO RONALDO and LIONEL MESSI Goals between Messi & Ronaldo,

player1 <- result[(result$Name=="Dani Carvajal"),]
player1

player2 <- result[(result$Name=="Jesús Vallejo"),]
player2

mr <- rbind(player1,player2)
mr
g=(mr$Goal+mr$Assists)
ggplot(mr,aes(x= mr$Name, y= g, fill= mr$Name))+
  geom_bar(stat = "identity")+
  labs(x="Names",y="Goals", title = "player1 Vs player2")  


#7)	Now we visualize the performance of senior and old campaigners,

ggplot(result, aes(x= age_group, fill= performace))+
  geom_bar(position = "dodge")+
  facet_wrap(~age_group)

#8)	Most fouls suffered between Messi and Ronaldo


barplot(mr$FA, names.arg = mr$Name)

#9)We visualize the minimum height of both teams through a bar plot:

c = result$age_group
plotresult <- result %>%
  group_by(result$age_group) %>%
  summarise(mean=mean(HT))
View(plotresult)

plotresult<-rename(plotresult, "Tname"="result$age_group")

ggplot(plotresult, aes(x= reorder(Tname, mean), y= mean))+
  geom_bar(stat="identity")+
  labs(x="age_group",y="", title = "Mean Height")

#9)Mean Goal

c = result$age_group
plotresult2 <- result %>%
  group_by(result$age_group) %>%
  summarise(mean=mean(Goal))
View(plotresult2)

plotresult2<-rename(plotresult2, "Tname"="result$age_group")
ggplot(plotresult2, aes(x= reorder(Tname, mean), y= mean))+
  geom_bar(stat="identity")

#10)	We all love players that can do both which is attack and defend. Here we try to find top goal-scoring defenders among both teams.

result %>% filter(result$Goal>=2 & result$POS == "D") %>% 
  ggplot(aes(x= Name, y= Goal, fill=Name))+
  geom_bar(stat = "identity")+
  labs(x="Names",y="Goals", title = "Goal Scoring Defenders")

#11)Next we try to figure out the most amount of goals scored by countries.

result %>% ggplot(aes(x= Nation, y= Goal, fill=Nation))+
  geom_bar(stat = "identity")+
  labs(x="Nationality",y="Goals", title = "Goal Scorers By Nationality")+
  facet_wrap(~age_group)+
  coord_flip()

#12) Now we compare the forward of the club based on goals,Forward Comparison

result %>% filter(result$POS =="F" & result$Goal >mean(result$Goal)) %>% 
  ggplot(aes(x= Name, y= Goal, fill=Name))+
  geom_bar(stat = "identity")+
  labs(x="Players",y="Goals", title = "Forward Comparision")+
  facet_wrap(~age_group)+
  coord_flip()

#13)	Density plot of defenders’ Appearance vs Yellow Cards


result %>% filter(result$POS =="D") %>% ggplot(aes(x=APP, y= Yellow.Card))+
  geom_density(stat = "identity", fill="red", bw= 0.5)+
  labs(x="Appearance",y="Yellow.Card", title = "Defenders Appearance Vs Yellow Cards")




  