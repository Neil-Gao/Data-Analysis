setwd("/Users/aimhighfly/Documents/StonyBrookUniversity/Spring_semester/EST508/Projects/movie")
library(dplyr)
library(ggplot2)
library(ggthemes)

#____________________________________________________________________________________________________________________
# action
df.action<-read.csv("action.csv", stringsAsFactors = F)
str(df.action)
colnames(df.action) <- df.action[nrow(df.action),]
df.action1 = df.action[-nrow(df.action),]
str(df.action1)
df.action.color <- df.action1[,-1:-3]
df.action.color <- as.data.frame(lapply(df.action.color,function(x) as.numeric(x)))
str(df.action.color)
action.min <- data.frame('color' = apply(df.action.color, 1,function(x)which.min(x)))
action.min
action.names = data.frame('color'=colnames(df.action.color)[action.min$color], stringsAsFactors = F)
str(action.names)
col_action<-action.names%>%group_by(color)%>%summarise(count=n())
col_action

ggplot(col_action, aes(x=reorder(color,-count),y=count)) + 
  geom_bar(aes(fill = color), colour="black", stat="identity",position='dodge')+
  # theme_economist(dkpanel=T)+
  scale_fill_manual(values = c("black"= "#000000",
                               "gray" ="#808080",
                               "white" ="#FFFFFF",
                               "silver"= "#C0C0C0",
                               "maroon" ="#800000",
                               "red" ="#FF0000",
                               "olive"= "#808000",
                               "yellow" ="#FFFF00",
                               "green" ="#008000",
                               "teal"= "#008080",
                               "aqua" ="#00FFFF",
                               "navy" ="#000080",
                               "blue"= "#0000FF",
                               "purple" ="#800080",
                               "fuchsia" ="#FF00FF",
                               "orange"="#FFA500",
                               "brown" ="#8B4513"),guide=FALSE)+labs(x="Basic Color",y="Count")
#____________________________________________________________________________________________________________________
# animation
df.animation<-read.csv("animation.csv", stringsAsFactors = F)
str(df.animation)
colnames(df.animation) <- df.animation[nrow(df.animation),]
df.animation1 = df.animation[-nrow(df.animation),]
str(df.animation1)
df.animation.color <- df.animation1[,-1:-3]
df.animation.color <- as.data.frame(lapply(df.animation.color,function(x) as.numeric(x)))
str(df.animation.color)
animation.min <- data.frame('color' = apply(df.animation.color, 1,function(x)which.min(x)))
animation.min
animation.names = data.frame('color'=colnames(df.animation.color)[animation.min$color], stringsAsFactors = F)
str(animation.names)
col_animation<-animation.names%>%group_by(color)%>%summarise(count=n())
col_animation
ggplot(col_animation, aes(x=reorder(color,-count),y=count)) + 
  geom_bar(aes(fill = color), colour="black", stat="identity",position='dodge')+
  scale_fill_manual(values = c("black"= "#000000",
                               "gray" ="#808080",
                               "white" ="#FFFFFF",
                               "silver"= "#C0C0C0",
                               "maroon" ="#800000",
                               "red" ="#FF0000",
                               "olive"= "#808000",
                               "yellow" ="#FFFF00",
                               "green" ="#008000",
                               "teal"= "#008080",
                               "aqua" ="#00FFFF",
                               "navy" ="#000080",
                               "blue"= "#0000FF",
                               "purple" ="#800080",
                               "fuchsia" ="#FF00FF",
                               "orange"="#FFA500",
                               "brown" ="#8B4513"),guide=FALSE)+labs(x="Basic Color",y="Count")

#____________________________________________________________________________________________________________________
# comedy
df.comedy<-read.csv("comedy.csv", stringsAsFactors = F)
str(df.comedy)
colnames(df.comedy) <- df.comedy[nrow(df.comedy),]
df.comedy1 = df.comedy[-nrow(df.comedy),]
str(df.comedy1)
df.comedy.color <- df.comedy1[,-1:-3]
df.comedy.color <- as.data.frame(lapply(df.comedy.color,function(x) as.numeric(x)))
str(df.comedy.color)
comedy.min <- data.frame('color' = apply(df.comedy.color, 1,function(x)which.min(x)))
comedy.min
comedy.names = data.frame('color'=colnames(df.comedy.color)[comedy.min$color], stringsAsFactors = F)
str(comedy.names)
col_comedy<-comedy.names%>%group_by(color)%>%summarise(count=n())
col_comedy
ggplot(col_comedy, aes(x=reorder(color,-count),y=count)) + 
  geom_bar(aes(fill = color), colour="black", stat="identity",position='dodge')+
  scale_fill_manual(values = c("black"= "#000000",
                               "gray" ="#808080",
                               "white" ="#FFFFFF",
                               "silver"= "#C0C0C0",
                               "maroon" ="#800000",
                               "red" ="#FF0000",
                               "olive"= "#808000",
                               "yellow" ="#FFFF00",
                               "green" ="#008000",
                               "teal"= "#008080",
                               "aqua" ="#00FFFF",
                               "navy" ="#000080",
                               "blue"= "#0000FF",
                               "purple" ="#800080",
                               "fuchsia" ="#FF00FF",
                               "orange"="#FFA500",
                               "brown" ="#8B4513"),guide=FALSE)+labs(x="Basic Color",y="Count")



#____________________________________________________________________________________________________________________
# horror
df.horror<-read.csv("horror.csv", stringsAsFactors = F)
str(df.horror)
colnames(df.horror) <- df.horror[nrow(df.horror),]
df.horror1 = df.horror[-nrow(df.horror),]
str(df.horror1)
df.horror.color <- df.horror1[,-1:-3]
df.horror.color <- as.data.frame(lapply(df.horror.color,function(x) as.numeric(x)))
str(df.horror.color)
horror.min <- data.frame('color' = apply(df.horror.color, 1,function(x)which.min(x)))
horror.min
horror.names = data.frame('color'=colnames(df.horror.color)[horror.min$color], stringsAsFactors = F)
str(horror.names)
col_horror<-horror.names%>%group_by(color)%>%summarise(count=n())
col_horror
ggplot(col_horror, aes(x=reorder(color,-count),y=count)) + 
  geom_bar(aes(fill = color), colour="black", stat="identity",position='dodge')+
  scale_fill_manual(values = c("black"= "#000000",
                               "gray" ="#808080",
                               "white" ="#FFFFFF",
                               "silver"= "#C0C0C0",
                               "maroon" ="#800000",
                               "red" ="#FF0000",
                               "olive"= "#808000",
                               "yellow" ="#FFFF00",
                               "green" ="#008000",
                               "teal"= "#008080",
                               "aqua" ="#00FFFF",
                               "navy" ="#000080",
                               "blue"= "#0000FF",
                               "purple" ="#800080",
                               "fuchsia" ="#FF00FF",
                               "orange"="#FFA500",
                               "brown" ="#8B4513"),guide=FALSE)+labs(x="Basic Color",y="Count")

# combine
action.comb <- data.frame(col_action, "type"="action", stringsAsFactors = F)
str(action.comb)
animation.comb <- data.frame(col_animation, "type"="animation", stringsAsFactors = F)
animation.comb
comedy.comb <- data.frame(col_comedy, "type"="comedy", stringsAsFactors = F)
comedy.comb
horror.comb <- data.frame(col_horror, "type"="horror", stringsAsFactors = F)
horror.comb
combination = rbind.data.frame(action.comb, animation.comb, comedy.comb, horror.comb)
ggplot(combination, aes(x=reorder(color,-count),y=count)) + facet_grid(. ~ type) + 
  geom_bar(aes(fill = color), colour="black", stat="identity",position='dodge')+
  scale_fill_manual(values = c("black"= "#000000",
                               "gray" ="#808080",
                               "white" ="#FFFFFF",
                               "silver"= "#C0C0C0",
                               "maroon" ="#800000",
                               "red" ="#FF0000",
                               "olive"= "#808000",
                               "yellow" ="#FFFF00",
                               "green" ="#008000",
                               "teal"= "#008080",
                               "aqua" ="#00FFFF",
                               "navy" ="#000080",
                               "blue"= "#0000FF",
                               "purple" ="#800080",
                               "fuchsia" ="#FF00FF",
                               "orange"="#FFA500",
                               "brown" ="#8B4513"),guide=FALSE)+labs(x="Basic Color",y="Count")







