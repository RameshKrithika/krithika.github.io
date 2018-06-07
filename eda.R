library('ProjectTemplate')
create.project('FoodAnalytics',merge.strategy = 'allow.non.conflict')
library(readr)
menu <- read_csv("C:/Krithika/Applied_project-R/menu.csv",
                 +     col_types = cols(`Serving Size` = col_character()))
View(menu)
head(menu)
str(menu)
summary(menu)
library(lattice)
##Relation among the variables
splom(~menu[c(4,6,11,13,15,17,19,20)],groups = NULL, data = menu, axis.line.tck = 0, axis.text.alpha = 0)
library(corrplot)
cr <- cor(menu[c(4,6,11,13,15,17,19,20)])
corrplot(cr,method = "number")
##Result: Total corelation Fat = 0.9, protein = 0.79 and carbohydrates = 0.78
##Analyze Breakfast menu
library(ggplot2)
ggplot(subset(menu,Category=="Breakfast"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Breakfast menu Vs Calories") + xlab("Item") + ylab("Calories")+ coord_flip()
##Analyze salad menu
ggplot(subset(menu,Category=="Salads"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Salads Menu Vs Calories") + xlab("Item") + ylab("Calories")  + coord_flip()
##Analyze snacks menu
ggplot(subset(menu,Category=="Snacks & Sides"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Snacks & Sides Menu Vs Calories") + xlab("Item") + ylab("Calories")  + coord_flip()
##Analyzing Beverages menu
ggplot(subset(menu,Category=="Beverages"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Beverages Menu Vs Calories") + xlab("Item") + ylab("Calories")  + coord_flip()
## Coffee& Tea
Coffeeandtea<-subset(menu,Category=="Coffee & Tea")
Coffeeandtea <- Coffeeandtea[order(-Coffeeandtea$Calories),]
Coffeeandtea<-Coffeeandtea[1:20,]
ggplot(Coffeeandtea, aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Coffee & Tea Menu Vs Calories") + xlab("Item") + ylab("Calories")  + coord_flip()
##Smmothies & Shakes
library(gridExtra)
library(grid)
ggplot(subset(menu,Category=="Smoothies & Shakes"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Smoothies & Shakes Menu Vs Calories") + xlab("Item") + ylab("Calories")  +  coord_flip()
###Boxplot - calories by category
options(repr.plot.height=3, repr.plot.width=6)
ggplot(menu, aes(x = reorder(Category, Calories), y = Calories)) +geom_boxplot() +coord_flip()
## Bar chart of Caloric values for specific menu Items from Chicken & Fish food category.The red bars menu equal or above 600 calories
library(repr)
library(plotly)
options(repr.plot.height=4, repr.plot.width=6)
menu %>%
  filter(.,Category=="Chicken & Fish") %>%
  ggplot(aes(x = reorder(Item, Calories), y = Calories)) +
  geom_bar(aes(fill=Calories<600), width=0.5, stat = "identity") +
  coord_flip()
## Analyzing lowest calorie combination meal

library(sqldf)
Mincal<-aggregate(menu$Calories, by=list(menu$Category), FUN=min)
colnames(Mincal)[1]<-"Category"
colnames(Mincal)[2]<-"Calories"
Mincalmenu<-sqldf("select a.Category,a.Item, a.Calories from menu a, Mincal b where a.Category==b.Category and a.Calories==b.Calories" )
## The below table gives each category minimum value
Mincalmenu
library(ggplot2)
##qplot(Calories,Dietary Fiber (% Daily Value), data=menu, color=Category,facets=~Category, size=I(1),
##      xlab="Calories", ylab="Dietary Fiber (% Daily Value)")
