install.packages("gapminder")
library(gapminder)
data("gapminder")

summary(gapminder)
#mean of gdpPercap
mean(gapminder$gdpPercap)
#assign the code to character x 
x<-mean(gapminder$gdpPercap)
x

#use attach
attach(gapminder)
mean(pop)
median(pop)
hist(lifeExp)
hist(pop)
#log tranormation as it looks right skewed
hist(log(pop))
#boxplot of lifeExp desegregated by continent 
boxplot(lifeExp~continent)
plot(lifeExp~ gdpPercap)
#it's not linear hence do a log transformation
plot(lifeExp~log(gdpPercap))


#Dplyr and the Pipe operator %>%('and then')
install.packages("dplyr")
library(dplyr)
gapminder%>%
  select(country,lifeExp)%>%
  filter(country=="South Africa"| country=="Ireland") %>%
  group_by(country) %>%
  summarize(Average_life=mean(lifeExp))

#The difference is 20
#We want to know whether that difference is statustically significant

#Apply the t-test
#Create a new dataframe 
df1<- gapminder%>%
  select(country,lifeExp)%>%
  filter(country=="South Africa"| country=="Ireland")

t.test(data= df1, lifeExp~country)

#Gggplot2
library(ggplot2)
 gapminder%>%
   filter(gdpPercap < 50000)%>%
   ggplot(aes(x=gdpPercap, y=lifeExp))+
   geom_point()
   
#add color to continents by adding col
 gapminder%>%
   filter(gdpPercap < 50000)%>%
   ggplot(aes(x=gdpPercap, y=lifeExp, col=continent))+
   geom_point()
 
 #make the points transparent by using alpha
 gapminder%>%
   filter(gdpPercap < 50000)%>%
   ggplot(aes(x=gdpPercap, y=lifeExp, col=continent))+
   geom_point(alpha=0.3)
 
 #make size of the dots proportional to the sizeof the population
 gapminder%>%
   filter(gdpPercap < 50000)%>%
   ggplot(aes(x=gdpPercap, y=lifeExp, col=continent, size=pop))+
   geom_point(alpha=0.3)
 #linearize by doing the log of gdpPercapita data
 gapminder%>%
   filter(gdpPercap < 50000)%>%
   ggplot(aes(x=log(gdpPercap), y=lifeExp, col=continent, size=pop))+
   geom_point(alpha=0.3)
 #add a line that tracks the various continents using geom(smooth)
 gapminder%>%
   filter(gdpPercap < 50000)%>%
   ggplot(aes(x=log(gdpPercap), y=lifeExp, col=continent, size=pop))+
   geom_point(alpha=0.3)+geom_smooth()
 #make it into a linear model using lm
 gapminder%>%
   filter(gdpPercap < 50000)%>%
   ggplot(aes(x=log(gdpPercap), y=lifeExp, col=continent, size=pop))+
   geom_point(alpha=0.3)+geom_smooth(method=lm)
 
#divide continents into various facets
   gapminder%>%
     filter(gdpPercap < 50000)%>%
     ggplot(aes(x=log(gdpPercap), y=lifeExp, col=continent, size=pop))+
     geom_point(alpha=0.3)+geom_smooth(method=lm)+
     facet_wrap(~ continent)

   
   
   

