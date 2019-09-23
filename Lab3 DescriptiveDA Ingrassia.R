install.packages("gapminder")
library(tidyverse)
library(gapminder)
library(dplyr)

??gapminder

summary(gapminder)

glimpse(gapminder)

head(gapminder)
tail(gapminder)

sum(is.na(gapminder))

gp_cnt_life <- select(gapminder, country, lifeExp)
head(gp_cnt_life)

gp_no_pop <-select(gapminder, -pop)
head(gp_no_pop)

gp_1957 <-gapminder %>% filter(year=="1957")
head(gp_1957)

glimpse(gp_1957)

gp_us <- gapminder %>% filter(country=="United States")
head(gp_us, 7)

gp_1957_asia <- gapminder %>% filter(year=="1957", continent=="Asia")
head(gp_1957_asia, 15)

write.csv(gp_1957_asia, 'gapminder1957Asia.csv')

gapminder %>% arrange(pop)

gapminder %>% arrange(desc(pop))

gapminder %>% filter(year=="1957") %>% arrange(desc(pop))

gapminder %>% mutate(popMil = pop/1000000)

gapminder %>% mutate(gdp = pop * gdpPercap)

gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>%
  filter(year=="1957") %>%
  arrange(desc(gdp))
  
gapminder %>% 
  summarise(meanlifeExp = mean(lifeExp))

gapminder %>% filter(year=="1957") %>% 
  summarise(meanlifeExp = mean(lifeExp))

gapminder %>% filter(year=="2007") %>% 
  summarise(meanlifeExp = mean(lifeExp))

gapminder %>% filter(year=="1957") %>% 
  group_by(continent) %>%
  summarise(meanlifeExp = mean(lifeExp), 
            totalPop = sum(as.numeric(pop))) 

# below is my practice finding the median lifeExp and gdp in the year 2000.  I was not able to retrieve data for 2000 so I used 1997

gapminder %>% filter(year=="1997" , country=="United States") %>% 
  mutate(gdp = pop * gdpPercap) %>%
  summarise(meanlifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))

gapminder %>% filter(year=="1997" , country=="United States") %>% 
  mutate(gdp = pop * gdpPercap) %>%
  summarise(meangdp = mean(gdp),
            totalPop = sum(as.numeric(pop))) 

gapminder %>%
  group_by(continent,year) %>%
  summarise(meanlifeExp = mean(lifeExp), 
            totalPop = sum(as.numeric(pop))) 

gapminder %>% 
  group_by(continent,year) %>%  
  mutate(gdp = pop * gdpPercap) %>%
  summarise(meangdp = mean(gdp),
            totalPop = sum(as.numeric(pop)))

by_year <- gapminder %>%
  group_by(year) %>% 
  summarize(totalPop = sum(as.numeric(pop)),
            meanLifeExp = mean(lifeExp))
            
head(by_year)

ggplot(by_year, aes(x=year, y=totalPop)) +
  geom_point()

ggplot(by_year, aes(x=year, y=totalPop)) +
  geom_point() +
  expand_limits(y=0)

by_year_continent <- gapminder %>%
  group_by(year, continent) %>% 
  summarize(totalPop = sum(as.numeric(pop)),
            meanLifeExp = mean(lifeExp)) 

head(by_year_continent)

ggplot(by_year_continent, aes(x=year, y=totalPop, color=continent)) +
  geom_point() +
  expand_limits(y=0)

#lauren practice gdp per cap over time

gdp_by_year_cont <- gapminder %>%
  group_by(year, continent) %>% 
  summarize(gdpPercap = sum(as.numeric(pop)),
            meangdpPercap = mean(gdpPercap))
head(gdp_by_year_cont)

ggplot(gdp_by_year_cont, aes(x=year, y=gdpPercap, color=continent)) +
  geom_line() +
  expand_limits(y=0)

#Help for Exploritory Data 1
summary(gapminder)

#Help for Exploritory Data 2

gap2007 <- gapminder %>% filter(year=="2007")
head(gap2007)

gap2007[which.min(gap2007$lifeExp),]
gap2007[which.min(gap2007$pop),]
gap2007[which.min(gap2007$gdpPercap),]

gap2007[which.max(gap2007$lifeExp),]
gap2007[which.max(gap2007$pop),]
gap2007[which.max(gap2007$gdpPercap),]

#Help for Exploritory Data 3

start_year <- min(gapminder['year'])
end_year <- max(gapminder['year'])
start_pop <- min(gapminder['pop'])
end_pop <- max(gapminder['pop'])

pop_growth_rate <- (end_pop-start_pop)/(end_year-start_year)
pop_growth_rate

#Help for Exploritory Data 4 Viz Plots

gdp_grouped <- gapminder %>%
  group_by(continent,year) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)),
            meanGdpPercap = mean(gdpPercap))
summary(gdp_grouped)
            
ggplot(gdp_grouped, aes(x=year, y=meanGdpPercap, color=continent)) +
  geom_point() +
  expand_limits(y=0)           

ggplot(gdp_grouped, aes(x=year, y=meanLifeExp, color=continent)) +
  geom_point() +
  expand_limits(y=0)     

ggplot(gdp_grouped, aes(x=year, y=totalPop , color=continent)) +
  geom_point() +
  expand_limits(y=0)                
  
#Method 2

gapminder %>% 
  group_by(continent) %>%
  summarize(maxLifeExp = max(lifeExp),
            minLifeExp = min(lifeExp), 
            meanLifeExp = mean(lifeExp))

gapminder %>% 
  group_by(continent) %>%
  summarize(maxPop = max(pop),
            minPop = min(pop), 
            meanPop = mean(pop))

gapminder %>% 
  group_by(continent) %>%
  summarize(maxGdpPerCap = max(gdpPercap),
            minGdpPerCap = min(gdpPercap), 
            meanGdpPerCap = mean(gdpPercap))

#Method 3
my_continents <- c("Africa", "Americas", "Asia", "Europe", "Oceania")

sapply(my_continents, funtion(cont){gapminder %>% filter(continent==cont) %>% summary()})
summary(gapminder)