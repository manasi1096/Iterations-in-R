#AIR QUALITY DATASET

airquality = airquality[complete.cases(airquality),]

sd(airquality$Ozone)
sd(airquality$Solar.R)
sd(airquality$Wind)
sd(airquality$Temp)
sd(airquality$Month)
sd(airquality$Day)

head(airquality)

stddev = vector("double", ncol(airquality))
for(i in seq_along(airquality))             
{
  stddev[[i]] = sd(airquality[[i]])          
  
}
stddev

sapply(airquality, sd)

stddev =vector("double", ncol(airquality))
median =vector("double", ncol(airquality))
for(i in seq_along(airquality))
{
  stddev[[i]] = sd(airquality[[i]])
  median[[i]] = median(airquality[[i]])
}
stddev
median


f <- function(x){
  list(sd(x),median(x))
}
sapply(airquality, f)


map_df(airquality, ~list(med = median(.x), sd = sd(.x)))

#GAPMINDER DATASET

library(gapminder)

gapminder = gapminder[complete.cases(gapminder),]

list = c("continent", "year")
DF= data.frame()
for( i in list)
{
  df = gapminder %>% group_by_at(i) %>% 
    top_n(1, gdpPercap) %>% 
    mutate(Remark = paste0("Country Max GDP Per capita in the ",i)) %>% 
    data.frame()
  DF = rbind(df,DF)
}
DF


do.call(rbind, lapply(list, function(x)
{
  gapminder %>% group_by_at(x) %>% 
    top_n(1, gdpPercap)%>%
    mutate(Remark = paste0("Country with the max GDP Per capita in the ",x)) %>% 
    data.frame}))


gapminder$year = as.character(gapminder$year)
map_dfr(list, ~gapminder %>% group_by(!!sym(.x)) %>% 
          top_n(1, gdpPercap)%>%
          mutate(Remark = paste0("Country with the max GDP Per capita in the ",.x)) %>% data.frame()
        
        
        
gapminder %>% 
split(.$Continent) %>% 
map(~lm(gdpPercap ~ lifeExp, data = .))
        
 gapminder %>% 
keep(is.factor) %>% 
str()
        
gapminder%>% 
some(is_character)
        
gapminder %>% 
every(is.integer))