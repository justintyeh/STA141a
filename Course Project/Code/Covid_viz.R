library(tidyverse)
library(gridExtra)
library(scales)
library(lubridate)
library(ggplot2)
library(htmltab)
library(readxl)
library(gsubfn)
library(car)


covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
write_csv(covid,file="./Data/WHO-Covid-19-backup.csv")


covid <- covid %>% 
  filter(WHO_region != "Other") %>% 
  mutate(WHO_region = fct_recode(WHO_region,
                                 "Eastern Mediterranean"="EMRO",
                                 "Europe" = "EURO","Africa" = "AFRO",
                                 "Western Pacific" = "WPRO","Americas"="AMRO",
                                 "South-East Asia" = "SEARO"),
         Country = gsub("United States of America", "United States", `Country`)) %>%
  group_by(Country) %>%
  summarise(Cumulative_Cases=sum(New_cases))

str(covid)

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_real_GDP_growth_rate"
rate.gdp <- htmltab(url, which = 2) %>% as_tibble%>%slice(-25) %>%rename(Country = 'Country/region')%>% 
  mutate(gdp.growth.rate = as.numeric(gsub("−","-", `Real GDP growthrate (%)`)))

exchange.rates <- read_excel("./Data/exchangerates.xlsx") %>% rename(Country = "...1")
exchange.rates2021<- exchange.rates[,c(1,7)] %>%
  mutate(exchange.rate = as.numeric(`2021`)) 
#which(is.na(exchange.rates2021$`2021`))
#countries.na <- exchange.rates2021[is.na(exchange.rates2021$rate), ] 

gdp.covid <- merge(rate.gdp[,c(2,4)], covid, by = "Country")
gdp.covid.exr <- merge(gdp.covid, exchange.rates2021[,c(1,3)], by = "Country")
#remove missing values
avai_id <- which(!is.na(gdp.covid.exr$exchange.rate))
data <- gdp.covid.exr[avai_id,]
length(unique(data$Country))

url.reserves <- "https://en.wikipedia.org/wiki/List_of_countries_by_foreign-exchange_reserves"
reserves <- htmltab(url.reserves, which = 1) %>% as_tibble%>%
  mutate(ex.res = as.integer(gsub(",", "", `Foreign exchangereserves(millions of US$)`)),
         Country = str_trim(`Country or region`, side = 'left')) %>%print

ggr.cc.er.eres <- merge(data,reserves[,c(6,7)],by = "Country")

inflation <- read_excel("./Data/inflation.xls") %>% filter
inf <- inflation[,c(1,43)]%>%rename(Country = "Inflation rate, average consumer prices (Annual percent change)") %>%
  mutate(inflation = as.numeric(gsub(",", "",`2021`)))

ggr.cc.er.eres.ir <- merge(ggr.cc.er.eres, inf[,c(1,3)], by = "Country")
str(inf)
url.fdi <- "https://en.wikipedia.org/wiki/List_of_countries_by_received_FDI"
fdi <- htmltab(url.fdi, which = 2) %>% as_tibble%>% 
  mutate(fdi = as.numeric(gsub(",", "",`Stock of FDI at home  (millions of USD)`)),
         Country = str_trim(`Country`, side = 'left'))
  
ggr.cc.er.eres.ir.fdi <- merge(ggr.cc.er.eres.ir, fdi[,c(2,5)], by = "Country")

url.smc <- "https://en.wikipedia.org/wiki/List_of_countries_by_stock_market_capitalization"
smc <- htmltab(url.smc, which = 1) %>% as_tibble%>%
  mutate(smc = as.numeric(gsub(",", "",`Total market cap(in mil. US$)`)),
         Country = str_trim(`Country`, side = 'left'))

ggr.cc.er.eres.ir.fdi <- merge(ggr.cc.er.eres.ir.fdi, smc[,c(2,7)], by = "Country")
head(ggr.cc.er.eres.ir.fdi)

summary(ggr.cc.er.eres.ir.fdi)

model <- lm(gdp.growth.rate~Cumulative_Cases+exchange.rate+ex.res+inflation+fdi+smc,data = ggr.cc.er.eres.ir.fdi)
anova(model)
summary(model)
par(mfrow=c(2,2))
plot(model)
vif(model)

mod.covid <- lm(gdp.growth.rate~Cumulative_Cases,data=ggr.cc.er.eres.ir.fdi)
summary(mod.covid)
attach(ggr.cc.er.eres.ir.fdi)
plot(x = Cumulative_Cases, y = gdp.growth.rate)

## Preprocessing #### 
covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
write_csv(covid,file="./Data/WHO-Covid-19-backup.csv")


covid <- covid %>% 
  filter(WHO_region != "Other") %>% 
  mutate(WHO_region = fct_recode(WHO_region,"Eastern Mediterranean"="EMRO","Europe" = "EURO","Africa" = "AFRO","Western Pacific" = "WPRO","Americas"="AMRO","South-East Asia" = "SEARO"))

summarise(covid)

range(covid$Date_reported)
length(unique(covid$Country))



## Scatterplot ####
fig.scatter.1 <- covid %>% 
  filter(Date_reported=="2021-01-28", WHO_region=="Africa") %>% 
  ggplot(aes(x=New_cases,y=New_deaths)) +
  geom_point()+
  geom_text(aes(label=Country),hjust=0, vjust=0)


fig.scatter.1


fig.scatter.2 <- covid %>% 
  filter(Date_reported=="2021-01-23", WHO_region=="Africa") %>% 
  ggplot(aes(x=Cumulative_cases,y=Cumulative_deaths)) +
  geom_point()+
  geom_text(aes(label=Country),hjust=0, vjust=0)

 fig.scatter.2

gridExtra::grid.arrange(fig.scatter.1, fig.scatter.2, nrow=1, ncol=2)


## Maps #### 
library(maps)
world <- map_data("world");
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) 
worldplot

covid.today<- covid %>% 
  filter(Date_reported == "2021-1-28") %>% 
  mutate(region=Country)


covid.today.world<- inner_join(world, covid.today, by = "region")

fig.map  <- ggplot() +
  geom_polygon(data = covid.today.world, aes(x=long, y = lat, group = group,fill=New_deaths)) + 
  coord_fixed(1.3)
fig.map

# Q: Why is USA not shown in the map?
# Check out:  
setdiff(unique(covid.today$Country),unique(world$region))

## Spaghetti plot

fig.spaghetti.1 <- covid %>% 
  filter(Date_reported>= "2021-01-01", Date_reported<= "2021-01-28", WHO_region=="Africa") %>% 
  mutate(Date=as.Date(Date_reported)) %>%
  ggplot(aes(x=Date,y=New_cases,by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
fig.spaghetti.1   

fig.spaghetti.2 <- covid %>% 
  filter(Date_reported>= "2021-01-01", Date_reported<= "2021-01-28", WHO_region=="Africa") %>% 
  mutate(Date=as.Date(Date_reported)) %>%
  ggplot(aes(x=Date,y=New_deaths,by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
fig.spaghetti.2  

## Interactive plot ####
library(plotly)
covid %>% 
  filter(Date_reported>= "2021-01-01", Date_reported<= "2021-01-28") %>% 
  group_by(Date_reported,WHO_region) %>%   summarize(deaths = sum(New_deaths),
            cases = sum(New_cases)) %>% 
  mutate(Days_2021 = Date_reported- as.Date("2021-01-01")) %>%
  plot_ly(
    x= ~cases,
    y= ~deaths,
    frame = ~Days_2021,
    text=~WHO_region,
    hoverinfo="WHO_region",
    color=~WHO_region,
    type = 'scatter',
    mode = 'markers',
    showlegend = T
  )
