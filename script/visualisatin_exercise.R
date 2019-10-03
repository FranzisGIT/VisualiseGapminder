library(tidyverse)

gapminder1 <- read_csv("data/gapminder.csv")
gapminder2 <- read_csv("data/gapminder_2012.csv")
gapminder <- bind_rows(gapminder1, gapminder2)
rm(gapminder1, gapminder2)

data1977 <- gapminder %>% 
  filter(year==1977)

glimpse(data1977)
str(data1977)  # different way of looking at the structure

ggplot(data = data1977)  #add the data to the plot

ggplot(data = data1977, 
       mapping = aes(x = gdpPercap,         # add the aesetics to the plot
                     y = lifeExp,
                     colour = continent,
                     size = pop
                     )
       )

ggplot(data = data1977, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp,
                     colour = continent,
                     size = pop
                      )
       )+           
  geom_point()                # outside of ggplot function add geometry etc as overlays

ggplot(data = data1977, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp,
                     colour = continent,
                     size = pop
       )
)+           
  geom_point() +
  scale_x_log10()              #scaling the axis (default is unit)

ggplot(data = data1977)+                 # can assign aestetics under geometry
  geom_point(mapping = aes(x = gdpPercap,       
                           y = lifeExp,
                           colour = continent,
                           size = pop
  )) +
  scale_x_log10()

ggplot(data = data1977, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp,
                     colour = continent,
                     size = pop
       )
)+           
  geom_point(alpha=0.8) +
  scale_x_log10()

ggplot(data = data1977, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp,
                     size = pop
       )
)+           
  geom_point(colour = "blue") +
  scale_x_log10()

ggplot(data = data1977, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp,
                     colour = continent,
                     size = pop
       )
)+           
  geom_point(colour="blue") +
  scale_x_log10()

ggplot(data = data1977, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp,
                     shape = continent,
                     colour=continent,
                     size = pop
       )
)+           
  geom_point() +
  scale_x_log10()

ggplot(data = data1977, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp,
                     shape = continent,
                     colour=continent,
                     size = pop
       )
)+           
  geom_point(shape="square") +
  scale_x_log10()

ggplot(data = data1977, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp,
                     shape = continent,
                     colour=continent,
                     size = pop
       )
)+           
  geom_point(shape="star") +
  scale_x_log10()


#challenge 7
ggplot(data=gapminder,
       mapping=aes(x=year,
                   y=lifeExp,
                   colour=continent)
       )+
  geom_point()

# put multiple aestetics onto the same 
ggplot(data=gapminder,
       mapping=aes(x=year,
                   y=lifeExp,
                   group=country,
                   colour=continent)
)+
  geom_line()+
  geom_point(colour="black")

# put multiple aestetics onto the same 
ggplot(data=gapminder,
       mapping=aes(x=year,
                   y=lifeExp,
                   group=country                   )
    )+
  geom_line(aes(colour=continent))+
  geom_point()


ggplot(data=gapminder,
       mapping=aes(x=year,
                   y=lifeExp,
                   colour=continent,
                   size=pop)
)+
  geom_jitter(alpha=0.5)

# viewing the averages only
Av_gapminder <- gapminder %>% 
  group_by(year, continent, country) %>% 
  summarise(avLifeExp=mean(lifeExp),
            avgdpPercap=mean(gdpPercap), 
            avgPop=mean(pop)) 

ggplot(data=Av_gapminder,
       mapping=aes(x=year,
                   y=avLifeExp,
                   colour=continent,
                   size=avgPop)
)+
  geom_point(alpha=0.5)

#new challenge
ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp )
       )+           
  geom_point(alpha=0.3) +
  scale_x_log10()+
  geom_smooth(method="lm",size=2, colour="red")

ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp )
)+           
  geom_point(alpha=0.6,aes(colour=continent), size=3) +
  scale_x_log10()+
  scale_colour_manual(values = c(Oceania="green", 
                                 Africa="purple",
                                 Americas="blue",
                                 Asia="pink",
                                 Europe="black"))

#challenge 9
ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp )
)+           
  geom_point(alpha=0.6,colour="green", size=0.5) +
  scale_x_log10()+
  geom_smooth(method="lm", size=1.5)

#challenge 10
ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp,
                     colour=continent,
                     shape=continent)
      )+           
  geom_point(alpha=0.6, 
             size=1.5) +
  scale_x_log10()+
  geom_smooth(method="lm" )

#challenge 10 version
ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp
                     )
)+           
  geom_point(alpha=0.6, 
             size=1,
             aes(shape=continent))+
  scale_x_log10()+
  geom_smooth(method="lm", aes(colour=continent) )

#challenge 10 version
ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp
       )
)+           
  geom_point(alpha=0.6, 
             size=1,
             aes(shape=continent))+
  scale_x_log10()+
  geom_smooth(method="lm", aes(colour=continent) )+
  scale_shape_manual(values=c(Africa=1, 
                                 Americas=6, 
                                 Asia=3,
                                 Europe=5,
                                 Oceania=2))


#challenge 11
ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap,       
                     y = lifeExp,
                     colour=continent,
                     shape=continent
       )
)+           
  geom_point(alpha=0.6, 
             size=1) +
  scale_x_log10()+
  geom_smooth(method="lm" )+
  scale_colour_brewer(palette="PRGn")

A_countries <-  gapminder %>% 
  filter(str_starts(country, "A"))

ggplot(
  data=A_countries,
  mapping=aes(x=year, 
              y=lifeExp,
              colour=continent,
              group=country)
  )+
  geom_line()
 
# facet: creating same graph but in panels
ggplot(
  data=A_countries,
  mapping=aes(x=year, 
              y=lifeExp,
              colour=continent,
              group=country)
)+
  geom_line()+
  facet_wrap(~country)

ggplot(data = gapminder)+                 # can assign aestetics under geometry
  geom_point(mapping = aes(x = gdpPercap,       
                           y = lifeExp,
                           colour = continent,
                           size = pop
  )) +
  scale_x_log10()+
  facet_wrap(~year)

  
#adding labels
  ggplot(data = data1977,
         mapping = aes(x = gdpPercap,       
                       y = lifeExp,
                       colour = continent,
                       size = pop)
         )+                 
    geom_point() +
    geom_text(aes(label=country))+
  scale_x_log10()
  
data1977_rich <-data1977 %>% 
  filter(gdpPercap>30000)

#adding labels to subset
ggplot(data = data1977,
       mapping = aes(x = gdpPercap,       
                     y = lifeExp,
                     colour = continent,
                     size = pop)
)+                 
  geom_point() +
  geom_text(data=data1977_rich,aes(label=country))+
  scale_x_log10()

# adjusting themes and the way the plot looks 
#ggplots can be assigned to a variable
rough_plot <- 
  ggplot(
    data=A_countries,
    mapping=aes(x=year, 
                y=lifeExp,
                colour=continent,
                group=country)
  )+
  geom_line(size=1.5)+
  facet_wrap(~country)

# now can use rough plot to play with things
rough_plot +scale_colour_brewer(palette="Dark2")

rough_plot+
  labs(title="Figure 1",
       x= "Year",
       y="life expectancy",
       colour="Continent")

# New challenge 1
rough_plot+
  labs(title="Life expectancy by year",
       x= "Year",
       y="life expectancy",
       colour="Continent",
       caption = "Fig 1: Life expectancy by year for countries starting with A out of the Gampinder data set")+
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=13),
        axis.line.x = element_line(colour="green"))

#saving the last created plot: ggsave
ggsave("figures/my_first_plot.png")
ggsave("figures/my_first_plot.jpg")
ggsave("figures/my_rough_plot.jpg", 
       plot=rough_plot, 
       width=12, 
       height=10,
       units= "cm",
       dpi=600)

# identify which is my working directory: getwd()
# setwd() can e sused to set your working directory