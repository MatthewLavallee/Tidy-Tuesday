library(tidyverse)
library(ggthemes)
library(ggtext)
library(scales)
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')
#load in data

falls<-tbi_year%>%filter(injury_mechanism=='Unintentional falls')

ggplot(falls)+geom_line(aes(year,rate_est,color=type))+theme_economist()+theme(legend.position = 'none',plot.subtitle = element_textbox_simple())+labs(y='Rate of Cases\n(Per 100,000)',x='Year',title='Rates of Traumatic Brain Injuries\ndue to Unintentional Falls',subtitle = '<span style=\'color: #00BA38;\'>**Emergency Department Visits**</span>, <span style=\'color: #F8766D;\'>**Deaths**</span>, and <span style=\'color: #619CFF;\'>**Hospitalizations**</span>',caption = 'Source: Centers for Disease Control and Prevention.')
ggsave('Falls.png')

ggplot(tbi_military,aes(x=service,y=diagnosed,fill=severity))+geom_bar(stat='identity',position='fill')+theme_economist()+labs(y='Portion of Total Cases',x='Branch',title='Severity of Traumatic Brain Injuries (2006-2014)',subtitle = '<span style=\'color: #F8766D;\'>**Mild**</span>, <span style=\'color: #A3A500;\'>**Moderate**</span>, <span style=\'color: #00B0F6;\'>**Penetrating**</span>, <span style=\'color: #E76BF3;\'>**Severe**</span>, and <span style=\'color: #00BF7D;\'>**Not Classifiable**</span>',caption = 'Source: Veterans Brain Injury Center.')+theme(legend.position = 'none',plot.subtitle = element_textbox_simple())
ggsave('Military.png')

show_col(hue_pal()(5))
