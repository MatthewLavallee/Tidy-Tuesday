library('tidyverse')
library('lubridate')
library('scales')
library('ggtext')
library('ggthemes')
options(scipen=999)


brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

#Does production increase due to taxcuts?
taxtrend<- beer_taxed %>% filter(type %in% c("Production","Sub Total Taxable","Sub Total Tax-Free"))
taxtrend$date <- paste(taxtrend$year, taxtrend$month,1,sep="-") %>% ymd() %>% as.Date()
#filter out variables I care about and create 'date' variable
dat = data.frame(x=taxtrend[388,11], y=taxtrend[388,6])
#create circle for weird observation

ggplot(data= taxtrend)+geom_line(aes(x=date,y=month_current,color=tax_status))+geom_vline(xintercept=as.numeric(taxtrend$date[361]),linetype=4,color='#C77CFF')+scale_y_continuous(trans='log10',breaks=pretty_breaks())+geom_point(aes(x=date, y=month_current), data=dat, size=5, shape=1, color="#A3A500")+theme_economist()+labs(x='Year',y='Barrels Produced\n(Monthly)',title = 'Beer Production (2008-2019)',subtitle='Changes in Production for <span style=\'color: #F8766D;\'>**Tax-Free**</span>, <span style=\'color: #00BA38;\'>**Taxable**</span>, and <span style=\'color: #619CFF;\'>**Total**</span>',caption='The <span style=\'color: #C77CFF;\'>**vertical line**</span> represents the beginning of 2018, when tax cuts began. The <span style=\'color: #A3A500;\'>**circle**</span> highlights a point when one of the sub-totals is greater than the total.')+theme(legend.position = 'none',plot.subtitle = element_textbox_simple(),plot.caption = element_textbox_simple())
#2018-10-01: Taxable subtotal > Total?
ggsave('beerproduct.png')


nsmall<- brewer_size %>% filter(!brewer_size %in% c('6,000,001 Barrels and Over','1,000,001 to 6,000,000 Barrels','Total')) %>% group_by(year)%>%summarise(sum(n_of_brewers))
#number of small brewers (tax-cut eligible) by year

nlarge<- brewer_size %>% filter(brewer_size %in% c('6,000,001 Barrels and Over','1,000,001 to 6,000,000 Barrels')) %>% filter(brewer_size!='Total') %>% group_by(year)%>%summarise(sum(n_of_brewers))
#number of large brewers by year

nsize<- merge(nsmall,nlarge,by='year')
colnames(nsize)<-c('year','n_small','n_large')
#merge them

ggplot(data=nsize,aes(x=year))+geom_line(aes(y=n_small),color='#F8766D')+geom_line(aes(y=n_large),color='#619CFF')+geom_vline(xintercept=2018,linetype=4,color='#C77CFF')+scale_y_continuous(trans='log10',breaks=pretty_breaks())+scale_x_continuous(breaks=pretty_breaks())+labs(y='Number of Brewers',x='',title = 'Number of Brewers (2009-2019)',subtitle = 'Changes in the number of <span style=\'color: #F8766D;\'>**small brewers**</span>, and <span style=\'color: #619CFF;\'>**large brewers**</span>.',caption='The <span style=\'color: #C77CFF;\'>**vertical line**</span> represents 2018, when tax cuts began for small brewers.')+theme_economist()+theme(plot.subtitle = element_textbox_simple(),plot.caption = element_textbox_simple())
ggsave('nbrewers.png')

show_col(hue_pal()(3))

