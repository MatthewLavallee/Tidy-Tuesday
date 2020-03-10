library('tidyverse')
library('ggthemes')
library('scales')
library('patchwork')
  #load necessary packages
tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 
salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')
diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')
  #load data for tidy tuesday week 11

show_col(hue_pal()(12))
  #show_col to help pick colors for plots

diversity_school$Percent <- (diversity_school$enrollment/diversity_school$total_enrollment)*100
  #create %s for each category

proportion_white<- diversity_school %>% filter(category=="White")
proportion_white %>% filter(name=="Hampshire College")
proportion_white_MA<-proportion_white%>% filter(state=="Massachusetts")
  #creating df of just "white" category, to calculate % of student body that is   POC
proportion_FE<- diversity_school%>%filter(category=="Women")
proportion_FE_MA<-proportion_FE%>%filter(state=="Massachusetts")
proportion_FE %>% filter(name=="Hampshire College")
  #same for "women"
Cost2016<- tuition_income%>% filter(year==2016&income_lvl=="0 to 30,000"&campus=="Off Campus")
Cost2016<- Cost2016%>%select(name,state,total_price)
Hampshire<-c(NA,"Hampshire College","Massachusetts",48500,92300,50,16)
  #got data for Hampshire College from payscale manually
    #https://www.payscale.com/college-salary-report/bachelors?search=Hampshire%20College
salary_potential<-rbind(salary_potential,Hampshire)
df<-merge(Cost2016,salary_potential,by="name")

RDensity<-ggplot(data=proportion_white)+geom_density(aes(x=100-Percent),col="#FF64B0")+geom_density(data=proportion_white_MA,aes(x=100-Percent),col="#619CFF")+labs(x="Percent of Student Body\nThat are POC",y="")+geom_segment(aes(x=100-67.8,y=0,xend=100-67.8,yend=0.0194),col="#00B4F0")+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+theme_economist()


FDensity<-ggplot(data=proportion_FE)+geom_density(aes(x=Percent),col="#FF64B0")+geom_density(data=proportion_FE_MA,aes(x=Percent),col="#619CFF")+labs(x="Percent of Student Body\nThat are Women",y="")+geom_segment(aes(x=59.2,y=0,xend=59.2,yend=0.037),col="#00B4F0")+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+theme_economist()

TuiVSalary<-ggplot(data=df)+geom_point(aes(x=total_price,y=as.numeric(mid_career_pay),col=name=="Hampshire College"),show.legend = FALSE)+labs(y="Salary",x="Total Price")+theme_economist()

df1<- df %>% filter(state=="MA")

MDensity<-ggplot(data=df)+geom_density(aes(x=as.numeric(make_world_better_percent),color="National Distribution"))+geom_density(data=df1,aes(as.numeric(make_world_better_percent),color="Massachusetts Distribution"))+labs(x="Percent of Alums\nThat Feel They Make the World Better",y="")+geom_segment(aes(x=50,y=0,xend=50,yend=0.059,color="Hampshire College"))+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+theme_economist()+scale_color_manual(values=c("National Distribution"="#FF64B0","Massachusetts Distribution"="#619CFF","Hampshire College"="#00B4F0"))+theme(legend.position = 'bottom',legend.text = element_text(size=5))

plots<-(RDensity+FDensity)/MDensity
plots


