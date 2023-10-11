## import libraries
devtools::install_github('bbc/bbplot')
pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')
library(tidyverse)
install.packages('naniar')
library(naniar)


#Import dataset

sales <- read_csv("C:/Users/jorge/Desktop/product_sales.csv")
sales_2 <- read_csv("C:/Users/jorge/Desktop/product_sales.csv")
table(sales$sales_method)
sales_2 %>% filter(!is.na(revenue)) %>% mutate(prod_price=revenue/nb_sold)
## Data cleaning: standarize sales_method catergories
sales <- sales %>%
  mutate(sales_method= str_to_lower(sales_method),sales_method= ifelse(
    sales_method == 'em + call',
    'email + call',
    sales_method)) %>%
  replace_na(list(revenue=median(sales$revenue,na.rm=T))) %>%
  mutate(years_as_customer= ifelse(years_as_customer > 39,
                                   median(sales$years_as_customer),
                                   years_as_customer))

#Visualazing missing values
vis_miss(sales)

#Verifying number of decimals in revenue column 
decimals <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}
rev <- sales %>% filter(is.na(revenue))
range(rev$years_as_customer)
decimals(3.520)
sum(dec>2)
nchar
#customer per approach
total_custom <- count(sales,sales_method)
plot1<-ggplot(sales,aes(factor(sales_method,
                        levels = c('email','call','email + call')))) + geom_bar(width = 0.7) +
  geom_text(aes(sales_method,
                n+200,label=n),
            data=total_custom,
            size=4)  +
  xlab('Sales method') +
  ylab('Total customers') + 
  labs(title = 'Total Customers by approach') +
  theme_minimal()+ theme(panel.grid.major.x=element_blank(),
                          panel.grid.major.y=element_line(size=0.6,
                                                          color='black',
                                                          linetype = 'dashed'),
                          panel.grid.minor.y = element_blank(),
                          axis.text = element_text(size=12,
                                                   color='black'),
                          axis.title.y = element_text(size=16,
                                                    face='bold',
                                                    vjust=2.6,
                                                    hjust=0.5),
                         axis.title.x = element_text(size=16,
                                                     face='bold',
                                                     vjust=-0.8,
                                                     hjust=0.5),
                          plot.title = element_text(size=19,
                                                    face='bold',
                                                    vjust=2.4,
                                                    hjust=0.5))

ggsave('prop2.png',plot_8,
       width=6,height=4,path="C:/Users/jorge/Desktop/R datasets/certification")

abb<- read_csv("C:/Users/jorge/Downloads/table-data.csv")
data_plot1<- count(sales,state,sales_method) %>% left_join(abb,by='state')
plot2<-ggplot(data_plot1,aes(code,n)) +
  geom_segment(aes(x=code,y=0,xend=code,yend=n),data=data_plot1,
               color='#1F3561',
               size=0.8)+
  geom_point(size=2,color='#B6CDFA') + 
  facet_grid(factor(sales_method,
                    levels = c('email','call','email + call')) ~.) + theme_minimal() +
  xlab('State') +
  ylab('Total customers') + 
  labs(title = 'Total Customers by state and approach')+
  theme(axis.text.x = element_text(angle=90,
                                   hjust=1,
                                   vjust=0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = 'darkgrey',
                                          linetype = 'dashed'),
        axis.text.x.bottom = element_text(size=8),
        axis.text = element_text(size=9,
                                 color='black'),
        axis.title.y = element_text(size=15,
                                    face='bold',
                                    vjust=2.6,
                                    hjust=0.5),
        axis.title.x = element_text(size=15,
                                    face='bold',
                                    vjust=-0.8,
                                    hjust=0.5),
        plot.title = element_text(size=20,
                                  face='bold',
                                  vjust=2.3,
                                  hjust=0.5),
        axis.ticks.x.bottom = element_line(),
        strip.text = element_text(size=11,
                                  face = 'bold'))


plot3<-ggplot(sales,aes(x=revenue)) + stat_halfeye(alpha=0.5,
                                              justification=-0.015
                                              )+
  geom_boxplot(width=0.1 ,alpha=0.1, outlier.color='red')+
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult=c(0,0.01)))+
  xlab('Revenue') +
  ylab('Density') + 
  labs(title = 'Distribution of revenue overall')+
  theme(axis.text.x = element_text(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = 'darkgrey',
                                          linetype = 'dashed'),
        axis.text.x.bottom = element_text(size=11),
        axis.text = element_text(size=9,
                                 color='black'),
        axis.title.y = element_text(size=15,
                                    face='bold',
                                    vjust=2.6,
                                    hjust=0.5),
        axis.title.x = element_text(size=15,
                                    face='bold',
                                    vjust=-0.8,
                                    hjust=0.5),
        plot.title = element_text(size=20,
                                  face='bold',
                                  vjust=2.3,
                                  hjust=0.5),
        axis.ticks.x.bottom = element_line())

plot4<- ggplot(sales,aes(sales_method,revenue)) + 
  stat_halfeye(alpha=0.5,
               justification=-0.015,
               linewidth=0,
               adjust = 2
  )+
  geom_boxplot(width=0.2,
               alpha=0.1,
               outlier.size = 2.5,
               outlier.color= 'red',
               linewidth=0.65) +
  theme_minimal() +
  xlab('Method of sale') +
  ylab('Revenue') + 
  labs(title = 'Distribution of revenue for each method')+
  theme(axis.text.x = element_text(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = 'darkgrey',
                                          linetype = 'dashed'),
        axis.text.x.bottom = element_text(size=11),
        axis.text = element_text(size=9,
                                 color='black'),
        axis.title.y = element_text(size=15,
                                    face='bold',
                                    vjust=2.6,
                                    hjust=0.5),
        axis.title.x = element_text(size=15,
                                    face='bold',
                                    vjust=-0.8,
                                    hjust=0.5),
        plot.title = element_text(size=20,
                                  face='bold',
                                  vjust=2.3,
                                  hjust=0.5))

sales %>% group_by(sales_method) %>% 
  summarize(min=range(revenue)[1],
            max=range(revenue)[2],
            total_rev=sum(revenue),
            rev_per_sale=median(revenue))

sales %>% group_by(week,sales_method) %>% 
  ggplot(aes(week,revenue,group=sales_method)) +
  stat_summary()

plot_5<-sales %>% ggplot(aes(week,revenue,group=sales_method,col=sales_method))  + 
  stat_summary(fun=mean,
               geom='line',
               size=1.1)+
  stat_summary(fun=mean,
               geom='point',
               size=2) +
  stat_summary(fun.data = mean_sdl,
               fun.args=list(mult=1),
               geom='errorbar',
               width=0.1) +
  scale_color_manual(values=c('#476596','#E3CBC8','#B2C5E3'))+
  theme_classic() +
  xlab('Week') +
  ylab('Revenue') + 
  labs(title = 'Average revenue per sale over \ntime by method'
       )+
  scale_x_discrete(limits=c(1,2,3,4,5,6),
                   expand = expansion(mult=c(0.05,0.05)))+
    theme(axis.text.x = element_text(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x.bottom = element_text(size=11),
        axis.text = element_text(size=10,
                                 color='black'),
        axis.title.y = element_text(size=15,
                                    face='bold',
                                    vjust=2.6,
                                    hjust=0.5),
        axis.title.x = element_text(size=15,
                                    face='bold',
                                    vjust=-0.8,
                                    hjust=0.5),
        plot.title = element_text(size=20,
                                  face='bold',
                                  vjust=2.3,
                                  hjust=0.5),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        axis.line = element_line(size=1,
                                 color='darkgrey'),
        axis.ticks = element_line(size=1.2,
                                  color='darkgrey'),
        title = element_text(vjust=0))

plot_6 <- sales %>% group_by(sales_method) %>% summarise(Total = sum(revenue)) %>%
  mutate(Total_rev=paste0('US $',Total)) %>%
  ggplot(aes(factor(sales_method, levels=c('email','email + call','call')),Total)) + 
  geom_col(width=0.7) + xlab('Sales method') +
  geom_text(aes(sales_method,Total+18000,label=Total_rev),
            size=4)+
  ylab('Total revenue') + 
  labs(title = 'Total revenue by approach') +
  theme_minimal()+ theme(panel.grid.major.x=element_blank(),
                         panel.grid.major.y=element_line(size=0.6,
                                                         color='black',
                                                         linetype = 'dashed'),
                         panel.grid.minor.y = element_blank(),
                         axis.text = element_text(size=12,
                                                  color='black'),
                         axis.title.y = element_text(size=16,
                                                     face='bold',
                                                     vjust=2.6,
                                                     hjust=0.5),
                         axis.title.x = element_text(size=16,
                                                     face='bold',
                                                     vjust=-0.8,
                                                     hjust=0.5),
                         plot.title = element_text(size=19,
                                                   face='bold',
                                                   vjust=2.4,
                                                   hjust=0.5))

         
ggsave('proportion.png',plot_8,
       width=6,height=4,path="C:/Users/jorge/Desktop/R datasets/certification")

plot_7 <-ggplot(sales,aes(week,revenue,group=sales_method,col=factor(sales_method,
                                                                     levels=c('email + call','call','email')))) +
  stat_summary(fun=sum,
               geom='line',
               size=1.2) +  
  scale_x_discrete(limits=c(1,2,3,4,5,6))+
  ylab('Revenue') + 
  labs(title = 'Weekly revenue for each method',
       color='Sales Method') +
  theme_minimal()+ theme(panel.grid.major.y=element_line(size=0.6,
                                                         color='gray',
                                                         linetype = 'dashed'),
                         panel.grid.minor.y = element_blank(),
                         axis.text = element_text(size=12,
                                                  color='black'),
                         axis.title.y = element_text(size=16,
                                                     face='bold',
                                                     vjust=2.6,
                                                     hjust=0.5),
                         axis.title.x = element_text(size=16,
                                                     face='bold',
                                                     vjust=-0.8,
                                                     hjust=0.5),
                         plot.title = element_text(size=19,
                                                   face='bold',
                                                   vjust=2.4,
                                                   hjust=0.5),
                         legend.position = 'bottom')

  

sales %>% group_by(week) %>%
  summarise(median= median(revenue),
            mean= mean(revenue),
            Total= sum(revenue)) %>% 
  mutate(Percentage=Total*100/sum(Total))

plot_8 <- sales %>% group_by(sales_method,week) %>% summarise(Total_rev=sum(revenue)) %>% 
  ggplot(aes(week,Total_rev,fill=factor(sales_method,
                                        levels=c('email + call','call','email')))) + 
           geom_col(position = 'fill') + 
  labs(title = '% of total revenue by method over time',
       y='Percentage') +
  scale_x_discrete(limits=c(1,2,3,4,5,6)) +
  theme_minimal()+ theme(panel.grid.major.y=element_line(size=0.6,
                                                         color='gray',
                                                         linetype = 'dashed'),
                         panel.grid.minor.y = element_blank(),
                         axis.text = element_text(size=12,
                                                  color='black'),
                         axis.title.y = element_text(size=16,
                                                     face='bold',
                                                     vjust=2.6,
                                                     hjust=0.5),
                         axis.title.x = element_text(size=16,
                                                     face='bold',
                                                     vjust=-0.8,
                                                     hjust=0.5),
                         plot.title = element_text(size=19,
                                                   face='bold',
                                                   vjust=2.4,
                                                   hjust=0.5),
                         legend.position = 'bottom',
                         legend.title = element_blank(),
                         legend.text = element_text(size=12))

ggplot(sales,aes(nb_site_visits,revenue)) + geom_point(alpha=0.2)

ggplot(sales,aes(revenue)) + geom_histogram()

print(sales %>% group_by(state) %>% summarise(med=median(revenue)),n=50)


