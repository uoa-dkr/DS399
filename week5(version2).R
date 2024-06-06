library(tidyverse)

heather <- read_csv('data/heather.csv')

last4_df <- heather %>% 
  mutate(Block = as_factor(Block),
         Year = as_factor(Year)) %>% 
  janitor::clean_names() %>% 
  select(2:3,9:12) %>%
  mutate(last4 = rowSums(.[3:6])) %>% 
  select(1:2,7)
  
last4_df %>% 
  group_by(year,treat) %>% 
  summarise(n = mean(last4),.groups = 'drop') %>% 
  ggplot(aes(year,n,col=treat,group=treat,shape=treat)) + geom_point() + geom_line() +
  labs(y='combination of last 5 groundcovers',title = 'different treatments Vs combination of last 5 groundcovers')


df <- heather %>% 
  mutate(Block = as.factor(Block),
         Treatment = as.factor(Treat),
         Year = as.numeric(Year)) 
df$Treatment <- factor(df$Treatment, levels=c('C', 'B', 'H', 'HB'))
levels(df$Treatment) <- c("Control", "Beetles", "Herbicide", "Beetles+Herbicide")

df %>% 
  mutate(last5 = rowSums(.[9:13])) %>% 
  lm(last5~Year*Treat,data=.) %>% 
  emmeans::emmeans('Treat') %>% 
  pairs()









  
heather_df %>% 
  ggplot(aes(Treat,Heather,fill=Treat)) + geom_boxplot() +
  facet_wrap(Year~.,nrow = 1) + geom_point() + 
  guides(fill=guide_legend(title="New Legend Title"))

   
fit1 <- heather_df %>% 
  lm(Heather~.,data=.)
anova(fit1)

fit2 <- heather_df %>% 
  lm(Heather~Treat * Year,data=.)
anova(fit2)

fit3 <- heather_df %>% 
  lm(Heather~.^2,data=.)
anova(fit3,type='III')
aov(fit3)
tukey_test <- TukeyHSD(aov(fit2))
tukey_test

#rstatix::anova_test(fit3)


calendR::calendR(year=2022,month = 2)





month_calender <- function(num) {
  #if (is.character(num)) {num <-  ifelse(nchar(num) == 3, which[month.abb==num], which[month.name==num])}
  
  times = seq.Date(as.Date('2022/1/1'),as.Date('2022/12/31'),1)
  month_num <- match(months(times),month.name)
  which_month <- times[month_num==num] # input month
  
  wdays <- as.POSIXlt(which_month)$wday
  wdays <- ifelse(wdays==0,7,wdays)
  
  pos_x <- table(cut.Date(which_month,'week'))
  pos_y <- ifelse(length(pos_x)==5,1,0)
  
  if (num %in% c(1,7,10,12,4,5)) { # input month
    polygon_x <-  c(5.5, 7.5,7.5,5.5) 
    polygon_y <-  rep(c(5.25, .75),each=2)
  } else if ( num %in% c(2,3,6,8,9,11)) {# input month
    polygon_x <-  c(5.5, 7.5,7.5,5.5)  
    polygon_y <-  rep(c(5.25, 1.75),each=2)
  } #else if ( test_expression3) {
  #statement3
  #} else {
  #  statement4
  #}
  
  
  wday_title <- c('M','T','W','T','F','S','S')
  df <- data.frame(date = which_month,
                   wday = wdays)
  
  plot(0:7,type = 'n')
  text(4,6.5,month.name[num],cex=2)     # input month
  text(1:7,5.5,wday_title)
  abline(h=5.25)
  polygon(polygon_x,polygon_y,col='gray96')
  text(df$wday,rep(5:pos_y,pos_x))
}

month_calender(5)





cancer <- read.csv('data/cancer.csv')
cancer$class <- ifelse(cancer$diagnose=='B',1,2)
y = cancer$x
n_matrix = outer(y, -4:13, function(x, y) abs(x-y))
which.min(n_matrix[,1])
which(n_matrix[,1] == sort(n_matrix[,1],partial=2)[2])


knn <- function(x,n=1){
  y = cancer$x
  n_matrix = outer(y, x, function(x, y) abs(x-y))
  x <- apply(n_matrix, 2, function(x) (cancer$class[sort(x,index.return=T)$ix[1:n]]))
  if (is.array(x)) apply(x, 2, function(x) ifelse(mean(x)>1.5,2,1)) else (x)
}
knn(-4:13,3)

knn <- function(x,n=1){
  y = cancer$x
  mat = outer(y, x, function(x, y) abs(x-y))
  mat_n <- apply(mat, 2, function(x) (cancer$class[order(x)[1:n]]))
  if (is.array(mat_n)) apply(mat_n, 2, function(x) ifelse(sum(x)>1.5*(n-1)+1,2,1)) else (mat_n)
}
knn(-4:13,1)




set.seed(1)
sa <- sample(c(2,5,7,1,9,2))
#sort(sa,partial=9)[9]
order(sa)[1:3]
which(rank((sa)) == 2)

sa;which(sa==sort((sa),partial=1:4)[1:2])

which(sa %in% sort(sa)[1:4])

apply(matrix(1:12,nr=3), 2, function(x) which(x == sort(x)[1:2]))


sort(sa,index.return=T)$ix[1:3]


tibble(x=1:3,y = letters[1:3], z=c(20,30,40)) %>% 
  pivot_wider(names_from = y,values_from = z,values_fill = 0)

sa;order(sa)[1:4]
sa;tail(order(sa),3)



order(sa)[1:3]


