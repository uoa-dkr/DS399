calculate.grades <- function(group, individual, t1 = seq(.6,.8,by=.01), t2 = seq(4.1,4.3,by=.01)) {
  bonus = c(.25, .05)                       # only for individual score >= 4
  n = 1                                     # we trust everyone at beginning
  s1 = sample(t1, 1)                        # random threshold value for group
  s2 = sample(t2, 1)                        # random threshold value for individual
  final_result = numeric(length(individual))# [0,0,0, ...]
  result_cal_above3 = setNames(seq(0,1,by=1/40)[1:21], seq(3,5,length=21))
  result_cal_2to3 = setNames(seq(.7,.9,length=10), seq(2,2.9,length=10))
  result_cal_1to2 = setNames(seq(.4,.6,length=10), seq(1,1.9,length=10))
  result_cal_below3 = c(result_cal_1to2, result_cal_2to3)
  result_cal_0 = setNames(c(rep(0,10),rep(5,10),rep(seq(10,20,length=2),each=10),30), seq(1,5,by=.1))
  
  if (group == 0){  # if group = 0, then 1-1.9 --> 0, 2-2.9 --> 5, 3-3.9 --> 10, 4-4.9 --> 20, 5 --> 30
    for (i in 1:length(individual)) 
      final_result[i] = result_cal_0[names(result_cal_0) == individual[i]]
    return (final_result)
  }
  
  # image if the mean of individual score is high, but their group score is relatively low, 
  # if it is a case, it means that we need to think the reliability of the individual score :(
  # in general, team with higher mean of individual score tend to have higher group score
  
  if (mean(individual) > s2) { # if mean of individual is too high, it becomes suspectable :(
    n = n + 1
    if (group > 100*s1){       # all clear hopefully  :>
      n = n - 1
      for (i in 1:length(individual))
        if (individual[i] >= 3){
          value1 = group + (100-group)*(result_cal_above3[names(result_cal_above3) == individual[i]])
          final_result[i] = value1
          if (individual[i] >= 4){
            value2 = value1 + (100-value1)*bonus[n]
            final_result[i] = value2}
          }
        else{
          value1 = group*(result_cal_below3[names(result_cal_below3) == individual[i]])
          final_result[i] = value1}
      }
      
    else{                       # doubtable  :l 
      for (i in 1:length(individual))
        if (individual[i] >= 3){
          value1 = group + (100-group)*(result_cal_above3[names(result_cal_above3) == individual[i]])
          final_result[i] = value1
          if (individual[i] >= 4){
            value2 = value1 + (100-value1)*bonus[n]
            final_result[i] = value2}
          }
        else{
          value1 = group*(result_cal_below3[names(result_cal_below3) == individual[i]])
          final_result[i] = value1}}
  } else {                      # the mean of individual score is low, it seems nothing need to be worried
    for (i in 1:length(individual))
      if (individual[i] >= 3){
        value1 = group + (100-group)*(result_cal_above3[names(result_cal_above3) == individual[i]])
        final_result[i] = value1
        if (individual[i] >= 4){
          value2 = value1 + (100-value1)*bonus[n]
          final_result[i] = value2}
        }
      else{
        value1 = group*(result_cal_below3[names(result_cal_below3) == individual[i]])
        final_result[i] = value1}
  }
  sapply(final_result, ceiling)
}

# case 0:(group = 0)
calculate.grades(group = 0, individual = c(5, 4, 1, 2, 3))
calculate.grades(group = 0, individual = c(5, 4.5, 1.9, 2.2, 3.6))
calculate.grades(group = 0, individual = c(1, 1, 1, 1, 1.9))
calculate.grades(group = 0, individual = c(5, 5, 4.9, 5, 1.9))


# case 1:(mean < 4.1-4.3)
calculate.grades(group = 75, individual = c(4.5, 3.2, 3.1, 1.0))
calculate.grades(group = 50, individual = c(5, 5, 1, 1.0))
calculate.grades(group = 100, individual = c(5, 3, 1, 2.9, 4.2))
calculate.grades(group = 1, individual = c(1, 1, 1, 1))


# case 2:(mean > 4.1-4.3 and group > 60-80)
calculate.grades(group = 75, individual = c(4.5, 4.2, 5, 4.0))
calculate.grades(group = 80, individual = c(4.5, 4.2, 5, 3.9))
calculate.grades(group = 80, individual = c(5, 5, 5, 5))



# case3:(mean > 4.1-4.3 and group < 60)
calculate.grades(group = 20, individual = c(4.5, 4.2, 5, 3.8))
calculate.grades(group = 59, individual = c(4.5, 4.2, 4.9, 3.8))
calculate.grades(group = 5, individual = c(4.5, 4.3, 5, 3.9))
calculate.grades(group = 8, individual = c(5, 5, 5, 5))


library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
df <- read_csv("group-d-week2/Book1.csv");df
df_new <- mutate(df, individual_score = mapply(calculate.grades, group = group_mark, individual = peer_rating))
df_new %>% ggplot(aes(x=group_mark,y=individual_score,color = peer_rating)) + geom_jitter(width = .2)
df_new %>% ggplot(aes(x=group_mark,y=individual_score,color = (peer_rating))) + geom_point() 












