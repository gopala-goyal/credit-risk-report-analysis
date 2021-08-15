library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readxl)
library(mice)
library(car)
library(caret)
library(magrittr)
library(readxl)

my_file <- read_excel("/Users/Ziqi/Downloads/Original_dataset.xlsx")
my_file2 <- read_excel("/Users/Ziqi/Downloads/Original_dataset_file 2.xlsx")

my_file2_prop.df <- left_join(my_file2 %>% select(-MONTHS_BALANCE)  %>% group_by(ID) %>% summarise(total_count=n()), 
                              my_file2 %>% subset(STATUS!="X" & STATUS!="C") %>% select(-MONTHS_BALANCE) %>% group_by(ID) %>% summarise(number_count=n()),by="ID")%>% replace_na(list(number_count=0)) %>% transmute(ID=ID, prop=number_count/total_count)


my_file2_cleaned.df <- inner_join(my_file2 %>% aggregate(by=list(.$ID), FUN=min) %>%select(-Group.1), my_file2_prop.df, by="ID") %>% select(-STATUS) %>% mutate(MONTHS_BALANCE=abs(MONTHS_BALANCE) +1)

merge_table.df <- inner_join(my_file2_cleaned.df, my_file, by="ID") 




my_file2 %<>% subset(STATUS!="X" & STATUS!="C")

my_file2 %>% aggregate(by=list(.$ID), FUN=min) %>%select(-Group.1) 

my_file2 %>% mutate(STATUS=as.numeric(STATUS)) %>% select(-MONTHS_BALANCE) %>% aggregate(by=list(.$ID), FUN=sum) %>%select(-Group.1) 
#Drop Duplicates
my_arranged_file <- arrange(merge_table.df, desc(merge_table.df$MONTHS_BALANCE))
Droped.df<-my_arranged_file %>% distinct(AMT_INCOME_TOTAL, NAME_INCOME_TYPE,NAME_INCOME_TYPE,DAYS_BIRTH,DAYS_EMPLOYED,.keep_all = TRUE)
Droped.df <- arrange(cleaned.df, cleaned.df$ID)
#Adjusting Birthday to Ages,Days_Employed to Worked_yrs
Droped.df$Ages<- floor(Droped.df$DAYS_BIRTH/-365.25)
Droped.df$Worked_yrs<- ifelse(Droped.df$DAYS_EMPLOYED>0,0,floor(Droped.df$DAYS_EMPLOYED/-365.25))
#Delete meaningless variable
Cleaned.df<-Droped.df[-c(9,13,14,15,16,17,18,19)]
#Change Gender,Own_car,Own_realty to dummies
Cleaned.df$CODE_GENDER<- ifelse(Cleaned.df$CODE_GENDER== "M", 1, 0)
Cleaned.df$FLAG_OWN_CAR<- ifelse(Cleaned.df$FLAG_OWN_CAR== "Y", 1, 0)
Cleaned.df$FLAG_OWN_REALTY<- ifelse(Cleaned.df$FLAG_OWN_REALTY== "Y", 1, 0)
#Education level ---> Dummies
Cleaned.df$Academic_degree<- ifelse(Cleaned.df$NAME_EDUCATION_TYPE== "Academic degree", 1, 0)
Cleaned.df$Higher_education<- ifelse(Cleaned.df$NAME_EDUCATION_TYPE== "Higher education", 1, 0)
Cleaned.df$Incomplete_higher<- ifelse(Cleaned.df$NAME_EDUCATION_TYPE== "Incomplete higher", 1, 0)
Cleaned.df$Lower_secondary<- ifelse(Cleaned.df$NAME_EDUCATION_TYPE== "Lower secondary", 1, 0)
Cleaned.df$Secondary<- ifelse(Cleaned.df$NAME_EDUCATION_TYPE== "Secondary / secondary special", 1, 0)
#Marriage Status----> Dummies
Cleaned.df$Civil_marriage<- ifelse(Cleaned.df$NAME_FAMILY_STATUS== "Civil marriage", 1, 0)
Cleaned.df$Married<- ifelse(Cleaned.df$NAME_FAMILY_STATUS== "Married", 1, 0)
Cleaned.df$Separated<- ifelse(Cleaned.df$NAME_FAMILY_STATUS== "Separated", 1, 0)
Cleaned.df$Single<- ifelse(Cleaned.df$NAME_FAMILY_STATUS== "Single / not married", 1, 0)
Cleaned.df$Widow<- ifelse(Cleaned.df$NAME_FAMILY_STATUS== "Widow", 1, 0)
#House Types--->Dummies
Cleaned.df$Rented_apartment<- ifelse(Cleaned.df$NAME_HOUSING_TYPE== "Rented apartment", 1, 0)
Cleaned.df$House_apartment<- ifelse(Cleaned.df$NAME_HOUSING_TYPE== "House / apartment", 1, 0)
Cleaned.df$With_parents<- ifelse(Cleaned.df$NAME_HOUSING_TYPE== "With parents", 1, 0)
Cleaned.df$Co-op_apartment<- ifelse(Cleaned.df$NAME_HOUSING_TYPE== "Co-op apartment", 1, 0)
Cleaned.df$Municipal_apartment<- ifelse(Cleaned.df$NAME_HOUSING_TYPE== "Municipal apartment", 1, 0)
Cleaned.df$Office_apartment<- ifelse(Cleaned.df$NAME_HOUSING_TYPE== "Office apartment", 1, 0)
#Drop Months_balance<=6
Final_df<-filter(Cleaned.df,Cleaned.df$MONTHS_BALANCE>6)


#Set benchmark at 0.4
Final_df_0.4<-Final_df
Final_df_0.4$Risk<-ifelse(Final_df_0.4$prop>0.4, 1,0)
Final_df_0.4<-Final_df_0.4[-c(1,5,7,3,9,10,11,20,19,13,17,16,15,22,23,24,12,21,26,27,28,29,6,4,25)]
set.seed(1)
sample_0.4 <- sample.int(n = nrow(Final_df_0.4), size=floor(.7*nrow(Final_df_0.4)), replace = F)
train <- Final_df_0.4[sample_0.4, ]
test <- Final_df_0.4[-sample_0.4, ]

reg_0.4 <- glm(Risk ~ ., data = train, family = binomial)
summary(reg_0.4)

