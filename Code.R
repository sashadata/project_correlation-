### Title: Exploratory Data Analysis
### Get to know data in the dataset


# INSTALL AND LOAD PACKAGES ################################

library(magrittr)
library(tidyverse)
library(rio)

# magrittr: for pipes
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

df <- import("dataeda.csv") 

#### using describe function obtain quick overview on the dataset: frequency, proportion and missing 

library(Hmisc)
describe(df)

#### variables Parent, Race, and English have missing values
### get % of missing values for English variable
### 29% missing data for English variable. If this variable to be included 
### for further analysis there should be strategy applied to deal with this issue. 

table(df$English)*100/1187

### Crosstable ####

library(crosstable)
library(dplyr)

###  what is percentage of participants did not complete the program?

crosstable(df, c(Gender), by=Dropout, total="column") %>% 
  as_flextable(keep_id=FALSE)

### result: 234 or 19.7% dropout
### 193 were females (20% of total number of all females), 
### 41 were males (18% of all males)


### create barplot to demonstrate the number of dropout out by gender
library(ggthemes)
df %>% 
  group_by(Gender, Dropout) %>%
  summarise(total= n()) %>%
  ggplot(aes(Dropout, total, fill=Dropout))+
  geom_bar(stat = 'identity')+facet_wrap(~ Gender)+
  theme_minimal()+
  scale_fill_tableau()+
  ylab("")+xlab("")+
  ggtitle("Completed vs Dropout")
  

### identify age group and gender that have the high number of drops
### create new age categories for age variable and identity the highest numbers including gender
### use crosstable to look at the number and %

df %>% 
  mutate(Age_group=case_when(Age >=18 & Age <= 30 ~"18_to_30",
                         Age >=31 & Age <= 40 ~ "31_to_40",
                         Age >=41 & Age <= 50 ~ "41_to_50",
                         Age >=51 & Age <= 60 ~ "51_to_60",
                         Age >=61 & Age <= 70 ~ "61_to_70",
                         Age >=71 & Age <= 80 ~ "71_to_80")) %>%
  select(Age_group,Gender, Dropout) %>%
  group_by(Gender, Age_group, Dropout) %>%
  crosstable(c(Age_group), by=c(Dropout,Gender), total="column") %>% 
  as_flextable(keep_id=FALSE)

### Result: Three age categories of females have high number of not completing the program
### females - 31_to_40 - 86 (16.04%), 41_to_50 -74(16.63%), 18_to_30- 27(23.28%)
### males - no obvious pattern observed 


df<-df%>%
  as_tibble() %>%
  select(Age, Income, NofCases, Children, Services, Days) %>% 
  print()

# CORRELATION MATRIX 

# Correlation matrix for data frame
df %>% cor()

# Fewer decimal places
df<-df %>%
  cor() %>%     
  round(2) %>%  
  print()

# Visualize correlation matrix with ggcorrplot
library(ggcorrplot)
ggcorrplot(df, lab=TRUE)

# Use cor.test() to test one pair of variables at a time.

df %>% cor.test(Income, Age)


########################################################
########################################################
########################################################


