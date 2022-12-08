# ----------------------- Sta 404 - Fall 2022 ---------------------#
#                 Group Project - Data Preparation                 #
#                        Thy Nguyen - Anh Vo                       #
#------------------------------------------------------------------#

### Packages
library(tidyverse)
library("readxl")
library(dplyr)

setwd("/Users/anh.vo/Downloads/Final Draft/Data")

### Data load and clean

## https://nces.ed.gov/programs/digest/d21/tables/dt21_502.30.asp?current=yes

## Data by Race/Ethnicity

# White

t_white <- read_excel("Raw data/tabn502.30.xls", 
                       range = "B58:AH64",
                       col_names = c("A", "blank1", "B", "C","blank2", "D", "E", "blank3", "F", "G", "blank4", "H",
                                     "I", "blank5", "J", "K", "blank6", "L", "M", "blank7", "N", "O", "blank8", "P",
                                     "Q", "blank9", "R", "S", "blank10", "T", "U", "blank11", "V"))

t_white <- subset(t_white, select = c("A", "C", "E", "G", "I", "K", "M", "O", "Q", "S", "U"))

t_white <-  rbind(c(1995, 2000, 2005, 2010, 2014, 2015, 2016, 2017, 2018, 2019, 2020), t_white)

t_white <- as.data.frame(t_white)

rownames(t_white) = c("Year", "Less than High School", "High School", "College - No degree", "Associate",
                      "Bachelor or Higher", "Bachelor", "Master")

white <- as.data.frame(t(t_white)) %>% 
  mutate(Race = "White")

# Black 

t_black <- read_excel("Raw data/tabn502.30.xls", 
                       range = "B75:AH81",
                       col_names = c("A", "blank1", "B", "C","blank2", "D", "E", "blank3", "F", "G", "blank4", "H",
                                     "I", "blank5", "J", "K", "blank6", "L", "M", "blank7", "N", "O", "blank8", "P",
                                     "Q", "blank9", "R", "S", "blank10", "T", "U", "blank11", "V"))

t_black <- subset(t_black, select = c("A", "C", "E", "G", "I", "K", "M", "O", "Q", "S", "U"))

t_black <-  rbind(c(1995, 2000, 2005, 2010, 2014, 2015, 2016, 2017, 2018, 2019, 2020), t_black)

t_black <- as.data.frame(t_black)

rownames(t_black) = c("Year", "Less than High School", "High School", "College - No degree", "Associate",
                      "Bachelor or Higher", "Bachelor", "Master")

black <- as.data.frame(t(t_black)) %>% 
  mutate(Race = "Black")


# Hispanic 

t_hispanic <- read_excel("Raw data/tabn502.30.xls", 
                          range = "B92:AH98",
                          col_names = c("A", "blank1", "B", "C","blank2", "D", "E", "blank3", "F", "G", "blank4", "H",
                                        "I", "blank5", "J", "K", "blank6", "L", "M", "blank7", "N", "O", "blank8", "P",
                                        "Q", "blank9", "R", "S", "blank10", "T", "U", "blank11", "V"))

t_hispanic <- subset(t_hispanic, select = c("A", "C", "E", "G", "I", "K", "M", "O", "Q", "S", "U"))

t_hispanic <-  rbind(c(1995, 2000, 2005, 2010, 2014, 2015, 2016, 2017, 2018, 2019, 2020), t_hispanic)

t_hispanic <- as.data.frame(t_hispanic)

rownames(t_hispanic) = c("Year", "Less than High School", "High School", "College - No degree", "Associate",
                         "Bachelor or Higher", "Bachelor", "Master")

hispanic <- as.data.frame(t(t_hispanic)) %>% 
  mutate(Race = "Hispanic")


# Combine data

race_income <- rbind(white, black, hispanic)

race_income <- dplyr::select(race_income, -"Bachelor or Higher")

save(race_income, file = "Clean data/RData file/race_income.RData")


## Data by Gender

# Male

t_male <- read_excel("Raw data/tabn502.30.xls", 
                      range = "B24:AH30",
                      col_names = c("A", "blank1", "B", "C","blank2", "D", "E", "blank3", "F", "G", "blank4", "H",
                                    "I", "blank5", "J", "K", "blank6", "L", "M", "blank7", "N", "O", "blank8", "P",
                                    "Q", "blank9", "R", "S", "blank10", "T", "U", "blank11", "V"))

t_male <- subset(t_male, select = c("A", "C", "E", "G", "I", "K", "M", "O", "Q", "S", "U"))

t_male <-  rbind(c(1995, 2000, 2005, 2010, 2014, 2015, 2016, 2017, 2018, 2019, 2020), t_male)

t_male <- as.data.frame(t_male)

rownames(t_male) = c("Year", "Less than High School", "High School", "College - No degree", "Associate",
                      "Bachelor or Higher", "Bachelor", "Master")

male <- as.data.frame(t(t_male))

male <- male %>% 
  pivot_longer(!Year, names_to = "Education", values_to = "Income") %>% 
  mutate(Gender = "Male")


# Female

t_female <- read_excel("Raw data/tabn502.30.xls", 
                     range = "B41:AH47",
                     col_names = c("A", "blank1", "B", "C","blank2", "D", "E", "blank3", "F", "G", "blank4", "H",
                                   "I", "blank5", "J", "K", "blank6", "L", "M", "blank7", "N", "O", "blank8", "P",
                                   "Q", "blank9", "R", "S", "blank10", "T", "U", "blank11", "V"))

t_female <- subset(t_female, select = c("A", "C", "E", "G", "I", "K", "M", "O", "Q", "S", "U"))

t_female <-  rbind(c(1995, 2000, 2005, 2010, 2014, 2015, 2016, 2017, 2018, 2019, 2020), t_female)

t_female <- as.data.frame(t_female)

rownames(t_female) = c("Year", "Less than High School", "High School", "College - No degree", "Associate",
                     "Bachelor or Higher", "Bachelor", "Master")

female <- as.data.frame(t(t_female))

female <- female %>% 
  pivot_longer(!Year, names_to = "Education", values_to = "Income") %>% 
  mutate(Gender = "Female")

# Combine data

gender_income <- rbind(male, female)

gender_income <- gender_income %>% 
  pivot_wider(names_from = Gender, values_from = Income) %>% 
  subset(Education != "Bachelor or Higher")

save(gender_income, file = "Clean data/Rdata file/gender_income.RData")










####################################################################################



library(writexl)
library(readxl)
library(dplyr)
library(tidyr)


#### CONSTANT DOLLARS 


### https://www.census.gov/topics/income-poverty/income/guidance/current-vs-constant-dollars.html#:~:text=Constant%2Ddollar%20value%20(also%20called,series%20reported%20in%20dollar%20terms.

cpi <- read_excel("Raw data/R_CPI_U_RS.xlsx", skip = 2)
cpi <- cpi %>%
  pivot_wider(names_from = Year,
              values_from = `R-CPI-U-RS1 Index               (December 1977 = 100)`)



to_current <- function(data, money_col, constYear) {
  l <- c()
  for (i in 1:nrow(data)) {
    curryear <- data[[i,'Year']]
    l[i] <- data[[i,money_col]]*(cpi[[curryear]] / cpi[[constYear]])
  }
  
  return (l)
}



to_constant2021 <- function(data, money_col) {
  l <- c()
  for (i in 1:nrow(data)) {
    curryear <- data[[i,'Year']]
    l[i] <- data[[i,money_col]]*(cpi[["2021"]] / cpi[[curryear]])
  }
  return (l)
}



### Average annual salary



teach_salary <- read_excel("Raw data/avg_teachers_annual_salary.xls", skip = 4)

teach_salary_pre <- teach_salary[c(1:51), c(1,5:8)]
names(teach_salary_pre) <- c("State","1999-2000", "2009-2010", "2019-2020", "2020-2021")
teach_salary_pre$MoneyStatus <- rep("Current", len = nrow(teach_salary_pre))

teach_salary_post <- teach_salary[c(1:51), c(1,12:15)]
names(teach_salary_post) <- c("State","1999-2000", "2009-2010", "2019-2020", "2020-2021")
teach_salary_post$MoneyStatus <- rep("Constant", len = nrow(teach_salary_post))

teach_salary <- rbind(teach_salary_pre, teach_salary_post)

teach_salary_long <- teach_salary %>% 
  pivot_longer(
    cols         = c(2:5),
    names_to     = "Year",
    values_to    = "Avg_annual_salary"
  ) 

teach_salary_long <- teach_salary_long %>%
  filter(!substr(State, 1,4) == "Dist")




### Median Income



median_income <- read_excel("Raw data/median_income.xls", skip = 4)

median_income <- median_income[c(1:51), c(1:3,6,8,10,12,14,16,18)]
names(median_income) <- c("State", "2000", "2005", "2010", "2014", "2015",
                          "2016", "2017", "2018", "2019")
median_income <- median_income %>%
  pivot_longer(
    cols         = c(2:10),
    names_to     = "Year",
    values_to    = "Med_income"
  ) 

median_income_pre <- median_income
median_income_pre$MoneyStatus <- rep("Current", len = nrow(median_income_pre))
median_income_pre$Med_income <- to_current(median_income_pre, "Med_income", "2019")

median_income_post <- median_income
median_income_post$MoneyStatus <- rep("Constant", len = nrow(median_income_post))
median_income_post$Med_income <- to_constant2021(median_income_pre, "Med_income")

median_income <- rbind(median_income_pre, median_income_post)


median_income <- median_income %>%
  filter(!substr(State, 1,4) == "Dist")



### Expenditures - Private post-sec


exp_pri_postsec <- read_excel("Raw data/expenditures_private_postsec.xls", skip = 5)
exp_pri_postsec_non <- exp_pri_postsec[c(1:51), c(1:7)]
names(exp_pri_postsec_non) <- c("State","1999-2000", "2009-2010", "2014-2015", "2017-2018", "2018-2019", "2019-2020")
exp_pri_postsec_non$Organization_type <- rep("Non-profit", len = nrow(exp_pri_postsec_non))

exp_pri_postsec_for <- exp_pri_postsec[c(1:51), c(1, 8:13)]
names(exp_pri_postsec_for) <- c("State","1999-2000", "2009-2010", "2014-2015", "2017-2018", "2018-2019", "2019-2020")
exp_pri_postsec_for$Organization_type <- rep("For-profit", len = nrow(exp_pri_postsec_for))

exp_pri_postsec <- rbind(exp_pri_postsec_non, exp_pri_postsec_for)

exp_pri_postsec <- exp_pri_postsec %>%
  pivot_longer(
    cols         = c(2:7),
    names_to     = "Year_range",
    values_to    = "Total_expenditures_thousands"
  ) %>%
  mutate(Total_expenditures_thousands = as.numeric(Total_expenditures_thousands),
         Year = substr(Year_range, 6, 9))

exp_pri_postsec2 <- exp_pri_postsec
exp_pri_postsec2$MoneyStatus <- rep("Constant", len = nrow(exp_pri_postsec2))
exp_pri_postsec2$Total_expenditures_thousands <- to_constant2021(exp_pri_postsec2, "Total_expenditures_thousands")
exp_pri_postsec$MoneyStatus <- rep("Current", len = nrow(exp_pri_postsec))
exp_pri_postsec <- rbind(exp_pri_postsec, exp_pri_postsec2)


exp_pri_postsec <- exp_pri_postsec %>%
  filter(!substr(State, 1,4) == "Dist")




### Expenditures - Public sec 


exp_pub_sec <- read_excel("Raw data/expenditures_public_sec.xls", skip = 4)
exp_pub_sec <- exp_pub_sec[, c(1, 5:17)]
names(exp_pub_sec) <- c("State","1999-2000", "2004-2005", "2008-2009", "2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017","2017-2018", "2018-2019")

exp_pub_sec_pre <- exp_pub_sec[c(1:51), ]
exp_pub_sec_pre <- exp_pub_sec_pre %>%
  mutate_at(c('1999-2000', '2004-2005'), as.numeric) %>%
  pivot_longer(
    cols         = c(2:14),
    names_to     = "Year_range",
    values_to    = "Total_expenditures_thousands"
  ) %>%
  mutate(Total_expenditures_thousands = as.numeric(Total_expenditures_thousands),
         Year = substr(Year_range, 6, 9))
exp_pub_sec_pre$MoneyStatus <- rep("Current", len = nrow(exp_pub_sec_pre))


exp_pub_sec_post <- exp_pub_sec[c(60:110), ]
exp_pub_sec_post <- exp_pub_sec_post %>%
  mutate_at(c('1999-2000', '2004-2005'), as.numeric) %>%
  pivot_longer(
    cols         = c(2:14),
    names_to     = "Year_range",
    values_to    = "Total_expenditures_thousands"
  ) %>%
  mutate(Total_expenditures_thousands = as.numeric(Total_expenditures_thousands),
         Year = substr(Year_range, 6, 9))
exp_pub_sec_post$MoneyStatus <- rep("Constant", len = nrow(exp_pub_sec_post))


exp_pub_sec <- rbind(exp_pub_sec_pre, exp_pub_sec_post)

exp_pub_sec <- exp_pub_sec %>%
  filter(!substr(State, 1,4) == "Dist")







### Public high school graduates


public_high_grad <- read_excel("Raw data/public_high_grad.xls", skip = 10)
public_high_grad <- public_high_grad[c(1:51), c(1, 5:16)]
names(public_high_grad) <- c("State","1999-2000", "2009-2010", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017","2017-2018", "2018-2019", "2019-2020", "2020-2021")

public_high_grad <- public_high_grad %>%
  filter(!substr(State, 1,4) == "Dist")  %>%
  mutate_at(c("1999-2000", "2009-2010", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017","2017-2018", "2018-2019", "2019-2020", "2020-2021"), as.numeric) %>%
  pivot_longer(
    cols         = c(2:13),
    names_to     = "Year_range",
    values_to    = "Total_graduates"
  )  %>%
  filter(!substr(State, 1,4) == "Dist")





### Public sec Student/Teacher Ratio


stu_staff_ratio <- read_excel("Raw data/stu_staff_ratio.xls", skip = 4)
stu_staff_ratio <- stu_staff_ratio[c(1:51), c(1:6, 9,12,16)]
names(stu_staff_ratio) <- c("State","2000-2001", "2005-2006", "2010-2011", "2015-2016", "2016-2017","2017-2018", "2018-2019", "2019-2020")

stu_staff_ratio <- stu_staff_ratio %>%
  filter(!substr(State, 1,4) == "Dist")  %>%
  mutate_at(c("2000-2001", "2005-2006", "2010-2011", "2015-2016", "2016-2017","2017-2018", "2018-2019", "2019-2020"), as.numeric) %>%
  pivot_longer(
    cols         = c(2:9),
    names_to     = "Year_range",
    values_to    = "Student/Teacher_Ratio"
  ) 



### Populations!!


pop <- read_excel("Raw data/total_and_schoolage_pop.xls", skip = 5)
total_pop <- pop[c(1:51), c(1, 5:12)]

names(total_pop) <- c("State","2000", "2010", "2015", "2016", "2017","2018", "2019", "2020")

total_pop <- total_pop %>%
  filter(!substr(State, 1,4) == "Dist")  %>%
  pivot_longer(
    cols         = c(2:9),
    names_to     = "Year",
    values_to    = "Total_population"
  ) 

school_pop <- pop[c(1:51), c(1, 16:23)]

names(school_pop) <- c("State","2000", "2010", "2015", "2016", "2017","2018", "2019", "2020")

school_pop <- school_pop %>%
  filter(!substr(State, 1,4) == "Dist")  %>%
  pivot_longer(
    cols         = c(2:9),
    names_to     = "Year",
    values_to    = "School_population"
  ) 

pop <- merge(total_pop, school_pop, by = c("State", "Year"))




### Percent in Poverty


poverty <- read_excel("Raw data/povertyrate.xls", skip = 13)
poverty <- poverty[c(1:51), c(1, 3,4,6,8)]
names(poverty) <- c("State","2000", "2010", "2015",  "2019")
poverty <- poverty %>%
  filter(!substr(State, 1,4) == "Dist")  %>%
  pivot_longer(
    cols         = c(2:5),
    names_to     = "Year",
    values_to    = "Poverty_rate"
  ) 






### Final versions


final_median_income <- median_income %>%
  pivot_wider(names_from = MoneyStatus,
              values_from = Med_income) 

final_teacher_salary <- teach_salary_long %>%
  pivot_wider(names_from = MoneyStatus,
              values_from = Avg_annual_salary) 

final_exp_pri_postsec <- exp_pri_postsec %>%
  pivot_wider(names_from = MoneyStatus,
              values_from = Total_expenditures_thousands) %>%
  dplyr::select(-Year) 
names(final_exp_pri_postsec)[names(final_exp_pri_postsec) == 'Year_range'] <- "Year"

final_exp_pub_sec <- exp_pub_sec %>%
  pivot_wider(names_from = MoneyStatus,
              values_from = Total_expenditures_thousands) %>%
  dplyr::select(-Year) 
names(final_exp_pub_sec)[names(final_exp_pub_sec) == 'Year_range'] <- "Year"



public_high_grad <- public_high_grad %>%
  mutate(Year = substr(Year_range,6,9)) %>%
  dplyr::select(-Year_range) 

pop <- inner_join(public_high_grad, pop, by = c("State", "Year"))
pop <- pop[, c(1,3,2,4,5)]



pop <- pop %>%
  mutate(hs_pop_ratio = Total_graduates/(Total_population*1000),
         school_pop_ratio = School_population/Total_population,
         hs_school_ratio = Total_graduates/(School_population*1000))




income_poverty <- left_join(final_median_income, poverty, by = c("State", "Year"))



final_exp_pri_postsec$Institution_Type <- rep("Private Post-secondary", len = nrow(final_exp_pri_postsec)) 
final_exp_pub_sec$Organization_type <- rep("Non-profit", len = nrow(final_exp_pub_sec)) 
final_exp_pub_sec$Institution_Type <- rep("Public Secondary", len = nrow(final_exp_pub_sec)) 
final_exp_pri_postsec <- final_exp_pri_postsec[, c(1, 3:5, 2, 6)]

final_exp_pri_postsec <- final_exp_pri_postsec%>% 
  filter(Year %in% unique(final_exp_pub_sec$Year))

final_exp_pub_sec <- final_exp_pub_sec%>% 
  filter(Year %in% unique(final_exp_pri_postsec$Year))

expenditures <- rbind(final_exp_pri_postsec, final_exp_pub_sec)
expenditures <- expenditures[, c(1, 5, 2:4, 6)]


save(expenditures, file = "Clean data/Rdata file/expenditures.RData")

save(pop, file = "Clean data/Rdata file/population.RData")


### Summary expenditures by state

## Constant value (adjusted for inflation)

sum_ex_adj <- expenditures %>% 
  group_by(State) %>% 
  summarize(Mean = mean(Constant, na.rm=T),
          Median = median(Constant, na.rm=T),
          Standard_Deviation = sd(Constant, na.rm=T),
          Lower_Quartile = quantile(Constant, 0.25, na.rm=T),
          Upper_Quartile = quantile(Constant, 0.75, na.rm=T)) %>% 
  dplyr::select(State, Lower_Quartile, Standard_Deviation, Median, Mean, Upper_Quartile)

save(sum_ex_adj, file = "Clean data/Rdata file/sum_ex_adj.RData")

## Current value (not adjusted for inflation)

sum_ex <- expenditures %>% 
  group_by(State) %>% 
  summarize(Mean = mean(Current, na.rm=T),
            Median = median(Current, na.rm=T),
            Standard_Deviation = sd(Current, na.rm=T),
            Lower_Quartile = quantile(Current, 0.25, na.rm=T),
            Upper_Quartile = quantile(Current, 0.75, na.rm=T)) %>% 
  dplyr::select(State, Lower_Quartile, Standard_Deviation, Median, Mean, Upper_Quartile)

save(sum_ex, file = "Clean data/Rdata file/sum_ex.RData")


## DATASETS NAMES

# Median earnings by race                       race_income
# Median earnings by gender                     gender_income
# Median income & poverty:                      income_poverty
# Expenditures:                                 expenditures
# Population & graduates ratio:                 pop
# ??Teacher Salary:                             final_teacher_salary
# Summary expenditures by State (not adjusted)  sum_ex
# Summary expenditures by State (adjusted)      sum_ex_adj





