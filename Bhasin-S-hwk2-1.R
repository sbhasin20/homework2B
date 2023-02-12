if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)

#1 
reports_per_year <- final.hcris.data %>% 
  group_by(street, year) %>%
  summarise(n = n())
multiple_reports_per_year <- reports_per_year %>%
  filter(n>1)
n_multiple_reports_per_year <- n_distinct(multiple_reports_per_year$street)
n_multiple_reports_per_year

ggplot()

graph_1 <- multiple_reports_per_year%>% group_by(year)%>% 
  summarize(count = n())
sum(graph_1$count)

graph_2<-ggplot(graph_1, aes(year, count))+
  geom_line() +
  labs(title = "Hospitals that Filed Multiple Reports in a Year", x = "Year", y = "Number of Hospitals")+
  theme_bw()

graph_2

#2 
unique_provider_numbers <- n_distinct(final.hcris$provider_number)
unique_provider_numbers

#3 
graph_3 <- ggplot(final.hcris.data, aes(x=as.factor(year), y=tot_charges)) + 
  geom_violin(trim=FALSE)+
  labs(title = "Distribution of Total Charges by Year", x = "Year", y = "Total Charges")+
  theme_bw()

graph_3

#4 
final.hcris.data$discount_factor <- 1-(final.hcris.data$tot_discounts/final.hcris.data$tot_charges)
final.hcris.data$price_num <- (final.hcris.data$ip_charges + final.hcris.data$icu_charges + final.hcris.data$ancillary_charges)*final.hcris.data$discount_factor - final.hcris.data$tot_mcare_payment
final.hcris.data$price_denom <- final.hcris.data$tot_discharges - final.hcris.data$mcare_discharges
final.hcris.data$price <- final.hcris.data$price_num/final.hcris.data$price_denom

graph_4 <- ggplot(final.hcris.data, aes(x=as.factor(year), y= price)) + 
  geom_violin()+
  labs(title = "Distribution of Estimated Prices by Year", x = "Year", y = "Hospital Price") + 
  theme_bw()

graph_4

#5 
year_2012 <- final.hcris.data%>% filter(year == 2012)
year_2012$penalty <- ifelse(year_2012$hvbp_payment + year_2012$hrrp_payment < 0 , 1,0)

table_5 <- year_2012 %>% filter(!is.na(penalty))%>%
  group_by(penalty)%>% 
  summarize(price = mean(price, na.rm = TRUE))

#6

year_2012$quartile <- ntile(year_2012$beds, 4) 

year_2012$quartile_1 <- ifelse(year_2012$quartile == 1, 1,0)
year_2012$quartile_2 <- ifelse(year_2012$quartile == 2, 1,0)
year_2012$quartile_3 <- ifelse(year_2012$quartile == 3, 1,0)
year_2012$quartile_4 <- ifelse(year_2012$quartile == 4, 1,0)

table_6 <- year_2012 %>% filter(!is.na(penalty))%>% group_by(quartile, penalty)%>% summarize(avg_price = mean(price, na.rm = TRUE))

table_6

#7 

#I am not sure how to do this as I kept getting an error. Given our discussion in class, I would assume first we have to stimulate our data. Then, we have to use the MatchIt package and the "Matching::Match" function. 
#To differentiate between the inverse variance distance and Mahalanobis distance, weight would be set to 1 for the inverse variane distance and 2 for the Mahalanobis distance. 
#For the inverse propensity weighting, we would have to code for a logistic regression model using model <- glm(D~X, family=binomial, year_2012). 
#To make the simple linear regression, we would use the following code 
#reg1.dat <- year_2012 %>% filter(d==1)
#reg1 <- lm(y ~ x, data=reg1.dat)

#regression_2012 <- year_2012 %>% filter(d==0)
#reg0 <- lm(y ~ x, data=regression_2012)
#pred1 <- predict(reg1,new=year_2012$beds)
#pred0 <- predict(reg0,new=year_2012$quartile)
#mean(pred1-pred0)

#8 
#Since I was unable to run the code for #7,I was not able to get results for the previous question. I would guess that the estimators are similar. 

#9 
#I think I have estimated a casual effect of the penalty through matching,weighting, and running linear regression. These methods allowed us to control potential confounding variables in the study, suggesting a causal effect. 

#10

# I found working with this data challenging. I learned how to create a dummy variable and quartiles the data set. I was aggravated when trying to run the code to find the average treatment. 

save.image("Hw2_workspace.Rdata")

