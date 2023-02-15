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

table_5

#6

year_2012 <- year_2012 %>% 
  filter(!is.na(penalty))%>% 
  filter(!is.na(price))

year_2012$quartile <- ntile(year_2012$beds, 4) 

year_2012$quartile_1 <- ifelse(year_2012$quartile == 1, 1,0)
year_2012$quartile_2 <- ifelse(year_2012$quartile == 2, 1,0)
year_2012$quartile_3 <- ifelse(year_2012$quartile == 3, 1,0)
year_2012$quartile_4 <- ifelse(year_2012$quartile == 4, 1,0)

table_6 <- year_2012 %>% group_by(quartile, penalty)%>% summarize(avg_price = mean(price, na.rm = TRUE))

table_6

#7

#a 
inv_var <- Matching::Match(Y=year_2012$price,
                           Tr=year_2012$penalty,
                           X=year_2012$quartile,
                           M=1,
                           Weight=1,
                           estimand="ATE")
summary(inv_var)

#b
Maha <- Matching::Match(Y=year_2012$price,
                           Tr=year_2012$penalty,
                           X=year_2012$quartile,
                           M=1,
                           Weight=2,
                           estimand="ATE")
summary(Maha)

#c 
#propensity score

# I was unable to get the code to run for the propensity scores. When I ran the following code, I kept getting an error that 'ps' must be size 1, not 673 and where this error happened. I think R i trying to tell me I am inputting too many values in the log regression model. I am unsure how to fix this problem right now. I think I may have to make a data frame to adjust the number of input values. 

#logit.model <- glm(penalty ~ quartile_1 + quartile_2 + quartile_3 + quartile_4,
                  # data = year_2012, family = binomial)

#ps <- fitted(logit.model)

#year_2012 <- year_2012 %>%
 # mutate(ps = predict(logit.model, type = 'response')) %>%
  #filter(ps>0 & ps<1)



#IPW Weights 

#year_2012 <- year_2012 %>%
 # mutate(ipw = case_when(
   # penalty == 1 ~ 1/ps,
    #penalty == 0 ~ 1/(1=ps),
    #TRUE~NA_real_ ))

#view(year_2012)

#mean.t1 <- year_2012 %>% 
 # filter(penalty==1) %>% 
 # dplyr::select(price, ipw) %>%
  #summarize(mean_y=weighted.mean(price, w=ipw))
#mean.t0 <- year_2012 %>% 
 # filter(penalty==0) %>% 
  #dplyr::select(price, ipw) %>%
 # summarize(mean_y=weighted.mean(price, w=ipw))

#mean.t1$mean_y - mean.t0$mean_y
#reg.ipw <- lm(price ~ penalty, data=year_2012, weights=ipw)

#reg.ipw

#d 

reg1.dat <- year_2012 %>% filter(penalty==1)
reg1 <- lm(price ~ penalty, data=reg1.dat)

reg0.dat <- year_2012 %>% filter(penalty==0)
reg0 <- lm(price ~ penalty, data=reg0.dat)
pred1_alt <- predict(reg1,new=year_2012)
pred0_alt <- predict(reg0,new=year_2012)
mean(pred1_alt-pred0_alt)

#8 Although I was unable to run the code for all of the different treatments, based on the results I got thus far, the results are similar.

#9 I think I have estimated a causal effect of the penalty. I think getting similar estimators by matching, weighting, and running regressions on the data allowed us to control for potential confounding variables, which would suggest a causal effect. 

#10 I found working with this data challenging but easier than homework 1 as I am getting more comfortable trouble shooting and working with this application. I learned how to create a dummy variable and quartiles for a data set. It has been very aggravating to troubleshoot the error I keep getting with the propensity score. 

save.image("Hwk2_workspace.Rdata")                                                          
                                                           