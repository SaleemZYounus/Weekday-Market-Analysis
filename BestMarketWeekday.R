library(tidyverse)
library(infer)
library(readr)
library(scales)
library(lubridate)
library(ggplot2)

s_p <- read_csv("S&P Historical Data.csv")

new_sp = s_p %>% 
  select(Date, "Close/Last") %>%
  rename("Price" = "Close/Last") %>%
  mutate(row=row_number()) %>%
  arrange(desc(row)) %>%
  mutate(Returns = (((Price)-lag(Price))/lag(Price))) 


date = as.Date(new_sp$Date, format = "%m/%d/%Y")
new_sp$Day_of_Week <- weekdays(date) 
new_sp

tidy_sp = drop_na(new_sp) %>%
  group_by(Day_of_Week) %>%
  summarise(Returns = percent(mean(Returns), accuracy = .0001)) %>%
  slice(match(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), Day_of_Week))
tidy_sp 
ggplot(new_sp, aes(date, Price))+
  geom_line() + 
  ggtitle("Price of S&P")+  
  geom_smooth(method = "lm", se = FALSE)

ggplot(new_sp, aes(date, Returns))+
  geom_line() + 
  ggtitle("Returns of S&P")

ggplot(tidy_sp, aes(x = factor(Day_of_Week, level=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")), y = Returns, fill = Day_of_Week)) +
  geom_col() +
  ggtitle("Average Returns of S&P by Day of the Week")+
  xlab('Day of week')

ggsave("Average Returns of S&P by Day of the Week")
 # Hypothesis Test on we\hether the difference is Significant
linear_model <- lm(Returns ~ Day_of_Week, data = new_sp)  
write.table(anova(linear_model), file = "Anova Model")
