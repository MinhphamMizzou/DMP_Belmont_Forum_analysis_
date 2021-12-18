#---retrieve packages 
library(readxl)
library(ggplot2)
library(dplyr)
library(scales)
library(tidytext)
library(jtools)





#---import data
dtm<- read_excel("/Users/minhpham/Downloads/DMP_Scoring.xlsx", 1)
dtm <- dtm[, -1]

str(dtm)
dtm$O5 <- as.numeric(dtm$O5)
dtm$O6 <- as.numeric(dtm$O6)
dtm$O8 <- as.numeric(dtm$O8)

#----composite score distribution for plans
dtm_cs <- dtm[, -1]
#-transpose
dtm_cs_t <- t(dtm_cs)
dtm_cs_t <- as.data.frame(dtm_cs_t)

dtm_cs_t$score <- dtm_cs_t%>%
  summarise(mean = round(rowMeans(., na.rm = T), 2)) # criteria score

score_d <- dtm_cs_t%>%
  group_by(score)%>%
  summarize(count=n())

score_d <- as.data.frame(score_d)

score_d$score <- ifelse(score_d$score<1, "<1",
                        ifelse(score_d$score>=1 & score_d$score<=1.25, "1-1.25",
                               ifelse(score_d$score>1.25 & score_d$score<=1.50, "1.26-1.50",
                                      ">1.50")))

t <- score_d%>%
  group_by(score)%>%
  summarise(count = sum(count))%>%
  arrange(desc(count))
  
t$score <- unlist(t$score)

t <- as.data.frame(t)


t$percent <- round((t$count/21)*100, 2)


t$score <- factor(t$score, levels = c("<1", "1-1.25", "1.26-1.50", ">1.50"))


plot1 <- t%>%
    ggplot(aes(x = score, 
               y = (t$count/21))) +
    geom_bar(stat = "identity", alpha = .5) +
    geom_text(aes(label = percent(count/21), vjust = -0.2)) +
    labs (
      x = "Composite Score", y = NULL)+
  scale_y_continuous(labels = scales::percent)


plot1 + theme_apa()
  
# Scores of individual scorecards
dtm_1 <- dplyr::select(dtm, O1:A8)
  
dtm$criteria_s <- dtm_1%>%
  summarise(mean = round(rowMeans(., na.rm = T), 2)) # criteria score
  
dtm$criteria_s <- unlist(dtm$criteria_s)
dtm$criteria_s <- as.numeric(dtm$criteria_s)


    
plot2 <- dtm %>%
      mutate(Criteria = reorder(Criteria, criteria_s))%>%
      ggplot(aes(Criteria, criteria_s)) +
      geom_col(show.legend = FALSE, alpha = .5) +
      scale_x_reordered() +
      coord_flip() +
      geom_text(aes(label = criteria_s, hjust = 0.4)) +
      labs(x = NULL, y = "Average Score") +
      theme(axis.text.x = element_text(color = "black"),
            axis.text.y = element_text(color = "black"))

plot2 + theme_apa()

#----Figure 3. Criteria scores: DMPs with scores averaging ≥1 and with scores averaging <1
dtm_long <- gather(dtm, Criteria, value, 'O1':'A8', factor_key = TRUE)
dtm_long <- gather(dtm, Plan, value, 'O1':'A8', factor_key = TRUE)


plan_s <- dtm_long%>%
  group_by(Plan)%>%
  summarise(score = round(mean(value, na.rm = T), 2))
plan_s$score_1 <- ifelse(plan_s$score>=1, "Sufficient DDOMPs", "Insufficient DDOMPs")
plan_s <- plan_s [, -2]

dtm_n <- dtm[, -23]
dtm_long_n <- left_join(dtm_long, plan_s, by = "Plan")


plot3 <- dtm_long_n%>%
  group_by(score_1, Criteria)%>%
  summarise(mean = mean(value, na.rm = T))%>%
  mutate(Criteria = reorder(Criteria, mean))%>%
  ggplot(aes(Criteria, mean)) +
  facet_wrap(~ score_1) + 
  theme_bw() +
  geom_col(alpha = .5) +
  scale_x_reordered() +
  coord_flip() +
  labs(x = NULL, y = "Average Score") +
  geom_text(aes(label = paste0(round(mean, 2), ""), hjust = .3)) 

plot3 + theme_apa()

#---word count
WC <- read_excel("/Users/minhpham/Downloads/DMP_Scoring.xlsx", 2)
WC <- as.data.frame(WC)
WC$Plan <- as.factor(WC$Plan)

plan_s <- dtm_long%>%
  group_by(Plan)%>%
  summarise(score = round(mean(value, na.rm = T), 2))
plan_s$Set <- ifelse(plan_s$score>=1, "Sufficient DDOMPs", "Insufficient DDOMPs")

plan_s <- dplyr::left_join(plan_s, WC, by = "Plan")

plot4 <- ggplot(plan_s, aes(x = Wordcount, y = score, color = Set, shape = Set)) +
  geom_point(size = 3) +
  geom_point() +
  theme(legend.position = "bottom") +
  labs(x = "Word Count", y = "Average Score") +
  expand_limits(x = 0, y = 0) 


plot4 + theme_apa()







