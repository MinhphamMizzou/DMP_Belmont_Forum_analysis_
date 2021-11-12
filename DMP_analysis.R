#---retrieve packages 
library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)
library(psych)
library(scales)
library(tidytext)


#---import data
dtm<- read_excel("/Users/minhpham/Downloads/DMP_Scoring.xlsx", 1)
View(dtm)

#---process data for the analysis
str(dtm)
dtm$O5 <- as.numeric(dtm$O5)
dtm$O6 <- as.numeric(dtm$O6)
dtm$O8 <- as.numeric(dtm$O8)

dtm_1 <- dplyr::select(dtm, O1:A8)

dtm$criteria_s <- dtm_1%>%
  summarise(mean = round(rowMeans(., na.rm = T), 2)) # criteria score

str(dtm)
dtm$criteria_s <- unlist(dtm$criteria_s)
dtm$criteria_s <- as.numeric(dtm$criteria_s)

dtm_long <- gather(dtm_wide, Criteria, value, 'Data collection methods':'Timeframe for access', factor_key = TRUE)


dtm_wide <- spread(dtm_long, Criteria, value)
str(dtm_wide)

#----average score for each plan
dtm_wide_1 <- dplyr::select(dtm_wide, 'Data collection methods':'Timeframe for access')
dtm_wide$plan_s <- dtm_wide_1%>%
  summarise(mean = round(rowMeans(., na.rm = T), 2))
dtm_wide$plan_s_c <- ifelse(dtm_wide$plan_s>=1, "DMPs with average scores ≥ 1", "DMPs with average score <1")
str(dtm_wide)
dtm_wide$plan_s <- unlist(dtm_wide$plan_s)
dtm_wide$plan_s <- as.numeric(dtm_wide$plan_s)

dtm_wide$plan_s_c <- unlist(dtm_wide$plan_s_c)
dtm_wide$plan_s_c <- as.factor(dtm_wide$plan_s_c)

#---word count
WC <- read_excel("/Users/minhpham/Downloads/DMP_Scoring.xlsx", 2)
str(WC)
WC <- as.data.frame(WC)
WC$Plan <- as.factor(WC$Plan)


dtm_wide <- dplyr::left_join(dtm_wide, WC, by = "Plan")




#dtm_wide_1 <- dtm_wide%>%
group_by(plan_s)%>%
  summarize(count=n()) # count unique value

#---Visualizations and Analysis
#----Figure 1. Composite score distribution for data management plans (N = 21)
ggplot(dtm_wide, aes(plan_s)) +
  geom_bar() + 
  theme_bw() +
  geom_bar(alpha = .5) +
  theme(strip.text=element_text(size=11),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(color="black", size = 10)
  ) +
  labs(x = "Score", y = "Count") 

#----Composite scores of data management plans for Table 1.
psych::describe(dtm_wide$plan_s) # total

dtm_wide%>% # composite scores by sets
  group_by(plan_s_c)%>%
  summarize(n = n(),
            mean = mean(plan_s),
            sd = sd(plan_s),
            median = median(plan_s),
            min = min(plan_s),
            max = max(plan_s)
  )

#----Figure 2. Scores of individual scorecard criteria
dtm %>%
  mutate(Criteria = reorder(Criteria, criteria_s))%>%
  ggplot(aes(Criteria, criteria_s)) +
  geom_col(show.legend = FALSE, fill = "grey") +
  scale_x_reordered() +
  coord_flip() +
  theme_bw()+
  theme(strip.text=element_text(size=11),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(color="black", size = 10)) +
  labs(x = NULL, y = "Composite scores") +
  geom_text(aes(label = paste0(round(criteria_s, 2), ""), hjust = .3))

#----Figure 3. Criteria scores: DMPs with scores averaging ≥1 and with scores averaging <1
dtm_long$plan_s_c_n <- ifelse(dtm_long$plan_s_c== "DMPs with average score <1", "Average scores < 1",
                              "Average scores ≥ 1")
dtm_long%>%
  group_by(plan_s_c_n, Criteria)%>%
  summarise(mean = mean(value, na.rm = T))%>%
  mutate(Criteria = reorder(Criteria, mean))%>%
  ggplot(aes(Criteria, mean)) +
  facet_wrap(~ plan_s_c_n) + 
  theme_bw() +
  geom_col(alpha = .5) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(color="black", size = 10)) +
  labs(x = NULL, y = "Composite scores") +
  geom_text(aes(label = paste0(round(mean, 2), ""), hjust = .3)) 


#----Figure 4. Wordcount vs. composite score
dtm_wide$Set <- dtm_wide$plan_s_c
ggplot(dtm_wide, aes(x = Wordcount, y = plan_s, color = Set, shape = Set)) +
  geom_point(size = 3) +
  theme_bw()+
  geom_point(alpha = .5) +
  theme(strip.text=element_text(size=11),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(color="black", size = 10),
        legend.position = "bottom") +
  labs(x = "Word count", y = "Composite scores") +
  expand_limits(x = 0, y = 0) 

#----Table 4. Average word counts of Sample DMPs across the two sets
psych::describe(WC$Wordcount)
dtm_wide%>% # Average word counts by sets
  group_by(plan_s_c)%>%
  summarize(n = n(),
            mean = mean(Wordcount),
            sd = sd(Wordcount),
            median = median(Wordcount),
            min = min(Wordcount),
            max = max(Wordcount)
  )



str(dtm_wide_n)
dtm_wide_n_1 <- dtm_wide_n%>%
  filter(plan_s_c == "DMPs with average scores ≥ 1")
psych::describe(dtm_wide_n_1$Wordcount)

dtm_wide_n_1 <- dtm_wide_n%>%
  filter(plan_s_c == "DMPs with average score <1")
psych::describe(dtm_wide_n_1$Wordcount)

psych::describe(WC$Wordcount)

