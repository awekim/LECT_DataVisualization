library(ggplot2)
library(tidyverse)

df <- read_csv("C:/Users/s-joe/Downloads/df.csv")

df$성별 <- as.factor(df$성별)
df$음주여부 <- as.factor(df$음주여부)

df %>% 
  group_by(성별, 음주여부) %>% 
  summarise(음주 = n()) %>% 
  filter(is.na(음주여부) == FALSE) %>% 
  mutate(비율 = 음주/nrow(df)*100) %>% 
  select(-음주) %>% 
  ungroup() %>% 
  ggplot(aes(x=음주여부, y=비율, fill=성별)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#FFA07A", "#20B2AA")) + 
  labs(title="성별에 따른 음주 비율", x="음주 여부", y="비율 (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),  
    axis.title.x = element_text(size = 14),             
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 13),             
    axis.text.y = element_text(size = 10)
  )