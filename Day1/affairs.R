library(tidyverse)
library(ggplot2)
library(dplyr)
library(grid)
library(shadowtext)

df <- read_csv("./Affairs.csv")


# The colors
BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"



df_m <-  df %>% filter(gender == "male") 
df_f <-  df %>% filter(gender == "female")
df_male <-  df_m %>%
  group_by(children, affairs) %>%
  summarise(freq = n())


df_female <-  df_f  %>%
  group_by(children, affairs) %>%
  summarise(freq = n())


df_male$affairs <- as.factor(df_male$affairs)
df_female$affairs <- as.factor(df_female$affairs)

#male fidelity barplot
plt_m <- ggplot(df_male, aes(freq, affairs, fill = children, label = freq)) +
  geom_bar(stat="identity", alpha=.6, width=.4)+
  geom_text(position = "stack", hjust = 2.2, vjust = -2)+
  xlab("") +
  theme_bw()+
  labs(
    title = "Day 1 Challenge: Part to whole",
    subtitle = "Fidelity Among Married Male Partners with Children and Without Children"
  ) + 
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
      plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 15
    )
  )

plt_m

