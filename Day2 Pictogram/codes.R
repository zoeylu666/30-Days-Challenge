library(tidyverse)
library(rvest)
library(tidyverse)
library(showtext)
library(sysfonts)
library(ggimage)


###Load csv file
df <- read_csv("./metacritic best tv shows of all time.csv")

##Filter the top 10 best tv shows
df1 <- df %>% filter(rank <=10)

#reoder the top shows based on userscores
df1 <- df1%>%   arrange(desc(userscore))

df1$userscore <- as.numeric(df1$userscore)
## Plot

p <- ggplot(df1, aes(x=title, y=userscore)) +
  geom_segment( aes(xend=title, yend=0), size = 3, color = "#aeb6bf", alpha = .5) +
    geom_point(
    color=ifelse(df1$title %in% c("The Office (UK): Season 3","The Office (UK): Season 1"), "#FF6666", "#808080"), 
    size=ifelse(df1$title %in% c("The Office (UK): Season 3","The Office (UK): Season 1"), 5, 4)
  ) +
coord_flip() +
  theme_bw() +
   theme(
    legend.position="none"
  ) +
  xlab("") +
  ylab("User Score") +
  ggtitle("Viewers' Rating on the Top 10 Best TV Shows")+
  geom_text(data = df1,
            aes(label = paste("Ranked",rank, ": ", userscore), x = title, y = userscore,
            color = "#66B2FF",
            size = 2,
            vjust = -.8,
            hjust = 1.5)) 
            
  p
  
  
  p_image <- p +
    geom_image(aes(x=title, image = img_url), y = 0,
               hjust = 1,
               inherit.aes = FALSE)
  
  p_image
  
  # Add annotation
 p <-  p_image + annotate("text", x=grep("The Office (UK): Season 3", df1$title), y=df1$userscore[which(df1$title=="The Office (UK): Season 3")]*1.2, 
               label="Out of these 10 TV shows, I only watched them", 
               color="orange", size=4 , angle=0, fontface="bold", hjust=0) + 
    
    annotate("text", x = grep("The Office (UK): Season 1", df1$title), y = df1$userscore[which(df1$title=="The Office (UK): Season 1")]*1.2, 
             label ="Out of these 10 TV shows, I only watched them"  , 
             color="orange", size=4 , angle=0, fontface="bold", hjust=0) 
      
 p      
 