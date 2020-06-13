rm(list=ls())

library(ggplot2)
library(wesanderson)
library(viridis)
library(forcats)
library(RCurl)

# Part 1: Top 30 Goalscorers

# Overall Goal-Scorer Summary 
goals = read.csv('Premier League Top Goal Scorers.csv',
                 header = T, fileEncoding = "UTF-8-BOM")
attach(goals)
#View(goals)

top_30_player = Player[0:30]
top_30_goals = Number_of_Goals[0:30]

goals_df = data.frame(y = top_30_player, 
                        x = top_30_goals,
                        stringsAsFactors = F)

plot_1 = 
  ggplot(goals_df, 
         mapping = aes(y = top_30_goals,
                       x = fct_reorder(top_30_player, - top_30_goals),
                       fill = as.numeric(fct_reorder(top_30_player,
                                                     - top_30_goals)))) +
  geom_col() +
  coord_flip() + 
  scale_fill_gradient("blue") +
  labs(title = "Top 30 Premier League Goalscorers",
       y = "Number of Goals",
       x = "Player Name",
       fill = "Player Name") +
  geom_text(aes(label = top_30_goals),
            color = "black", hjust = -0.25,
            vjust = 0.4)+
  theme(plot.title = element_text(size = 16,
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5),
        legend.position = "none")
plot_1

# Part 2: Combined Goals & Assists

combined = read.csv('Premier League Top Goal Scorers.csv',
                    header = T, fileEncoding = "UTF-8-BOM")
attach(combined)
#View(combined)

top_30_combined_player = Player[0:30]
top_30_combined = Combined_Goals_and_Assists[0:30]

combined_df = data.frame(y = top_30_combined_player, 
                         x = top_30_combined,
                         stringsAsFactors = F)

plot_2 = 
  ggplot(combined_df, 
         mapping = aes(y = top_30_combined, 
                       x = fct_reorder(top_30_combined_player, - top_30_combined),
                       fill = as.numeric(fct_reorder(top_30_combined_player,
                                - top_30_combined)))) +
  geom_col()+
  coord_flip() + 
  scale_fill_gradient("blue") +
  labs(title = "Top 30 Premier League Total Combined Goals & Assists",
       y = "Number of Goals & Assists",
       x = "Player Name",
       fill = "Player Name") +
  geom_text(aes(label = top_30_combined),
            color = "black", hjust = -0.25,
            vjust = 0.4)+
  theme(plot.title = element_text(size = 16,
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5),
        legend.position = "none")
plot_2

# Part 3: Assists Graph

# Overall Assists Summary 
assists = read.csv('Premier League Top Assists.csv',
                   header = T, fileEncoding = "UTF-8-BOM")

attach(assists)
#View(assists)

top_30_player = Player[0:30]
top_30_assists = Number_of_Assists[0:30]

assists_df = data.frame(y = top_30_player, 
                          x = top_30_assists,
                          stringsAsFactors = F)

plot_3 = 
  ggplot(assists_df, 
         mapping = aes(y = top_30_assists, 
                       x = fct_reorder(top_30_player, - top_30_assists), 
                       fill = as.numeric(fct_reorder(top_30_player,
                                                     - top_30_assists))))+
  geom_col()+
  coord_flip() + 
  scale_fill_gradient("blue") +
  labs(title = "Top 30 Premier League Assist Makers",
       y = "Number of Assists",
       x = "Player Name",
       fill = "Player Name") +
  geom_text(aes(label = top_30_assists),
            color = "black", hjust = -0.25,
            vjust = 0.4)+
  theme(plot.title = element_text(size = 16,
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5),
        legend.position = "none")
plot_3