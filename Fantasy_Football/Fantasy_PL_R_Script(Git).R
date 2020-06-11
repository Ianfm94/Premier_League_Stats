rm(list=ls())

# Required packages, you might need to install these
library(ggplot2)
library(dplyr)
library(plotly)
library(tibble)
library(forcats)
library(Rcurl)

## Fantasy_PL Data 

fpl_data = read.csv('2020-06-01_updated_fpl_stats.csv', header = T,
                    fileEncoding = "UTF-8-BOM")
attach(fpl_data)
#View(fpl_data)

# Top 20 FPL Players

# Interactive Plot Workings

top_20_points = total_points[0:20]
top_20_player_pos = factor(player_pos)[0:20]
top_20_surnames = factor(web_name)[0:20]
top_20_team = factor(team_name)[0:20]

color_table = tibble(
  Team_Name = c("Arsenal", "Burnley", "Chelsea", "Everton", 
                "Leicester City", "Liverpool", "Manchester City",
                "Manchester United", "Norwich City",
                "Sheffield United", "Southampton", 
                "Wolverhampton Wanderers"),
  Team_Color = c("#EF0107", "#6C1D45", "#034694", "#003399",
                 "#003090", "#C8102E", "#6CABDD", "#DA291C",
                 "#FFF200", "#EE2737", "#D71920", "#FDB913"),
  Team_Color_2 = c("white", "white", "white", "white",
                   "white", "white", "white", "white",
                   "white", "white", "white", "white")
)

position_table = tibble(
  Position_Name = c("Striker", "Midfielder", "Defender", "Goalkeeper"),
)

fpl_df = data.frame(y = top_20_points, 
                    x = top_20_player_pos, 
                    z = top_20_surnames,
                    w = top_20_team,
                    stringsAsFactors = F)

fpl_df$w = factor(fpl_df$w, levels = color_table$Team_Name)
fpl_df$x = factor(fpl_df$x, levels = position_table$Position_Name)

names(fpl_df)[names(fpl_df) == "x"] = "Position_Name"
names(fpl_df)[names(fpl_df) == "y"] = "Total_Points_by_Position"
names(fpl_df)[names(fpl_df) == "z"] = "Player_Surname"
names(fpl_df)[names(fpl_df) == "w"] = "Team_Name"
View(fpl_df)

plot_fpl_1 = ggplot(fpl_df, aes(x = Position_Name,
                                y = Total_Points_by_Position,
                                z = fct_reorder(Player_Surname, 
                                                - Total_Points_by_Position),
                                fill = Team_Name,
                                color = Team_Name,
                                text = paste("Player Surname:", fct_reorder(Player_Surname, 
                                                                            - Total_Points_by_Position)))) +
  geom_col() +
  scale_fill_manual(values = color_table$Team_Color) + 
  scale_color_manual(values = color_table$Team_Color_2) +
  labs(title = "Top 20 Fantasy PL Pointscorer by Position & Team",
       y = "Player Fantasy Points, (sorted from lowest to highest L-R)",
       x = "Player Positions",
       fill = "Team Name") +
  theme_bw() +
  coord_flip() + 
  theme(plot.title = element_text(size = 14,
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5),
        legend.title = element_text(color = "navy",
                                    face = "bold",
                                    size = 10),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position = "none")

plot_fpl_1 = ggplotly(plot_fpl_1, 
                      tooltip = c("x", "y", "text", "fill"))
plot_fpl_1

htmlwidgets::saveWidget(as_widget(plot_fpl_1),
                        "Top_20_Fantasy_PL_Pointscorers.html")

## Top 100 FPL Pointscorers

# Interactive Plot Workings

top_100_points = total_points[0:100]
top_100_player_pos = factor(player_pos)[0:100]
top_100_surnames = factor(web_name)[0:100]
top_100_team = factor(team_name)[0:100]

color_table = tibble(
  Team_Name = c("Arsenal", "Aston Villa", "Bournemouth", "Brighton & Hove Albion",
                "Burnley", "Chelsea", "Crystal Palace", "Everton", 
                "Leicester City", "Liverpool", "Manchester City",
                "Manchester United", "Newcastle United", "Norwich City",
                "Sheffield United", "Southampton", "Tottenham Hotspurs",
                "Watford", "West Ham United", "Wolverhampton Wanderers"),
  Team_Color = c("#EF0107", "#670E36", "#B50E12", "#0057B8",
                 "#6C1D45", "#034694", "#1B458F", "#003399",
                 "#003090", "#C8102E", "#6CABDD", "#DA291C",
                 "#241F20", "#FFF200", "#EE2737", "#D71920",
                 "#132257", "#FBEE23", "#7A263A", "#FDB913"),
  Team_Color_2 = c("white", "white", "white", "white",
                   "white", "white", "white", "white",
                   "white", "white", "white", "white",
                   "white", "white", "white", "white",
                   "white", "white", "white", "white")
)

position_table = tibble(
  Position_Name = c("Striker", "Midfielder", "Defender", "Goalkeeper"),
)

fpl_df = data.frame(y = top_100_points, 
                    x = top_100_player_pos, 
                    z = top_100_surnames,
                    w = top_100_team,
                    stringsAsFactors = F)

fpl_df$w = factor(fpl_df$w, levels = color_table$Team_Name)
fpl_df$x = factor(fpl_df$x, levels = position_table$Position_Name)

names(fpl_df)[names(fpl_df) == "x"] = "Position_Name"
names(fpl_df)[names(fpl_df) == "y"] = "Total_Points_by_Position"
names(fpl_df)[names(fpl_df) == "z"] = "Player_Surname"
names(fpl_df)[names(fpl_df) == "w"] = "Team_Name"
#View(fpl_df)

plot_fpl_2 = ggplot(fpl_df, aes(x = Position_Name,
                                y = Total_Points_by_Position,
                                z = fct_reorder(Player_Surname, 
                                                - Total_Points_by_Position),
                                fill = Team_Name,
                                color = Team_Name,
                                text = paste("Player Surname:", fct_reorder(Player_Surname, 
                                                                            - Total_Points_by_Position)))) +
  geom_col() +
  scale_fill_manual(values = color_table$Team_Color) + 
  scale_color_manual(values = color_table$Team_Color_2) +
  labs(title = "Top 100 Fantasy PL Pointscorer by Position & Team",
       y = "Player Fantasy Points, (sorted from lowest to highest L-R)",
       x = "Player Positions",
       fill = "Team Name") +
  theme_bw() +
  coord_flip() + 
  theme(plot.title = element_text(size = 14,
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5),
        legend.title = element_text(color = "navy",
                                    face = "bold",
                                    size = 10),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position = "none")

plot_fpl_2 = ggplotly(plot_fpl_2, 
                      tooltip = c("x", "y", "text", "fill"))
plot_fpl_2

htmlwidgets::saveWidget(as_widget(plot_fpl_2),
                        "Top_100_Fantasy_PL_Pointscorers.html")

## Fantasy_PL_MU Data 

fpl_data_mu = read.csv('2020-06-01_updated_fpl_stats_mu.csv',
                       header = T, fileEncoding = "UTF-8-BOM")
attach(fpl_data_mu)
#View(fpl_data_mu)

# Interactive Plot Workings

## Create Daily RoR IG

colour_table = tibble(
  Position_Name = c("Striker", "Midfielder",
                    "Defender", "Goalkeeper"), 
  Position_Colour = c("#DA020E", "#000000",  
                      "#696969", "#b8ae76"),
  Position_Colour_2 = c("white","white",
                        "white","white")
)

points = total_points
player_pos = factor(player_pos)
surnames = factor(web_name)

fpl_mu_df = data.frame(x = player_pos, 
                       y = points, 
                       z = surnames,
                       stringsAsFactors = F)

fpl_mu_df$x = factor(fpl_mu_df$x,
                     levels = colour_table$Position_Name)

names(fpl_mu_df)[names(fpl_mu_df) == "x"] = "Position_Name"
names(fpl_mu_df)[names(fpl_mu_df) == "y"] = "Player_Points"
names(fpl_mu_df)[names(fpl_mu_df) == "z"] = "Player_Surname"
#View(fpl_mu_df)

plot_fpl_mu_1 = ggplot(fpl_mu_df, aes(y = Player_Points,
                                      x = Position_Name,
                                      color = Position_Name,
                                      fill = Position_Name,
                                      z = fct_reorder(Player_Surname, - Player_Points),
                                      text = paste("Player Surname:", 
                                                   fct_reorder(Player_Surname, - Player_Points)))) +
  geom_col() +
  labs(title = "Manchester United Fantasy PL Pointscorers",
       y = "Player Fantasy Points, (sorted from lowest to highest L-R)",
       x = "Player Positions") +
  theme_bw() +
  coord_flip() +
  scale_fill_manual(values = colour_table$Position_Colour) +
  scale_color_manual(values = colour_table$Position_Colour_2) +
  theme(plot.title = element_text(size = 14,
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5),
        legend.title = element_text(color = "navy",
                                    face = "bold",
                                    size = 10),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position = "none")

plot_fpl_mu_1 = ggplotly(plot_fpl_mu_1,
                         tooltip = c("x", "y", "text"))
plot_fpl_mu_1

htmlwidgets::saveWidget(as_widget(plot_fpl_mu_1),
                        "Man_United_Fantasy_PL_Pointscorers.html")
