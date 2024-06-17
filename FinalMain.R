#install.packages("ggplot2")
library(ggplot2)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("devtools")
library(devtools)
#devtools::install_github("lbenz730/ncaahoopR")
library(ncaahoopR)
#install.packages("extrafont")
library(extrafont)
#install.packages("cowplot")
library(cowplot)
#devtools::install_github("jjvanderwal/SDMTools")
#devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)
#devtools::install_github("FCrSTATS/SBpitch")
library(SBpitch)


# Fetch competition and match data
competition <- FreeCompetitions() %>% filter(competition_name == "FIFA World Cup" & season_name == "2022") #Here to change for a different match just replace the variables competition name and season name
competition_data <- FreeMatches(competition)
competition_data <- competition_data %>% filter(match_id == 3869685) #you can change match id for another match in the database
single_match_data <- free_allevents(MatchesDF = competition_data, Parallel = T)

# Filter and summarize the data to get total passes per Argentina player
argentina_players <- single_match_data %>%
  filter(team.name == "Argentina" & type.name == "Ball Receipt*") %>% #you can change team.name == whatever team you want to distribute
  group_by(player.name) %>%
  summarise(total_passes = n())

# Print the summarized data
print(argentina_players)

# Plot the data using ggplot2
ggplot(argentina_players, aes(x = reorder(player.name, total_passes), y = total_passes)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Total Received Passes",
       x = "Player",
       y = "Total Received Passes") +
  theme_minimal()

# Filter and summarize the data to get total passes per Argentina player
passA_players <- single_match_data %>%
  filter(team.name == "Argentina" & type.name == "Pass") %>%
  group_by(player.name) %>%
  summarise(total_passes = n())

# Create a scatterplot using ggplot2
ggplot(passA_players, aes(x = player.name, y = total_passes)) +
  geom_point() +
  geom_text(aes(label = total_passes), vjust = -0.5, hjust = 0.5) +
  labs(title = "Total Passes per Argentina Player",
       x = "Player",
       y = "Total Passes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Fetch the data
comps <- FreeCompetitions() %>% filter(competition_name == "FIFA World Cup" & season_name == "2022") #Here to change for a different match just replace the variables competition name and season name
comp_data <- FreeMatches(comps)


# Filter for the specific match
specific_match <- comp_data%>% filter(match_id == 3869685) #you can change match id for another match in the database

events <- free_allevents(MatchesDF = specific_match,Parallel = T)
events <- allclean(events)

# Filter for Argentina passes in the specific match
Argentina_passes <- events %>% filter(team.name == "Argentina" #you can change team.name == whatever team you want to distribute
                                   , type.name == "Pass", is.na(pass.outcome.name))

# Select necessary columns
Argentina_passes <- Argentina_passes %>% select(period, minute, type.name, pass.length, pass.angle, player.name, position.name, pass.recipient.name, 
                                        pass.outcome.name, pass.height.name, pass.body_part.name, location.x, location.y, 
                                        pass.end_location.x, pass.end_location.y, carry.end_location.x, carry.end_location.y, 
                                        shot.end_location.x, shot.end_location.y, shot.end_location.z)

# Define color palette
palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", direction = 1)

# Create the heatmap
p1 <- create_Pitch(grass_colour = "gray15", background_colour = "gray15", line_colour = "white") + 
  geom_density_2d_filled(data = Argentina_passes, aes(x = pass.end_location.x, y = pass.end_location.y, fill = ..level..), 
                         alpha = .4, contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10)) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 120)) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) + 
  theme(legend.position = "none", 
        plot.background = element_rect(colour = "gray15", fill = "gray15"),
        plot.title = element_text(color = "white", hjust = .5, size = 22, family = "Comic Sans MS", face = "bold", vjust = -1),
        plot.subtitle = element_text(color = "white", hjust = .5, size = 10, family = "Comic Sans MS", face = "bold", vjust = -4),
        plot.caption = element_text(color = "white", hjust = .5, size = 10, family = "Comic Sans MS", face = "bold", vjust = 4)) +
  labs(title = "Argentina World Cup Passes 2022",
       subtitle = "France vs Argentina", 
       caption = "")

# Plot the heatmap
ggdraw(p1) + theme(plot.background = element_rect(fill = "gray15", color = NA))


#--------------------------------------------------------------------------------------------------------------------------------------------


#Goal representation plot
# -------- Filter Statsbomb's free competition repository
competition <- FreeCompetitions() %>% filter(competition_name == "FIFA World Cup" & season_name=="2022") #Here to change for a different match just replace the variables competition name and season name

# -------- Use Statsbomb's function to scrape match information from that competition
competition_data <- FreeMatches(competition)

# -------- Use Statsbomb's function to scrape single match data from that competition
competition_data <- competition_data %>% filter(match_id == 3869685) #you can change match id for another match in the database
single_match_data <- free_allevents(MatchesDF = competition_data, Parallel = T)

# -------- Statsbomb's own cleaning function to wrangle the data in the most usable form
single_match_data <- allclean(single_match_data)

# -------- filter the game's event data for specific team and type of event 
unique(single_match_data$type.name)
single_team <- single_match_data %>% filter(team.name == "Argentina", type.name == "Shot") #you can change team.name == whatever team you want to distribute

# -------- Select specific columns to decrease clutter for easier analyzation
single_team_shots <- single_team %>% select(period, minute, type.name, pass.length, pass.angle, player.name, 
                                            location.x, location.y, shot.statsbomb_xg, shot.technique.name, shot.body_part.name, shot.type.name,
                                            shot.outcome.name, shot.end_location.x, shot.end_location.y, shot.end_location.z)

# -------- Create goal outcome column
single_team_shots <- single_team_shots %>% 
  mutate(
    goal = case_when(
      shot.outcome.name == "Goal" ~ "True",
      shot.outcome.name != "Goal" ~ "False",
    )
  )

# -------- filter out an penalty shootout shot (period 5 refers to shootouts)
single_team_shots <- single_team_shots %>%
  filter(period != 5)


# --------------------------------- plotting and theme

# create pitch and theme
p1 <- create_Pitch(grass_colour = "gray15", background_colour = "gray15", line_colour = "gray40") +
  # plot a point for each shot by x and y location ()
  geom_point(single_team_shots, mapping = aes(x = location.x, y = location.y, fill = goal, size = shot.statsbomb_xg), 
             color = "gray60", pch=21) +
  # set the scale for the size and labels of the expected goal for each shot
  scale_size_continuous(limits=c(0,1), breaks = c(.25,.5,.75, 1), labels=c(".25", ".5", ".75", "1")) + 
  # set the color and legend label manually for the fill value (goal or no goal)
  scale_fill_manual(breaks=c("True", "False"), values = c("green3", "gray15"), labels=c("Goal", "No Goal")) +
  # set the limits for the x-axis
  scale_x_continuous(limits = c(0, 120)) +
  # set the limits for the y-axis
  scale_y_continuous(limits = c(0, 80)) +
  # theme elements for the plot
  theme(
    plot.background = element_rect(colour = "gray15", fill = "gray15"),
    plot.title = element_text(color = "white", hjust = .5, size = 20, family = "Comic Sans MS", face = "bold", vjust = -1),
    plot.subtitle = element_text(color = "lightgrey", hjust = .5, size = 8, family = "Comic Sans MS", face = "bold", vjust = -2),
    plot.caption = element_text(color = "white", hjust = .5, size = 6, family = "Comic Sans MS", face = "bold", vjust = 4),
    legend.position = c(.5,.2),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill = "gray20", colour = "transparent"),
    legend.title = element_text(hjust = .4, vjust = .5, size = 10, family = "Comic Sans MS", face = "bold", colour = "white"),
    legend.text = element_text(hjust = .4, vjust = .5, size = 8, family = "Comic Sans MS", face = "bold", colour = "white"),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.box.just = "center",
    legend.margin = margin(t = .1, b=.1, r=.1, l=.1, unit='cm')
  ) +
  # create the plot's title, subtitle, and legend titles
  labs(title = "Argentina VS France - 2022 FIFA World Cup Final Shot Map",
       subtitle = "Includes all open-play and set piece shots",
       fill = "Outcome",
       size = "xG") +
  # flip the horizontal pitch to a vertical layout
  coord_flip(xlim = c(60, 120), ylim = c(0,80)) +
  # order the legends, setting fill 1st and size 2nd
  guides(fill = guide_legend(order = 1))


p1 + draw_image("https://static.vecteezy.com/system/resources/previews/015/731/283/non_2x/argentina-and-france-flag-with-names-symbol-design-latin-america-and-europe-football-final-latin-american-and-european-countries-football-teams-illustration-vector.jpg", 
                x = 100, y = 0, width = 12, height = 12) 


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Load necessary libraries
library(igraph)
library(ggraph)
library(tidyverse)
library(scales)

# Example edge list preparation 
edge_list <- Argentina_passes %>%
  filter(!is.na(pass.recipient.name)) %>%
  group_by(player.name, pass.recipient.name) %>%
  summarise(weight = n()) %>%
  ungroup()

# Create graph object
g <- graph_from_data_frame(d = edge_list, directed = TRUE)

# Calculate node sizes based on the total number of passes each player is involved in
node_degrees <- degree(g, mode = "all")
node_sizes <- rescale(node_degrees, to = c(10, 25))  # Rescale using scales package to increase/decrease node size!

position_groups <- list(
  Goalkeeper = c("Goalkeeper"),
  Defender = c("Left Back", "Right Back", "Left Center Back", "Right Center Back","Center Back"),
  Midfielder = c("Right Wing Back", "Left Wing Back", "Left Defensive Midfield", "Right Defensive Midfield",
                 "Center Defensive Midfield", "Center Midfield", "Attacking Midfield","Left Center Midfield","Right Center Midfield"),
  Forward = c("Left Wing", "Right Wing", "Center Forward")
)


get_role <- function(position) {
  for (key in names(position_groups)) {
    if (position %in% position_groups[[key]]) {
      return(key)
    }
  }
  return("Unknown")  # Return "Unknown" if position doesn't match any group
}


player_positions <- setNames(Argentina_passes$position.name, Argentina_passes$player.name)  # Map player names to positions
V(g)$role <- sapply(V(g)$name, function(player) get_role(player_positions[player]))

print(data.frame(Player = V(g)$name, Role = V(g)$role))


# Print out players and their roles for verification
print(data.frame(Player = V(g)$name, Role = V(g)$role))

# Calculate node colors based on degree centrality
node_colors <- rev(heat.colors(10))  # Generate a gradient from blue to red

# Assign colors based on degree centrality
V(g)$color <- node_colors[rescale(node_degrees, to = c(1, 10))]

# Create a 3D spherical layout
l <- layout_on_sphere(g)

# Plotting in 3D using igraph without labels
plot(g, layout = l, vertex.size = node_sizes, vertex.color = V(g)$color, edge.color = "gray", edge.width = 1,
     vertex.label = NA,  # Disable default vertex labels
     main = "Argentina Team Pass Network in 2022 World Cup Match",
     sub = "Nodes are colored from (Yellow to Red) based on the players passing abilities",
     edge.arrow.size = 0.4)  # Adjust arrow size here (smaller tips))

# Add custom vertex labels with resized text and color coding
for (i in 1:vcount(g)) {
  player_name <- V(g)$name[i]
  role <- V(g)$role[i]
  text(l[i, 1], l[i, 2], player_name, col = ifelse(role == "Goalkeeper", "orange", 
                                                   ifelse(role == "Defender", "blue", 
                                                          ifelse(role == "Midfielder", "black", "red"))), cex = 0.8, pos = 3)
}

# Add legend for role colors
legend("topleft", legend = c("Goalkeeper", "Defender", "Midfielder", "Forward"),
       col = c("orange", "blue", "black", "red"), pch = 16, pt.cex = 2, cex = 0.8, bty = "n")

# Add legend for node coloring
legend("bottomleft", legend = "Degree Centrality",
       fill = node_colors, density = NULL, angle = 45, cex = 0.8, bty = "n")

# Adjust plot parameters (e.g., background, font size)
par(bg = "white", cex = 1.2)  # Adjust background color and font size as needed
