library(tidyverse)
library(ggsoccer)
library(gganimate)

source("R/00-setup.R")

meta_df <- FreeCompetitions() 

liga_matches <- meta_df %>% 
  filter(competition_id == 11 &
           season_id != 278 #73/74 season (?)
         ) %>% 
  FreeMatches()

messi_events <- map_df(liga_matches$match_id, function(x){
  get.matchFree(liga_matches %>% filter(match_id == x)) %>% 
    as_tibble()  %>% 
    filter(type.name == "Pass" &
             pass.recipient.name ==  "Lionel Andrés Messi Cuccittini")
}
) 

# dictionary
#https://github.com/statsbomb/open-data/blob/master/doc/StatsBomb%20Open%20Data%20Specification%20v1.1.pdf
messi_receipts <- messi_events %>% 
  filter(is.na(pass.outcome.name) & #completed pass
           !pass.type.name %in% c("Corner","Kick Off", "Throw-in")) %>% 
  mutate(season = 
           case_when(
             season_id == 90 ~ "2020-21",
             season_id == 42 ~ "2019-20",
             season_id == 4 ~ "2018-19",
             season_id == 1 ~ "2017-18",
             season_id == 2 ~ "2016-17",
             season_id == 27 ~ "2015-16",
             season_id == 26 ~ "2014-15",
             season_id == 25 ~ "2013-14",
             season_id == 24 ~ "2012-13",
             season_id == 23 ~ "2011-12",
             season_id == 22 ~ "2010-11",
             season_id == 21 ~ "2009-10",
             season_id == 41 ~ "2008-09",
             season_id == 40 ~ "2007-08",
             season_id == 39 ~ "2006-07",
             season_id == 38 ~ "2005-06",
             season_id == 37 ~ "2004-05"
             )) %>% 
  cleanlocations()


p <- ggplot() +
  geom_density2d_filled(data = messi_receipts, 
                        aes(pass.end_location.x, 80-pass.end_location.y,
                            group = season),
                        adjust = 4/6) + #bandwidth adj
  annotate_pitch(dimensions = pitch_statsbomb,  fill = NA) +
  theme_pitch() +
  labs(caption ="Data source: StatsBomb Open Data | Viz: @iecastro\n*Passes from corners, free kicks, or throw-ins not included") +
  theme(legend.position = "none") +
  scico::scale_fill_scico_d(palette = "batlowW", direction = -1)

p +  facet_wrap(~season) +
  direction_label(y_label = 70, text_size = 1,
                  x_label = 20,
                  colour = "black") +
  labs(subtitle = "Pitch location of completed passes received by Messi across LaLiga seasons:") 

#ggsave("plots/messi_receipts_static.png", plot = last_plot())

p + 
  direction_label(y_label = 70, text_size = 3,
                  x_label = 20,
                  colour = "black") +
  transition_states(season, transition_length = 2, 
                    state_length = 50) +
  labs(title = "Pitch location density of completed passes\nreceived by Messi during LaLiga season: {closest_state}") +
  theme(plot.title=element_text(hjust=0.5))
  
anim_save("plots/messi_animate.gif", animation = last_animation())

#save(animate,
#     file = "plots/2020-01-21-spotify/animate.RData")


messi_shots <- messi_events %>% 
  filter(type.name == "Shot" &
           shot.type.name != "Penalty") %>% 
  cleanlocations() %>% 
  mutate(season = 
           case_when(
             season_id == 90 ~ "2020-21",
             season_id == 42 ~ "2019-20",
             season_id == 4 ~ "2018-19",
             season_id == 1 ~ "2017-18",
             season_id == 2 ~ "2016-17",
             season_id == 27 ~ "2015-16"
           ),
         outcome = if_else(shot.outcome.name %in%
                             c("Goal", "Saved"),
                           shot.outcome.name,
                           "Missed"))



xg <- messi_shots %>% 
  group_by(season) %>% 
  summarise(shots = n(),
           total_xG = sum(shot.statsbomb_xg))

goals <- messi_shots %>% 
  filter(shot.outcome.name == "Goal") %>% 
  group_by(season) %>% 
  tally(name = "goals")

messi_shots %>% 
  filter(shot.outcome.name == "Goal" &
           shot.statsbomb_xg < .1) %>% 
  group_by(season) %>% 
  tally(name = "goals")


xg %>% inner_join(goals, by = "season")

to_opta <- rescale_coordinates(from = pitch_statsbomb, to = pitch_opta)

shots_rescaled <-
  data.frame(x = to_opta$x(messi_shots$location.x),
             y = to_opta$y(messi_shots$location.y),
             xend = to_opta$x(messi_shots$shot.end_location.x),
             yend = to_opta$y(messi_shots$shot.end_location.y),
             outcome = messi_shots$shot.outcome.name,
             xG = messi_shots$shot.statsbomb_xg,
             season = messi_shots$season)

ggplot() +
  annotate_pitch(fill = "steelblue4", 
                 colour = "white", goals = goals_line) +
  geom_point(data = shots_rescaled %>% 
               filter(outcome == "Goal"),
             alpha = .75,
             aes(x, 100-y,
                 color = xG < .1,
                 size = xG)) +
  theme_pitch() +
  coord_flip(xlim = c(60, 101)) +
  scale_y_reverse() +
  facet_wrap(~season) +
  theme(legend.position = "bottom",
        legend.key = element_rect(fill = NA)) +
  #scale_color_brewer(palette = "Dark1") +
  scale_color_manual(values = 
                       c("FALSE" = "deeppink",
                         "TRUE" = "yellow")) +
  guides(color = 
           guide_legend(
             override.aes = 
               list(size = 3.5, 
                    alpha = 1))
  ) +
  ggtitle("Messi's non-penalty La Liga goals")
  

ggplot() +
  annotate_pitch(fill = "steelblue4", 
                 colour = "white", goals = goals_line) +
  geom_point(data = shots_rescaled %>% 
               filter(outcome == "Goal"),
             alpha = .7,
             aes(x, 100-y,
                 color = xG < .1),
             size = 2.4
             ) +
  geom_segment(data = shots_rescaled %>% 
               filter(outcome == "Goal"),
             alpha = .6,
             aes(x = x, y = 100-y,
                 xend = xend,
                 yend = 100-yend,
                 color = xG < .1,
                 size = xG),
             lineend = "round", size = 0.5,
             show_guide = FALSE) +
  theme_pitch() +
  coord_flip(xlim = c(60, 101)) +
  scale_y_reverse() +
  facet_wrap(~season) +
  theme(legend.position = "top",
        legend.justification='left',
        legend.key = element_rect(fill = NA)) +
  scale_color_manual(values = 
                       c("FALSE" = "azure2",
                         "TRUE" = "yellow"),
                     labels = c( "xG: 0.1 or higher",
                                 "xG less than 0.1"),
                     name = NULL) +
  guides(color = 
           guide_legend(
             override.aes = 
               list(size = 3.5, 
                    alpha = 1))
  ) +
  ggtitle("Messi's non-penalty LaLiga goals",
          subtitle = "roughly 1/3 of goals were converted from low probability shots\n(as mesured by expected goals value, xG)") +
  labs(caption = "Source: StatsBomb Open Data")



goal_matchid <- messi_shots %>% 
  filter(outcome == "Goal") %>% 
  distinct(match_id) %>% 
  pull()


liga_matches %>% 
  filter(match_id %in% goal_matchid) %>% 
  group_by(home_team.home_team_name, away_team.away_team_name)



messi_shots %>% 
  filter(shot.statsbomb_xg < .1 &
           outcome == "Goal") %>% 
  inner_join(liga_matches %>% select(match_id, home_team.home_team_name),
             by = "match_id")  %>% 
  mutate(game = if_else(home_team.home_team_name == "Barcelona",
                        "Home", "Away")) %>% 
  ggplot() +
  annotate_pitch(fill = "springgreen4", dimensions = pitch_statsbomb,
                 colour = "white", goals = goals_line) +
  geom_point(alpha = .75,
             aes(location.x, location.y,
                 color = outcome
                 #size = shot.statsbomb_xg
                 )) +
  theme_pitch() +
  coord_flip(xlim = c(60, 120)) +
  #scale_y_reverse() +
  facet_wrap( ~ game) +
  theme(legend.position = "bottom",
        legend.key = element_rect(fill = NA)) +
  #scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values =
                       c("Goal" = "cadetblue1",
                         "Shot" = "blue2")) +
  guides(color = 
           guide_legend(
             override.aes = 
               list(size = 3.5, 
                    alpha = 1))
  ) +
  ggtitle("Messi's non-penalty La Liga goals")
