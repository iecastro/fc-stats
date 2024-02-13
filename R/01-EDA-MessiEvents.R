library(tidyverse)
library(ggsoccer)

source("R/00-setup.R")

meta_df <- FreeCompetitions() 

liga_matches <- meta_df %>% 
  filter(competition_id == 11 &
           season_id %in% c(90,42,4,1,2,27)
         ) %>% # seasons 15/16 to 20/21
  FreeMatches()

messi_events <- map_df(liga_matches$match_id, function(x){
  get.matchFree(liga_matches %>% filter(match_id == x)) %>% 
    as_tibble()  %>% 
    filter(player.name == "Lionel Andrés Messi Cuccittini")
}
) 


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
