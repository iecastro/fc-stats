library(ggsoccer)
library(patchwork)

passes <- pedri %>%
  #messi %>% 
  filter(type.name =="Pass" & is.na(pass.outcome.name)) %>% #1
  cleanlocations() %>% 
  # filter(pass.end_location.x>=102 & pass.end_location.y<=62 &
  #          pass.end_location.y>=18) #2
  mutate(footing = 
           if_else(pass.body_part.name %in% 
                     c("Left Foot", "Right Foot"),
                   pass.body_part.name, "Other"))


# create_Pitch(goaltype = "barcanumbers", 
#              grass_colour = "#538032",
#              line_colour =  "#ffffff",
#              background_colour = "#538032") +
#   geom_segment(data = passes,
#                alpha = .6,
#                aes(x = location.x, y = location.y,
#                    xend = pass.end_location.x, 
#                    yend = pass.end_location.y),
#                lineend = "round", size = 0.5, 
#                colour = "#000000", 
#                arrow =
#                  arrow(length = unit(0.07, "inches"),
#                        ends = "last", type = "open")) + #3
#   
#   labs(title = "Pedri, Completed Passes", 
#   subtitle = "La Liga, 2020-21") + #4
#   scale_y_reverse() + #5
#   coord_fixed(ratio = 105/100) #6


select_passes <- passes %>% 
  select(starts_with("pass"),
         under_pressure,
         location.x, location.y) %>% 
  filter(pass.recipient.name %in%
           c("Jordi Alba Ramos",
             "Lionel Andrés Messi Cuccittini",
             "Frenkie de Jong", "Sergio Busquets i Burgos",
             "Clément Lenglet", "Marc-André ter Stegen"))


ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_segment(data = select_passes,
               alpha = .6,
               aes(x = location.x, y = location.y,
                   xend = pass.end_location.x,
                   yend = pass.end_location.y,
                   color = pass.height.name),
               lineend = "round", size = 0.5,
               #colour = "#000000",
               arrow =
                 arrow(length = unit(0.07, "inches"),
                       ends = "last", type = "open")) +
  scale_y_reverse() + 
  coord_fixed(ratio = 105/100) +
  facet_wrap(~pass.recipient.name) +
  theme_pitch() +
  theme(legend.position = "top") +
  direction_label(y_label = -10) +
  scale_color_brewer(palette = "Set1")


## pressing
press <- bind_rows(messi, pedri, dest) %>% 
  filter(type.name == "Pressure") %>% 
  cleanlocations()


pr1 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_point(data = press, 
             aes(location.x, 80-location.y),
             alpha = .4, color = "purple") +
  #scale_y_reverse() + 
  #coord_fixed(ratio = 105/100) +
  facet_wrap(~name_short) +
  theme_pitch() +
  theme(legend.position = "top") +
  direction_label(y_label = -3.2, text_size = 2,
                  x_label = 60,
                  colour = "black")

pr2 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_density2d_filled(data = press, 
                        aes(location.x, 80-location.y),
                        alpha = .7) +
  facet_wrap(~name_short) +
  theme_pitch() +
  labs(caption ="Data source: StatsBomb Open Date") +
  theme(legend.position = "none",
        plot.subtitle=element_text(hjust=0.5)) +
  # direction_label(y_label = -2.3, text_size = 2,
  #                 x_label = 60,
  #                 colour = "black") +
  scale_fill_viridis_d(option = "A", direction = -1) 
  
pr1 / pr2 +
  plot_annotation(title = "Barcelona, 2020/21 LaLiga season",
                  subtitle = "Data represent the pitch location of pressure events on opposing players recieving, carrying, or releasing the ball.")


## counterpress all types

cpress <- bind_rows(messi, pedri, dest) %>% 
  filter(counterpress == TRUE) %>% 
  cleanlocations()


cpress %>% group_by(type.name, counterpress) %>% tally()


cpr1 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_point(data = cpress %>% 
               filter(counterpress == TRUE), 
             aes(location.x, 80-location.y),
             alpha = .4, color = "purple") +
  #scale_y_reverse() + 
  #coord_fixed(ratio = 105/100) +
  facet_wrap(~name_short) +
  theme_pitch() +
  theme(legend.position = "top") +
  direction_label(y_label = -3.2, text_size = 2,
                  x_label = 60,
                  colour = "black")

cpr2 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_density2d_filled(data = cpress %>% 
                          filter(counterpress == TRUE), 
                        aes(location.x, 80-location.y),
                        alpha = .7) +
  facet_wrap(~name_short) +
  theme_pitch() +
  labs(caption ="Data source: StatsBomb Open Date") +
  theme(legend.position = "none",
        plot.subtitle=element_text(hjust=0.5)) +
  # direction_label(y_label = -2.3, text_size = 2,
  #                 x_label = 60,
  #                 colour = "black") +
  scale_fill_viridis_d(option = "A", direction = -1) 

cpr1 / cpr2 +
  plot_annotation(title = "Barcelona, 2020/21 LaLiga season",
                  subtitle = "Data represent the pitch location of counter press events on opposing players recieving, carrying, or releasing the ball.")


press %>% group_by(name_short) %>% 
  summarise(gp = n_distinct(match_id), 
            pos = n_distinct(position.name))

 ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_density2d_filled(data = press, 
                        aes(location.x, 80-location.y),
                        alpha = .8,
                        adjust = 3/3) +
  facet_wrap(~name_short) +
  theme_pitch() +
  labs(caption ="Data source: StatsBomb Open Date") +
  theme(legend.position = "none",
        plot.subtitle=element_text(hjust=0.3)) +
  # direction_label(y_label = -2.3, text_size = 2,
  #                 x_label = 60,
  #                 colour = "black") +
  scale_fill_viridis_d(option = "A", direction = -1) 
 



create_Pitch(goaltype = "barcanumbers", JdeP = TRUE,
             line_colour = "black") +
  geom_density2d_filled(data = press %>% 
                          filter(name_short == "Messi"), 
                        aes(location.x, location.y),
                        alpha = .7,
                        adjust = 4/5) +
  scale_y_reverse() +
  labs(caption ="Data source: StatsBomb Open Date") +
  theme(legend.position = "none",
        plot.subtitle=element_text(hjust=0.3)) +
  scale_fill_viridis_d(option = "A", direction = -1) 


## pedri on pitch
pedri <- 
  barca_events %>% 
  filter(player.name == "Pedro González López") %>% 
  cleanlocations()

create_Pitch(goaltype = "barcanumbers", JdeP = TRUE,
             line_colour = "black") +
  geom_density2d_filled(data = pedri %>% filter(type.name == "Foul Committed"), 
                        aes(location.x, location.y),
                        alpha = .7,
                        adjust = 4/5) +
  scale_y_reverse() +
  coord_fixed(ratio = 105/100) +
  labs(caption ="Data source: StatsBomb Open Date") +
  theme(legend.position = "none",
        plot.subtitle=element_text(hjust=0.3)) +
  scale_fill_viridis_d(option = "A", direction = -1) 

## nutmegs
nmg <- barca_events %>% 
  filter(dribble.nutmeg == TRUE) %>% 
  group_by(player.name) %>% 
  mutate(nutmeg_tally = n()) %>% 
  filter(nutmeg_tally >=3) %>% 
  ungroup() %>% 
  cleanlocations()


ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_point(data = nmg, 
             aes(location.x, 80-location.y,
                 color = dribble.outcome.name),
             alpha = .6, size = 3) +
  #scale_y_reverse() + 
 # coord_fixed(ratio = 105/100) +
  facet_wrap(~player.name) +
  theme_pitch() +
  theme(legend.position = "top") +
  direction_label(y_label = -10) +
  scale_color_brewer(palette = "Set1")


pedri_shots <- pedri %>% 
  filter(type.name == "Shot") %>% 
  cleanlocations() 


ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_segment(data = pedri_shots,
               alpha = .8,
               aes(x = location.x, y = location.y,
                   xend = shot.end_location.x,
                   yend = shot.end_location.y),
               lineend = "round", size = 0.5,
               colour = "DarkBlue",
               arrow =
                 arrow(length = unit(0.07, "inches"),
                       ends = "last", type = "open")) +
  scale_y_reverse() + 
  coord_fixed(ratio = 105/100) +
  facet_wrap(~play_pattern.name) +
  theme_pitch() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set1")

ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_point(data = pedri_shots %>% 
               mutate(outcome = 
                        if_else(shot.outcome.name %in%
                                  c("Goal", "Saved"),
                                shot.outcome.name,
                                "Missed")),
               aes(x = location.x, y = location.y,
                   color = outcome,
                   size = shot.statsbomb_xg),
               lineend = "round",
               #colour = "tomato",
               arrow =
                 arrow(length = unit(0.07, "inches"),
                       ends = "last", type = "open")) +
  coord_flip(xlim = c(60, 120)) +
  #scale_y_reverse() + 
  #coord_fixed(ratio = 105/100) +
  #facet_wrap(~play_pattern.name) +
  theme_pitch() +
  theme(legend.position = "top")


 messi_shots <- messi %>% 
  filter(type.name == "Shot") %>% 
  cleanlocations() 
 
 
 ggplot() +
   annotate_pitch(dimensions = pitch_statsbomb) +
   geom_point(data = messi_shots %>% 
                mutate(outcome = 
                         if_else(shot.outcome.name %in%
                                   c("Goal", "Saved"),
                                 shot.outcome.name,
                                 "Missed")),
              alpha = .6,
              aes(x = location.x, y = location.y,
                  color = outcome,
                  size = shot.statsbomb_xg)) +
   coord_flip(xlim = c(60, 120)) +
   facet_wrap( ~ shot.type.name) +
   theme_pitch() +
   theme(legend.position = "top") +
   scale_color_manual(
     values = c("Goal" = "DarkBlue",
                "Missed" = "Gray80",
                "Saved" = "Firebrick")
   )
 
 
 barca_shots <- barca_events %>% 
   filter(type.name == "Shot" & team.name == "Barcelona") %>% 
   cleanlocations()
 
 
 ggplot() +
   annotate_pitch(dimensions = pitch_statsbomb) +
   geom_point(data = barca_shots %>% 
                filter(shot.outcome.name == "Goal"),
              alpha = .4,
              aes(x = location.x, y = location.y,
                  color = shot.statsbomb_xg,
                  size = shot.statsbomb_xg),
              #colour = "DarkBlue"
              ) +
   coord_flip(xlim = c(60, 120)) +
   #facet_wrap( ~ shot.type.name) +
   #facet_wrap( ~ player.name) +
   facet_wrap( ~ play_pattern.name) +
   theme_pitch() +
   theme(legend.position = "top")
 
 barca_shots %>% filter( shot.outcome.name == "Goal") %>% distinct(position.name) %>%  arrange(position.name) %>% print(n=Inf)
   