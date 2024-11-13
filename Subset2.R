library(tidyverse)

setwd("~/GitHub/mlb-pitchers")

pitch_21 <- read.csv("pitches_21.csv", header = TRUE)

pitch_22 <- read.csv("pitches_22.csv", header = TRUE)

pitch_23 <- read.csv("pitches_23.csv", header = TRUE)


unique(pitch_23$pitchname_desc)

pitch_21 <- pitch_21 %>% mutate(pitchtype = case_when(pitchname_desc == "Four-seam FB" | pitchname_desc == "Cutter" | pitchname_desc == "Sinker" | pitchname_desc == "Fastball" | 
                                                        pitchname_desc == "Two-seam FB" | pitchname_desc == "Four-seam FB" ~ "Fastball",
                                                      pitchname_desc == "Knuckle Curve" | pitchname_desc == "Slow Curve" | pitchname_desc == "Sweeper" | pitchname_desc == "Slider" | pitchname_desc == "Slurve" |
                                                        pitchname_desc == "Curveball" ~ "Breaking Ball",
                                                      pitchname_desc == "Changeup" | pitchname_desc == "Splitter" | pitchname_desc == "Forkball" ~ "Offspeed",
                                                      pitchname_desc == NA | pitchname_desc == "Knuckleball" | pitchname_desc == "Euphus Pitch" ~ NA))

pitch_22 <- pitch_22 %>% mutate(pitchtype = case_when(pitchname_desc == "Four-seam FB" | pitchname_desc == "Cutter" | pitchname_desc == "Sinker" | pitchname_desc == "Fastball" | 
                                                        pitchname_desc == "Two-seam FB" | pitchname_desc == "Four-seam FB" ~ "Fastball",
                                                      pitchname_desc == "Knuckle Curve" | pitchname_desc == "Slow Curve" | pitchname_desc == "Sweeper" | pitchname_desc == "Slider" | pitchname_desc == "Slurve" |
                                                        pitchname_desc == "Curveball" ~ "Breaking Ball",
                                                      pitchname_desc == "Changeup" | pitchname_desc == "Splitter" | pitchname_desc == "Forkball" ~ "Offspeed",
                                                      pitchname_desc == NA | pitchname_desc == "Knuckleball" | pitchname_desc == "Euphus Pitch" ~ NA))

pitch_23 <- pitch_23 %>% mutate(pitchtype = case_when(pitchname_desc == "Four-seam FB" | pitchname_desc == "Cutter" | pitchname_desc == "Sinker" | pitchname_desc == "Fastball" | 
                                                        pitchname_desc == "Two-seam FB" | pitchname_desc == "Four-seam FB" ~ "Fastball",
                                                      pitchname_desc == "Knuckle Curve" | pitchname_desc == "Slow Curve" | pitchname_desc == "Sweeper" | pitchname_desc == "Slider" | pitchname_desc == "Slurve" |
                                                        pitchname_desc == "Curveball" ~ "Breaking Ball",
                                                      pitchname_desc == "Changeup" | pitchname_desc == "Splitter" | pitchname_desc == "Forkball" ~ "Offspeed",
                                                      pitchname_desc == NA | pitchname_desc == "Knuckleball" | pitchname_desc == "Euphus Pitch" ~ NA))


all <- rbind(pitch_21, pitch_22, pitch_23)


test <- all %>% group_by(pitcher) %>% select(pitcher, pitcherthrows, pitchtype) %>% distinct(pitchtype) 

test


test %>%
  filter(n() == 2) -> subset


subset <- subset[(subset$pitchtype == "Fastball" | subset$pitchtype == "Breaking Ball"),]

subset %>%
  filter(n() == 2) -> subset

print(subset, n = 424) # Full dataset

length(unique(subset$pitcher)) # 212 pitchers in the sample

subset %>% distinct(pitcher) %>% write.csv("subset.csv", row.names = FALSE)
