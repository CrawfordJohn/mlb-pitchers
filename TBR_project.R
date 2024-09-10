############# Exploratory Phase Rays Project ##################################
library(needs)

needs(dplyr, tidyverse)

setwd("C:/Users/dfack/OneDrive/Documents/Rays Project")



pitch_23 <- read.csv("pitches_23.csv", header = TRUE)



test <- pitch_23 %>% group_by(pitcher) %>% select(pitcher, pitchname_desc) %>% distinct(pitchname_desc) 

test %>%
  filter(n() == 2) -> subset


print(subset, n = 110)

subset <- subset[-c(1,2,16,17,27,30,36,37,39:41,44:48,53:56,62:71,74,81:86,88:93,
                    98:100,102,103,105,108:110),]


print(subset, n = 58)

length(unique(subset$pitcher)) # 29 pitchers in the sample

################################################################################


















































































######################## Scratch Work from here down ##########################


###############################################################################

#pitch_21 <- read.csv("pitches_21.csv", header = TRUE)

#pitch_22 <- read.csv("pitches_22.csv", header = TRUE)


fb_all <- pitch_23 %>% filter(pitchname_desc == 'Four-seam FB' | pitchname_desc ==
                                 'Fastball' | pitchname_desc == 'Two-seam FB' | 
                                 pitchname_desc == 'Sinker')

plot_fb <- pitch_23 %>% filter(pitchname_desc == 'Four-seam FB' | pitchname_desc ==
  'Fastball') %>%
  ggplot(aes(relspeed, inducedvertbreak, color=pitchname_desc)) +
  geom_point() +
  theme_bw()

fb_all %>% filter(relspeed >= 97.5 & inducedvertbreak >= 25) %>% select(pitcher, pitchname_desc, relspeed, inducedvertbreak)



length(unique(pitch_23$pitcher))

unique(pitch_23$pitchname_desc)


colMeans(pitch_23 %>% filter(pitchname_desc == 'Four-seam FB') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak))



cbind(c(unique(pitch_23$pitchname_desc)[-17]))



fb_4seam <- pitch_23 %>% filter(pitchname_desc == 'Four-seam FB') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
cut <- pitch_23 %>% filter(pitchname_desc == 'Cutter') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
change <- pitch_23 %>% filter(pitchname_desc == 'Changeup') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
slider <- pitch_23 %>% filter(pitchname_desc == 'Slider') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
curve <- pitch_23 %>% filter(pitchname_desc == 'Curveball') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
sink <- pitch_23 %>% filter(pitchname_desc == 'Sinker') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
KC <- pitch_23 %>% filter(pitchname_desc == 'Knuckle Curve') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
Sweep <- pitch_23 %>% filter(pitchname_desc == 'Sweeper') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
Split <- pitch_23 %>% filter(pitchname_desc == 'Splitter') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
Forkball <- pitch_23 %>% filter(pitchname_desc == 'Forkball') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
fb <- pitch_23 %>% filter(pitchname_desc == 'Fastball') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
slow_curve <- pitch_23 %>% filter(pitchname_desc == 'Slow Curve') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
slurve <- pitch_23 %>% filter(pitchname_desc == 'Slurve') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)
fb_2seam <- pitch_23 %>% filter(pitchname_desc == 'Two-seam FB') %>% select(spinrate, relspeed, horzbreak, inducedvertbreak)

na.omit(fb_4seam) ; na.omit(cut) ; na.omit(change) ; na.omit(slider) ; na.omit(curve) 
na.omit(sink) ; na.omit(KC) ; na.omit(Sweep) ; na.omit(Split) ; na.omit(Forkball) 
na.omit(fb) ; na.omit(slow_curve) ; na.omit(slurve) ; na.omit(fb_2seam)



rbind(colMeans(fb_4seam), colMeans(cut), colMeans(change), colMeans(slider), colMeans(curve), 
      colMeans(sink), colMeans(KC), colMeans(Sweep), colMeans(Split), colMeans(Forkball), 
      colMeans(fb), colMeans(slow_curve), colMeans(slurve), colMeans(fb_2seam))



################################################################################
pit_names <- unique(all_pitch$pitcher)
pit_type  <- unique(all_pitch$pitchname_desc)[-15]

n.pit_names <- length(pit_names)
n.pit_type <- length(pit_type)


pit_mat <- matrix(nrow = 23409, ncol = 6, dimnames = list(c(1:23409), c("Name", "Pitch_Type", 
                                          "SpinRate", "Velo", "IndVertBreak", "HorzBreak")))


pit_avg <- function(i) {
  
  pitch_sum_1 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[1])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[1], round(colMeans(pitch_sum_1[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_2 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[2])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[2], round(colMeans(pitch_sum_2[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_3 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[3])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[3], round(colMeans(pitch_sum_3[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_4 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[4])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[4], round(colMeans(pitch_sum_4[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_5 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[5])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[5], round(colMeans(pitch_sum_5[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_6 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[6])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[6], round(colMeans(pitch_sum_6[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_7 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[7])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[7], round(colMeans(pitch_sum_7[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_8 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[8])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[8], round(colMeans(pitch_sum_8[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_9 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[9])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[9], round(colMeans(pitch_sum_9[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_10 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                         pit_type[10])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[10], round(colMeans(pitch_sum_10[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_11 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                         pit_type[11])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[11], round(colMeans(pitch_sum_11[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_12 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                         pit_type[12])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[12], round(colMeans(pitch_sum_12[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_13 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                         pit_type[13])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[13], round(colMeans(pitch_sum_13[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_14 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                         pit_type[14])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[14], round(colMeans(pitch_sum_14[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_15 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                         pit_type[15])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[15], round(colMeans(pitch_sum_15[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_16 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                         pit_type[16])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[16], round(colMeans(pitch_sum_16[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_17 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                         pit_type[17])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[17], round(colMeans(pitch_sum_17[,3:6]), 2))
  
  x <- x + 1
  
  i <- i + 1
}
  
  










i <- 1
for (x in 1:n.pit_names) {
  for (y in 1:n.pit_type) {
    pitch_sum <- all_pitch %>% filter(pitcher == pit_names[x], pitchname_desc == 
                              pit_type[y])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
    
    pit_mat[i, 1:6] <- c(pit_names[x], pit_type[y], round(colMeans(pitch_sum[,3:6]), 2))
    i <- i + 1
    
  }
}

pit_mat <- matrix(nrow = 23409, ncol = 6, dimnames = list(c(1:23409), c("Name", "Pitch_Type", 
                                                                        "SpinRate", "Velo", "IndVertBreak", "HorzBreak")))

i <- 1
x <- 1
while (i <= 1377) {
  pitch_sum_1 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                          pit_type[1])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[1], round(colMeans(pitch_sum_1[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_2 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[2])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[2], round(colMeans(pitch_sum_2[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_3 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[3])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[3], round(colMeans(pitch_sum_3[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_4 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[4])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[4], round(colMeans(pitch_sum_4[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_5 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[5])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[5], round(colMeans(pitch_sum_5[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_6 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[6])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[6], round(colMeans(pitch_sum_6[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_7 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[7])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[7], round(colMeans(pitch_sum_7[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_8 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[8])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[8], round(colMeans(pitch_sum_8[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_9 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[9])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[9], round(colMeans(pitch_sum_9[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_10 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[10])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[10], round(colMeans(pitch_sum_10[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_11 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[11])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[11], round(colMeans(pitch_sum_11[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_12 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[12])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[12], round(colMeans(pitch_sum_12[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_13 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[13])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[13], round(colMeans(pitch_sum_13[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_14 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[14])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[14], round(colMeans(pitch_sum_14[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_15 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[15])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[15], round(colMeans(pitch_sum_15[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_16 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[16])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[16], round(colMeans(pitch_sum_16[,3:6]), 2))
  
  x <- x + 1
  
  pitch_sum_17 <- all_pitch %>% filter(pitcher == pit_names[i], pitchname_desc == 
                                        pit_type[17])%>% select(pitcher, pitchname_desc, spinrate, relspeed, horzbreak, inducedvertbreak)
  
  pit_mat[x, 1:6] <- c(pit_names[i], pit_type[17], round(colMeans(pitch_sum_17[,3:6]), 2))
  
  x <- x + 1
  
  i <- i + 1
  

}
