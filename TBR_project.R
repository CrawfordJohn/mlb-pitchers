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


# New Subset csv file

sub <- read.csv('pitcher_subset.csv', header = TRUE)


# All situations 


count.00 <- sub %>% filter(balls == 0 & strikes == 0) 
count.01 <- sub %>% filter(balls == 0 & strikes == 1)
count.02 <- sub %>% filter(balls == 0 & strikes == 2)
count.10 <- sub %>% filter(balls == 1 & strikes == 0)
count.11 <- sub %>% filter(balls == 1 & strikes == 1)
count.12 <- sub %>% filter(balls == 1 & strikes == 2)
count.20 <- sub %>% filter(balls == 2 & strikes == 0)
count.21 <- sub %>% filter(balls == 2 & strikes == 1)
count.22 <- sub %>% filter(balls == 2 & strikes == 2)
count.30 <- sub %>% filter(balls == 3 & strikes == 0)
count.31 <- sub %>% filter(balls == 3 & strikes == 1)
count.32 <- sub %>% filter(balls == 3 & strikes == 2)


unique(sub$pitchname_desc)

# Total Num Fb in each count
num.fb.00 <- dim(count.00[(count.00$pitchname_desc == 'Four-seam FB' | 
           count.00$pitchname_desc == "Sinker"),])[1]

num.fb.01 <- dim(count.01[(count.01$pitchname_desc == 'Four-seam FB' | 
                             count.01$pitchname_desc == "Sinker"),])[1]

num.fb.02 <- dim(count.02[(count.02$pitchname_desc == 'Four-seam FB' | 
                             count.02$pitchname_desc == "Sinker"), ])[1]

num.fb.10 <- dim(count.10[(count.10$pitchname_desc == 'Four-seam FB' | 
                             count.10$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.11 <- dim(count.11[(count.11$pitchname_desc == 'Four-seam FB' | 
                             count.11$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.12 <- dim(count.12[(count.12$pitchname_desc == 'Four-seam FB' | 
                             count.12$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.20 <- dim(count.20[(count.20$pitchname_desc == 'Four-seam FB' | 
                             count.20$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.21 <- dim(count.21[(count.21$pitchname_desc == 'Four-seam FB' | 
                             count.21$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.22 <- dim(count.22[(count.22$pitchname_desc == 'Four-seam FB' | 
                             count.22$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.30 <- dim(count.30[(count.30$pitchname_desc == 'Four-seam FB' | 
                             count.30$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.31 <- dim(count.31[(count.31$pitchname_desc == 'Four-seam FB' | 
                             count.31$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.32 <- dim(count.32[(count.32$pitchname_desc == 'Four-seam FB' | 
                             count.32$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]


# Pct FB and Pct BB as a vector in each count
pct.00 <- c((num.fb.00 / dim(count.00)[1]), ((dim(count.00)[1] - num.fb.00) / 
                                               dim(count.00)[1]))

pct.01 <- c((num.fb.01 / dim(count.01)[1]), ((dim(count.01)[1] - num.fb.01) / 
                                               dim(count.01)[1]))

pct.02 <- c((num.fb.02 / dim(count.02)[1]), ((dim(count.02)[1] - num.fb.02) / 
                                               dim(count.02)[1]))

pct.10 <- c((num.fb.10 / dim(count.10)[1]), ((dim(count.10)[1] - num.fb.10) / 
                                               dim(count.10)[1]))

pct.11 <- c((num.fb.11 / dim(count.11)[1]), ((dim(count.11)[1] - num.fb.11) / 
                                               dim(count.11)[1]))

pct.12 <- c((num.fb.12 / dim(count.12)[1]), ((dim(count.12)[1] - num.fb.12) / 
                                               dim(count.12)[1]))

pct.20 <- c((num.fb.20 / dim(count.20)[1]), ((dim(count.20)[1] - num.fb.20) / 
                                               dim(count.20)[1]))

pct.21 <- c((num.fb.21 / dim(count.21)[1]), ((dim(count.21)[1] - num.fb.21) / 
                                               dim(count.21)[1]))

pct.22 <- c((num.fb.22 / dim(count.22)[1]), ((dim(count.22)[1] - num.fb.22) / 
                                               dim(count.22)[1]))

pct.30 <- c((num.fb.30 / dim(count.30)[1]), ((dim(count.30)[1] - num.fb.30) / 
                                               dim(count.30)[1]))

pct.31 <- c((num.fb.31 / dim(count.31)[1]), ((dim(count.31)[1] - num.fb.31) / 
                                               dim(count.31)[1]))

pct.32 <- c((num.fb.32 / dim(count.32)[1]), ((dim(count.32)[1] - num.fb.32) / 
                                               dim(count.32)[1]))


tot.pct <- rbind(pct.00, pct.01, pct.02, pct.10, pct.11, pct.12, pct.20, pct.21, pct.22, 
              pct.30, pct.31, pct.32)

rownames(tot.pct) <- c('0-0', '0-1', '0-2', '1-0', '1-1', '1-2', '2-0', '2-1', '2-2',
                       '3-0', '3-1', '3-2')

colnames(tot.pct) <- c("FB.pct", "Break.pct")

round(tot.pct, 3)
####################################################


# 0 outs

# All counts
count.00_zero <- sub %>% filter(balls == 0 & strikes == 0 & outs == 0) 
count.01_zero <- sub %>% filter(balls == 0 & strikes == 1 & outs == 0)
count.02_zero <- sub %>% filter(balls == 0 & strikes == 2 & outs == 0)
count.10_zero <- sub %>% filter(balls == 1 & strikes == 0 & outs == 0)
count.11_zero <- sub %>% filter(balls == 1 & strikes == 1 & outs == 0)
count.12_zero <- sub %>% filter(balls == 1 & strikes == 2 & outs == 0)
count.20_zero <- sub %>% filter(balls == 2 & strikes == 0 & outs == 0)
count.21_zero <- sub %>% filter(balls == 2 & strikes == 1 & outs == 0)
count.22_zero <- sub %>% filter(balls == 2 & strikes == 2 & outs == 0)
count.30_zero <- sub %>% filter(balls == 3 & strikes == 0 & outs == 0)
count.31_zero <- sub %>% filter(balls == 3 & strikes == 1 & outs == 0)
count.32_zero <- sub %>% filter(balls == 3 & strikes == 2 & outs == 0)



# Total Num Fb in each count
num.fb.00_zero <- dim(count.00_zero[(count.00_zero$pitchname_desc == 'Four-seam FB' | 
                                       count.00_zero$pitchname_desc == "Sinker"),])[1]

num.fb.01_zero <- dim(count.01_zero[(count.01_zero$pitchname_desc == 'Four-seam FB' | 
                                       count.01_zero$pitchname_desc == "Sinker"),])[1]

num.fb.02_zero <- dim(count.02_zero[(count.02_zero$pitchname_desc == 'Four-seam FB' | 
                                       count.02_zero$pitchname_desc == "Sinker"), ])[1]

num.fb.10_zero <- dim(count.10_zero[(count.10_zero$pitchname_desc == 'Four-seam FB' | 
                                       count.10_zero$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.11_zero <- dim(count.11_zero[(count.11_zero$pitchname_desc == 'Four-seam FB' | 
                                       count.11_zero$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.12_zero <- dim(count.12_zero[(count.12_zero$pitchname_desc == 'Four-seam FB' | 
                                       count.12_zero$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.20_zero <- dim(count.20_zero[(count.20_zero$pitchname_desc == 'Four-seam FB' | 
                                       count.20_zero$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.21_zero <- dim(count.21_zero[(count.21_zero$pitchname_desc == 'Four-seam FB' | 
                                       count.21_zero$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.22_zero <- dim(count.22_zero[(count.22_zero$pitchname_desc == 'Four-seam FB' | 
                                       count.22_zero$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.30_zero <- dim(count.30_zero[(count.30_zero$pitchname_desc == 'Four-seam FB' | 
                                       count.30_zero$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.31_zero <- dim(count.31_zero[(count.31_zero$pitchname_desc == 'Four-seam FB' | 
                                       count.31_zero$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.32_zero <- dim(count.32_zero[(count.32_zero$pitchname_desc == 'Four-seam FB' | 
                                       count.32_zero$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]


# Pct FB and Pct BB as a vector in each count
pct.00_zero <- c((num.fb.00_zero / dim(count.00_zero)[1]), ((dim(count.00_zero)[1] - num.fb.00_zero) / 
                                                              dim(count.00_zero)[1]))

pct.01_zero <- c((num.fb.01_zero / dim(count.01_zero)[1]), ((dim(count.01_zero)[1] - num.fb.01_zero) / 
                                                              dim(count.01_zero)[1]))

pct.02_zero <- c((num.fb.02_zero / dim(count.02_zero)[1]), ((dim(count.02_zero)[1] - num.fb.02_zero) / 
                                                              dim(count.02_zero)[1]))

pct.10_zero <- c((num.fb.10_zero / dim(count.10_zero)[1]), ((dim(count.10_zero)[1] - num.fb.10_zero) / 
                                                              dim(count.10_zero)[1]))

pct.11_zero <- c((num.fb.11_zero / dim(count.11_zero)[1]), ((dim(count.11_zero)[1] - num.fb.11_zero) / 
                                                              dim(count.11_zero)[1]))

pct.12_zero <- c((num.fb.12_zero / dim(count.12_zero)[1]), ((dim(count.12_zero)[1] - num.fb.12_zero) / 
                                                              dim(count.12_zero)[1]))

pct.20_zero <- c((num.fb.20_zero / dim(count.20_zero)[1]), ((dim(count.20_zero)[1] - num.fb.20_zero) / 
                                                              dim(count.20_zero)[1]))

pct.21_zero <- c((num.fb.21_zero / dim(count.21_zero)[1]), ((dim(count.21_zero)[1] - num.fb.21_zero) / 
                                                              dim(count.21_zero)[1]))

pct.22_zero <- c((num.fb.22_zero / dim(count.22_zero)[1]), ((dim(count.22_zero)[1] - num.fb.22_zero) / 
                                                              dim(count.22_zero)[1]))

pct.30_zero <- c((num.fb.30_zero / dim(count.30_zero)[1]), ((dim(count.30_zero)[1] - num.fb.30_zero) / 
                                                              dim(count.30_zero)[1]))

pct.31_zero <- c((num.fb.31_zero / dim(count.31_zero)[1]), ((dim(count.31_zero)[1] - num.fb.31_zero) / 
                                                              dim(count.31_zero)[1]))

pct.32_zero <- c((num.fb.32_zero / dim(count.32_zero)[1]), ((dim(count.32_zero)[1] - num.fb.32_zero) / 
                                                              dim(count.32_zero)[1]))

# Full Table
tot.pct_zero <- rbind(pct.00_zero, pct.01_zero, pct.02_zero, pct.10_zero, pct.11_zero, pct.12_zero, pct.20_zero, pct.21_zero, pct.22_zero, 
                      pct.30_zero, pct.31_zero, pct.32_zero)

rownames(tot.pct_zero) <- c('0-0', '0-1', '0-2', '1-0', '1-1', '1-2', '2-0', '2-1', '2-2',
                            '3-0', '3-1', '3-2')

colnames(tot.pct_zero) <- c("FB.pct", "Break.pct")

round(tot.pct_zero, 3)

########################################

# 1 out

# All Counts
count.00_one <- sub %>% filter(balls == 0 & strikes == 0 & outs == 1) 
count.01_one <- sub %>% filter(balls == 0 & strikes == 1 & outs == 1)
count.02_one <- sub %>% filter(balls == 0 & strikes == 2 & outs == 1)
count.10_one <- sub %>% filter(balls == 1 & strikes == 0 & outs == 1)
count.11_one <- sub %>% filter(balls == 1 & strikes == 1 & outs == 1)
count.12_one <- sub %>% filter(balls == 1 & strikes == 2 & outs == 1)
count.20_one <- sub %>% filter(balls == 2 & strikes == 0 & outs == 1)
count.21_one <- sub %>% filter(balls == 2 & strikes == 1 & outs == 1)
count.22_one <- sub %>% filter(balls == 2 & strikes == 2 & outs == 1)
count.30_one <- sub %>% filter(balls == 3 & strikes == 0 & outs == 1)
count.31_one <- sub %>% filter(balls == 3 & strikes == 1 & outs == 1)
count.32_one <- sub %>% filter(balls == 3 & strikes == 2 & outs == 1)



# Total Num Fb in each count
num.fb.00_one <- dim(count.00_one[(count.00_one$pitchname_desc == 'Four-seam FB' | 
                                     count.00_one$pitchname_desc == "Sinker"),])[1]

num.fb.01_one <- dim(count.01_one[(count.01_one$pitchname_desc == 'Four-seam FB' | 
                                     count.01_one$pitchname_desc == "Sinker"),])[1]

num.fb.02_one <- dim(count.02_one[(count.02_one$pitchname_desc == 'Four-seam FB' | 
                                     count.02_one$pitchname_desc == "Sinker"), ])[1]

num.fb.10_one <- dim(count.10_one[(count.10_one$pitchname_desc == 'Four-seam FB' | 
                                     count.10_one$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.11_one <- dim(count.11_one[(count.11_one$pitchname_desc == 'Four-seam FB' | 
                                     count.11_one$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.12_one <- dim(count.12_one[(count.12_one$pitchname_desc == 'Four-seam FB' | 
                                     count.12_one$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.20_one <- dim(count.20_one[(count.20_one$pitchname_desc == 'Four-seam FB' | 
                                     count.20_one$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.21_one <- dim(count.21_one[(count.21_one$pitchname_desc == 'Four-seam FB' | 
                                     count.21_one$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.22_one <- dim(count.22_one[(count.22_one$pitchname_desc == 'Four-seam FB' | 
                                     count.22_one$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.30_one <- dim(count.30_one[(count.30_one$pitchname_desc == 'Four-seam FB' | 
                                     count.30_one$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.31_one <- dim(count.31_one[(count.31_one$pitchname_desc == 'Four-seam FB' | 
                                     count.31_one$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.32_one <- dim(count.32_one[(count.32_one$pitchname_desc == 'Four-seam FB' | 
                                     count.32_one$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]


# Pct FB and Pct BB as a vector in each count
pct.00_one <- c((num.fb.00_one / dim(count.00_one)[1]), ((dim(count.00_one)[1] - num.fb.00_one) / 
                                                           dim(count.00_one)[1]))

pct.01_one <- c((num.fb.01_one / dim(count.01_one)[1]), ((dim(count.01_one)[1] - num.fb.01_one) / 
                                                           dim(count.01_one)[1]))

pct.02_one <- c((num.fb.02_one / dim(count.02_one)[1]), ((dim(count.02_one)[1] - num.fb.02_one) / 
                                                           dim(count.02_one)[1]))

pct.10_one <- c((num.fb.10_one / dim(count.10_one)[1]), ((dim(count.10_one)[1] - num.fb.10_one) / 
                                                           dim(count.10_one)[1]))

pct.11_one <- c((num.fb.11_one / dim(count.11_one)[1]), ((dim(count.11_one)[1] - num.fb.11_one) / 
                                                           dim(count.11_one)[1]))

pct.12_one <- c((num.fb.12_one / dim(count.12_one)[1]), ((dim(count.12_one)[1] - num.fb.12_one) / 
                                                           dim(count.12_one)[1]))

pct.20_one <- c((num.fb.20_one / dim(count.20_one)[1]), ((dim(count.20_one)[1] - num.fb.20_one) / 
                                                           dim(count.20_one)[1]))

pct.21_one <- c((num.fb.21_one / dim(count.21_one)[1]), ((dim(count.21_one)[1] - num.fb.21_one) / 
                                                           dim(count.21_one)[1]))

pct.22_one <- c((num.fb.22_one / dim(count.22_one)[1]), ((dim(count.22_one)[1] - num.fb.22_one) / 
                                                           dim(count.22_one)[1]))

pct.30_one <- c((num.fb.30_one / dim(count.30_one)[1]), ((dim(count.30_one)[1] - num.fb.30_one) / 
                                                           dim(count.30_one)[1]))

pct.31_one <- c((num.fb.31_one / dim(count.31_one)[1]), ((dim(count.31_one)[1] - num.fb.31_one) / 
                                                           dim(count.31_one)[1]))

pct.32_one <- c((num.fb.32_one / dim(count.32_one)[1]), ((dim(count.32_one)[1] - num.fb.32_one) / 
                                                           dim(count.32_one)[1]))

# Full table
tot.pct_one <- rbind(pct.00_one, pct.01_one, pct.02_one, pct.10_one, pct.11_one, pct.12_one, pct.20_one, pct.21_one, pct.22_one, 
                     pct.30_one, pct.31_one, pct.32_one)

rownames(tot.pct_one) <- c('0-0', '0-1', '0-2', '1-0', '1-1', '1-2', '2-0', '2-1', '2-2',
                           '3-0', '3-1', '3-2')

colnames(tot.pct_one) <- c("FB.pct", "Break.pct")

round(tot.pct_one, 3)

################################

# Two outs

# All Counts
count.00_two <- sub %>% filter(balls == 0 & strikes == 0 & outs == 2) 
count.01_two <- sub %>% filter(balls == 0 & strikes == 1 & outs == 2)
count.02_two <- sub %>% filter(balls == 0 & strikes == 2 & outs == 2)
count.10_two <- sub %>% filter(balls == 1 & strikes == 0 & outs == 2)
count.11_two <- sub %>% filter(balls == 1 & strikes == 1 & outs == 2)
count.12_two <- sub %>% filter(balls == 1 & strikes == 2 & outs == 2)
count.20_two <- sub %>% filter(balls == 2 & strikes == 0 & outs == 2)
count.21_two <- sub %>% filter(balls == 2 & strikes == 1 & outs == 2)
count.22_two <- sub %>% filter(balls == 2 & strikes == 2 & outs == 2)
count.30_two <- sub %>% filter(balls == 3 & strikes == 0 & outs == 2)
count.31_two <- sub %>% filter(balls == 3 & strikes == 1 & outs == 2)
count.32_two <- sub %>% filter(balls == 3 & strikes == 2 & outs == 2)



# Total Num Fb in each count
num.fb.00_two <- dim(count.00_two[(count.00_two$pitchname_desc == 'Four-seam FB' | 
                                     count.00_two$pitchname_desc == "Sinker"),])[1]

num.fb.01_two <- dim(count.01_two[(count.01_two$pitchname_desc == 'Four-seam FB' | 
                                     count.01_two$pitchname_desc == "Sinker"),])[1]

num.fb.02_two <- dim(count.02_two[(count.02_two$pitchname_desc == 'Four-seam FB' | 
                                     count.02_two$pitchname_desc == "Sinker"), ])[1]

num.fb.10_two <- dim(count.10_two[(count.10_two$pitchname_desc == 'Four-seam FB' | 
                                     count.10_two$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.11_two <- dim(count.11_two[(count.11_two$pitchname_desc == 'Four-seam FB' | 
                                     count.11_two$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.12_two <- dim(count.12_two[(count.12_two$pitchname_desc == 'Four-seam FB' | 
                                     count.12_two$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.20_two <- dim(count.20_two[(count.20_two$pitchname_desc == 'Four-seam FB' | 
                                     count.20_two$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.21_two <- dim(count.21_two[(count.21_two$pitchname_desc == 'Four-seam FB' | 
                                     count.21_two$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.22_two <- dim(count.22_two[(count.22_two$pitchname_desc == 'Four-seam FB' | 
                                     count.22_two$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.30_two <- dim(count.30_two[(count.30_two$pitchname_desc == 'Four-seam FB' | 
                                     count.30_two$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.31_two <- dim(count.31_two[(count.31_two$pitchname_desc == 'Four-seam FB' | 
                                     count.31_two$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]

num.fb.32_two <- dim(count.32_two[(count.32_two$pitchname_desc == 'Four-seam FB' | 
                                     count.32_two$pitchname_desc == "Sinker"), c(8,9,11,19)])[1]


# Pct FB and Pct BB as a vector in each count
pct.00_two <- c((num.fb.00_two / dim(count.00_two)[1]), ((dim(count.00_two)[1] - num.fb.00_two) / 
                                                           dim(count.00_two)[1]))

pct.01_two <- c((num.fb.01_two / dim(count.01_two)[1]), ((dim(count.01_two)[1] - num.fb.01_two) / 
                                                           dim(count.01_two)[1]))

pct.02_two <- c((num.fb.02_two / dim(count.02_two)[1]), ((dim(count.02_two)[1] - num.fb.02_two) / 
                                                           dim(count.02_two)[1]))

pct.10_two <- c((num.fb.10_two / dim(count.10_two)[1]), ((dim(count.10_two)[1] - num.fb.10_two) / 
                                                           dim(count.10_two)[1]))

pct.11_two <- c((num.fb.11_two / dim(count.11_two)[1]), ((dim(count.11_two)[1] - num.fb.11_two) / 
                                                           dim(count.11_two)[1]))

pct.12_two <- c((num.fb.12_two / dim(count.12_two)[1]), ((dim(count.12_two)[1] - num.fb.12_two) / 
                                                           dim(count.12_two)[1]))

pct.20_two <- c((num.fb.20_two / dim(count.20_two)[1]), ((dim(count.20_two)[1] - num.fb.20_two) / 
                                                           dim(count.20_two)[1]))

pct.21_two <- c((num.fb.21_two / dim(count.21_two)[1]), ((dim(count.21_two)[1] - num.fb.21_two) / 
                                                           dim(count.21_two)[1]))

pct.22_two <- c((num.fb.22_two / dim(count.22_two)[1]), ((dim(count.22_two)[1] - num.fb.22_two) / 
                                                           dim(count.22_two)[1]))

pct.30_two <- c((num.fb.30_two / dim(count.30_two)[1]), ((dim(count.30_two)[1] - num.fb.30_two) / 
                                                           dim(count.30_two)[1]))

pct.31_two <- c((num.fb.31_two / dim(count.31_two)[1]), ((dim(count.31_two)[1] - num.fb.31_two) / 
                                                           dim(count.31_two)[1]))

pct.32_two <- c((num.fb.32_two / dim(count.32_two)[1]), ((dim(count.32_two)[1] - num.fb.32_two) / 
                                                           dim(count.32_two)[1]))

# Full table
tot.pct_two <- rbind(pct.00_two, pct.01_two, pct.02_two, pct.10_two, pct.11_two, pct.12_two, pct.20_two, pct.21_two, pct.22_two, 
                     pct.30_two, pct.31_two, pct.32_two)

rownames(tot.pct_two) <- c('0-0', '0-1', '0-2', '1-0', '1-1', '1-2', '2-0', '2-1', '2-2',
                           '3-0', '3-1', '3-2')

colnames(tot.pct_two) <- c("FB.pct", "Break.pct")

round(tot.pct_two, 3)

####################################














































































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
