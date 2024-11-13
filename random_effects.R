
library(tidyverse)
library(lme4)
library(pROC)
library(Metrics)
setwd("~/GitHub/mlb-pitchers")

train <- read.csv('train_data.csv')

train <- train %>% select(Count_0_1, Count_0_2, Count_1_0, Count_1_1, Count_1_2, Count_2_0, Count_2_2, Count_3_0, Count_3_1, Count_2_1,
                 pitcher, pitch_type, batter, pitcher_fb_rate_batter_count, batter_fb_rate_batter_count, pitcher_fb_rate_pitcher_count,
                 batter_fb_rate_pitcher_count, )


model<- glm(pitch_type ~  Count_0_1 + Count_0_2 + Count_1_0 + Count_1_1 + Count_1_2 + Count_2_0 + Count_2_2 +
        Count_3_0 + Count_3_1 + Count_2_1 +  pitcher_fb_rate_batter_count + batter_fb_rate_batter_count + pitcher_fb_rate_pitcher_count + 
          batter_fb_rate_pitcher_count,
      data = train, 
      family = binomial)
drop1(model, test='LRT')
summary(model)

test <- read.csv('test_data.csv')

Xtest <- test %>% select(Count_0_1, Count_0_2, Count_1_0, Count_1_1, Count_1_2, Count_2_0, Count_2_2, Count_3_0, Count_3_1, Count_3_2, Count_2_1,
                         pitcher_fb_rate_batter_count, batter_fb_rate_batter_count, pitcher_fb_rate_pitcher_count,
                         batter_fb_rate_pitcher_count, platoon_l_l, platoon_l_r, 
                         platoon_r_r, outs, leverage, prev_pitch)
Xtest[is.na(Xtest)] <- 0
predicted_probs <- predict(model, newdata = Xtest, type = "response")

write.csv(predicted_probs, 'predicted_probs.csv')
actuals <- test$pitch_type

brier_score <- mean((actuals - predicted_probs)^2)

brier_score

auc_score <- roc(actuals, predicted_probs)$auc
auc_score

