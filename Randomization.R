#------------------------Randomization test to calculate two sided P value---------#

guide_a_score <- c(68,77,82,85)
guide_b_score <- c(53,64,71)


df.bar <- mean(guide_a_score) - mean(guide_b_score)
df.bar


reps <- 10000


results <- numeric(reps)

z <- c(guide_a_score, guide_b_score)
z

for (i in 1:reps) {
  temp<-sample(z)
  results[i] <- mean(temp[1:4]) - mean(temp[5:7])
  
}

results


hist(results)


#------------------Two sided P value--------#


p_value <- sum(results <= -15.33 | results >= 15.33)/reps

p_value