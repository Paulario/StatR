library(tidyverse)
library(nycflights13)

# Create a function that randomly samples 100 observations from some population and calculates its mean
mean_generator <- function(population, n){
  sample <- sample(population, n, replace = T)
  mean(sample, na.rm = T)
  }


# Set up the sample size and the mumber of replicates beforehand
  n = 500
  times = 1200000
# Replicate sampling 100 obs from the population and store means of those samples in a data-frame
sample_distribution <- enframe(replicate(times, mean_generator(flights$air_time, n)))
summary <- sample_distribution %>% 
  summarise(mean = mean(value),
            sd = sd(value))

# SD of sampling distribution calculated bare-handed
sd(sample(flights$air_time, 500, replace = T),na.rm = T)/sqrt(500)

# Plot the results
sample_distribution %>%   
ggplot(aes(
          x = value,
          fill = (value<=summary$mean+summary$sd & value>=summary$mean-summary$sd))
       )+
  geom_histogram(binwidth = 0.09)+
  geom_segment(data = summary,aes(
    x=mean, y=0.01,
    xend = mean-sd, yend=0.01
  ), inherit.aes = F, arrow = arrow(length = unit(x = 0.1, units = "cm")),  size = 0.8, color = "blue")+
  #geom_vline(xintercept = c(mean(sample_distribution$value), 
                           # mean(flights$air_time, na.rm = T)), color = c("red","blue"), size = 1.1)+
  labs(fill = "", title = "Sampling distribution of sample means")+
  theme_bw()

  
# Suppose we've got a sample and its mean is 140. Calculate p-value that mean <=135

# Null hypothesis: mean == 150.6865
# Alternative hypothesis: mean != 150.6865
# Treshhold: 0.05



# Calculate a z-score for the supposed mean
z <- (140-summary$mean)/summary$sd

# Calculated based on the given sampling distribution
sample_distribution %>% 
  mutate(prop = ifelse(value <= summary$mean + z*summary$sd, 1,0)) %>% 
  summarise(p_value = mean(prop))

# Or use z-table to find out the probability to get this or a more extreme result
# z-table score: 0.00587 vs.0.00521

