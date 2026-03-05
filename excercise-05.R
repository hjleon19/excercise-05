
#CHALLENGE 1


#step 1
library(tidyverse)

d <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/main/IMDB-movies.csv")

head(d)
nrow(d) 
glimpse(d)
summary(d)

#step2
d <- d %>%
  filter(startYear >= 1920 & startYear <= 1979 & runtimeMinutes >= 60 & runtimeMinutes <= 180) %>%
  mutate(decade = case_when(
    startYear >= 1920 & startYear <= 1929 ~ "20s",
    startYear >= 1930 & startYear <= 1939 ~ "30s",
    startYear >= 1940 & startYear <= 1949 ~ "40s",
    startYear >= 1950 & startYear <= 1959 ~ "50s",
    startYear >= 1960 & startYear <= 1969 ~ "60s",
    startYear >= 1970 & startYear <= 1979 ~ "70s"
  ))

table(d$decade)


#step3:Histograms
ggplot(data = d, aes(x = runtimeMinutes)) +
  geom_histogram() +
  facet_wrap(~decade)



#step4: get the mean and standard deviation for runtime by decade
results <- d %>%
  group_by(decade) %>%
  summarize(mean = mean(runtimeMinutes, na.rm = TRUE), 
            sd = sd(runtimeMinutes, na.rm = TRUE))

results

#Step 5: Sample , Statistics
sample1 <- d %>%
  group_by(decade) %>%
  slice_sample(n = 100, replace = FALSE) %>%
  summarize(sample_mean = mean(runtimeMinutes), 
            sample_sd = sd(runtimeMinutes))

sample1

#step:6 : Standard Error
# formula is SE = sample SD divided by square root of sample size
sample1 <- sample1 %>%
  mutate(n = 100) %>%
  mutate(se = sample_sd / sqrt(n))

sample1

#step 7: 
#combining sample results with population results to compare
comparison <- results %>%
  rename(pop_mean = mean, pop_sd = sd) %>%
  left_join(sample1, by = "decade")

# calculation foor the expected SE based on population SD
comparison <- comparison %>%
  mutate(expected_se = pop_sd / sqrt(100))

# calculation of how far off sample mean is from population mean
comparison <- comparison %>%
  mutate(mean_diff = sample_mean - pop_mean) %>%
  mutate(se_diff = se - expected_se)

comparison


#step8: Sapling distribution

sampling_dist <- data.frame()

#1000 samples of 100 movies from each decade
for(i in 1:1000) {
  sample_i <- d %>%
    group_by(decade) %>%
    sample_n(size = 100, replace = FALSE) %>%
    summarize(sample_mean = mean(runtimeMinutes),
              sample_sd = sd(runtimeMinutes)) %>%
    mutate(replicate = i)
  
  sampling_dist <- rbind(sampling_dist, sample_i)
}

head(sampling_dist)
dim(sampling_dist)


#step8: # mean and sd of sampling distribution for each decade
sampling_dist_summary <- sampling_dist %>%
  group_by(decade) %>%
  summarize(mean_of_means = mean(sample_mean),
            sd_of_means = sd(sample_mean))

sampling_dist_summary
comparison


#step9: 
# The sampling distributions are bell-shaped, demonstrating the Central Limit Theorem. Sample means closely approximate population means as expected.
sampling_dist_summary <- sampling_dist %>%
  group_by(decade) %>%
  summarize(mean_of_means = mean(sample_mean),
            sd_of_means = sd(sample_mean))

sampling_dist_summary

ggplot(data = sampling_dist, aes(x = sample_mean)) +
  geom_histogram(bins = 30) +
  facet_wrap(~decade) +
  labs(title = "Sampling Distribution of Sample Means by Decade",
       x = "Sample Mean Runtime (Minutes)",
       y = "Frequency")

# step 10!
se_comparison <- comparison %>%
  left_join(sampling_dist_summary, by = "decade")

se_comparison <- se_comparison %>%
  select(decade, se, expected_se, sd_of_means)

se_comparison <- se_comparison %>%
  rename(se_from_sample = se) %>%
  rename(se_from_population = expected_se) %>%
  rename(se_from_sampling_dist = sd_of_means)

se_comparison




##################################################
#CHALLENGE 2

library(tidyverse)
z = read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv")
head(z)
glimpse(z)
dim(z)


#STEP2: 
#calculate population mean and sd for each variable
mean_height = mean(z$height)
sd_height = sqrt(sum((z$height - mean_height)^2) / nrow(z))

mean_weight = mean(z$weight)
sd_weight = sqrt(sum((z$weight - mean_weight)^2) / nrow(z))

mean_age = mean(z$age)
sd_age = sqrt(sum((z$age - mean_age)^2) / nrow(z))

mean_zombies = mean(z$zombies_killed)
sd_zombies = sqrt(sum((z$zombies_killed - mean_zombies)^2) / nrow(z))

mean_education = mean(z$years_of_education)
sd_education = sqrt(sum((z$years_of_education - mean_education)^2) / nrow(z))

mean_height
sd_height
mean_weight
sd_weight
mean_age
sd_age
mean_zombies
sd_zombies
mean_education
sd_education


#step: 3
z %>%
  select(gender, height, weight, age, zombies_killed, years_of_education) %>%
  pivot_longer(cols = -gender, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = gender, y = value, fill = gender)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Distribution of Variables by Gender",
       x = "Gender", y = "Value")
#Step4
#Height vs Age: The scatterplot shows a weak positive correlation between height and age for both genders. As age increases, height tends to increase slightly, possibly because younger individuals are still growing.
ggplot(data = z, aes(x = age, y = height)) +
  geom_point(aes(color = gender))

#Weight vs Age: The scatterplot shows no clear relationship between weight and age. The points are scattered randomly with no obvious pattern, indicating little to no correlation.
ggplot(data = z, aes(x = age, y = weight)) +
  geom_point(aes(color = gender))


#Step5: Normality check
z %>%
  select(height, weight, age, zombies_killed, years_of_education) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = "free")
#Height, weight, and age all look pretty normal. The histograms are bell shaped and symmetric, and in the Q-Q plots the points follow the diagonal line pretty closely.


# Q-Q plot
z %>%
  select(height, weight, age, zombies_killed, years_of_education) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(sample = value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~variable, scales = "free")
#poisson distribution! counting discrete events.Zombies killed and years of education don't look normal at all. 


#step5
#take a sample of 50 people
sample_50 = z %>% sample_n(size=50, replace=FALSE)

#calculate means for each variable
mean_height_sample = mean(sample_50$height)
mean_weight_sample = mean(sample_50$weight)
mean_age_sample = mean(sample_50$age)
mean_zombies_sample = mean(sample_50$zombies_killed)
mean_education_sample = mean(sample_50$years_of_education)

#calculate sample standard deviations
sd_height_sample = sd(sample_50$height)
sd_weight_sample = sd(sample_50$weight)
sd_age_sample = sd(sample_50$age)
sd_zombies_sample = sd(sample_50$zombies_killed)
sd_education_sample = sd(sample_50$years_of_education)

#calculate standard errors (SE = sd/sqrt(n))
n = 50
se_height = sd_height_sample / sqrt(n)
se_weight = sd_weight_sample / sqrt(n)
se_age = sd_age_sample / sqrt(n)
se_zombies = sd_zombies_sample / sqrt(n)
se_education = sd_education_sample / sqrt(n)

#get critical value for 95% CI using t distribution
#df = n - 1 = 49
critical_value = qt(0.975, df=49)

#calculate 95% confidence intervals
#CI = mean +/- critical_value * SE

# height CI
ci_lower_height = mean_height_sample - critical_value * se_height
ci_upper_height = mean_height_sample + critical_value * se_height

# weight CI
ci_lower_weight = mean_weight_sample - critical_value * se_weight
ci_upper_weight = mean_weight_sample + critical_value * se_weight

#age CI
ci_lower_age = mean_age_sample - critical_value * se_age
ci_upper_age = mean_age_sample + critical_value * se_age

# zombies killed CI
ci_lower_zombies = mean_zombies_sample - critical_value * se_zombies
ci_upper_zombies = mean_zombies_sample + critical_value * se_zombies

#years of education CI
ci_lower_education = mean_education_sample - critical_value * se_education
ci_upper_education = mean_education_sample + critical_value * se_education

#print!!!
mean_height_sample
se_height
ci_lower_height
ci_upper_height

mean_weight_sample
se_weight
ci_lower_weight
ci_upper_weight

mean_age_sample
se_age
ci_lower_age
ci_upper_age

mean_zombies_sample
se_zombies
ci_lower_zombies
ci_upper_zombies

mean_education_sample
se_education
ci_lower_education
ci_upper_education



#STEP 7!!!

# draw 199 more samples of 50
set.seed(456)
sampling_dist_200 <- replicate(199, {
  z %>%
    slice_sample(n = 50) %>%
    summarize(
      mean_height = mean(height),
      mean_weight = mean(weight),
      mean_age = mean(age),
      mean_zombies = mean(zombies_killed),
      mean_education = mean(years_of_education)
    )
}, simplify = FALSE) %>%
  bind_rows()

# add first sample to make 200 total
first_sample_means <- sample_50 %>%
  summarize(
    mean_height = mean(height),
    mean_weight = mean(weight),
    mean_age = mean(age),
    mean_zombies = mean(zombies_killed),
    mean_education = mean(years_of_education)
  )

sampling_dist_200 <- bind_rows(first_sample_means, sampling_dist_200)

#get mean and sd for each sampling distribution
sampling_summary <- sampling_dist_200 %>%
  summarize(
    mean_of_height_means = mean(mean_height),
    sd_of_height_means = sd(mean_height),
    mean_of_weight_means = mean(mean_weight),
    sd_of_weight_means = sd(mean_weight),
    mean_of_age_means = mean(mean_age),
    sd_of_age_means = sd(mean_age),
    mean_of_zombies_means = mean(mean_zombies),
    sd_of_zombies_means = sd(mean_zombies),
    mean_of_education_means = mean(mean_education),
    sd_of_education_means = sd(mean_education)
  )

sampling_summary

#compare sd of sampling dist to SE from original sample
comparison_se <- tibble(
  variable = c("height", "weight", "age", "zombies_killed", "years_of_education"),
  se_from_sample = c(se_height, se_weight, se_age, se_zombies, se_education),
  sd_of_sampling_dist = c(
    sampling_summary$sd_of_height_means,
    sampling_summary$sd_of_weight_means,
    sampling_summary$sd_of_age_means,
    sampling_summary$sd_of_zombies_means,
    sampling_summary$sd_of_education_means
  )
)

comparison_se
#7 --- The SD of the sampling distribution is pretty close to the SE from the first sample for all variables. This shows that the SE formula works well to estimate how much sample means will vary. They're not exactly the same because of random variation, but they're close enough to show the theory works in practice.




#STEP 8!!!

# Plot histograms of sampling distributions
sampling_dist_200 %>%
  pivot_longer(everything(), names_to = "variable", values_to = "sample_mean") %>%
  ggplot(aes(x = sample_mean)) +
  geom_histogram(bins = 25) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Sampling Distributions of Sample Means",
       x = "Sample Mean", y = "Frequency")

# Q-Q plots to check normality
sampling_dist_200 %>%
  pivot_longer(everything(), names_to = "variable", values_to = "sample_mean") %>%
  ggplot(aes(sample = sample_mean)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Q-Q Plots of Sampling Distributions")

#8--- All the sampling distributions look pretty normal and bell-shaped.
#The histograms are symmetric and centered, and in the Q-Q plots the points follow the diagonal line really well.
#It is intereesting that 'zombies_killed' and 'years_of_education' look normal even though the original data was Poisson distributed (not normal at all). 
#This shows the Central Limit Theorem working - when you take means of samples that are big enough (n=50), the distribution of those means becomes normal no matter what the original distribution looked like.
#Pretty cool that it actually works in practice.


#STEP9(!!!)
#get the 95% CI using the quantile function
#need 2.5% and 97.5% to get middle 95%


ci_from_sampling <- tibble(
  variable = c("height", "weight", "age", "zombies_killed", "years_of_education"),
  ci_lower = c(
    quantile(sampling_dist_200$mean_height, 0.025),
    quantile(sampling_dist_200$mean_weight, 0.025),
    quantile(sampling_dist_200$mean_age, 0.025),
    quantile(sampling_dist_200$mean_zombies, 0.025),
    quantile(sampling_dist_200$mean_education, 0.025)
  ),
  ci_upper = c(
    quantile(sampling_dist_200$mean_height, 0.975),
    quantile(sampling_dist_200$mean_weight, 0.975),
    quantile(sampling_dist_200$mean_age, 0.975),
    quantile(sampling_dist_200$mean_zombies, 0.975),
    quantile(sampling_dist_200$mean_education, 0.975)
  )
)

ci_from_sampling

#compare the two different CI methods
#one from step 6 [single sample]and one from sampling dist
ci_comparison <- tibble(
  variable = c("height", "weight", "age", "zombies_killed", "years_of_education"),
  #these are from the first sample CI
  ci_lower_sample = c(ci_lower_height, ci_lower_weight, ci_lower_age, ci_lower_zombies, ci_lower_education),
  ci_upper_sample = c(ci_upper_height, ci_upper_weight, ci_upper_age, ci_upper_zombies, ci_upper_education),
  #these are from the quantiles of sampling distribution
  ci_lower_quantile = ci_from_sampling$ci_lower,
  ci_upper_quantile = ci_from_sampling$ci_upper,
  #add pop mean to see if its captured
  pop_mean = c(mean_height, mean_weight, mean_age, mean_zombies, mean_education)
)
ci_comparison

#STEP10

#bootstrapping- from our original sample w replacement
#1000 times and calculate mean each time
#set seed
set.seed(123) 

#make empty vectors to store the bootstrap means
boot_height <- numeric(1000)
boot_weight <- numeric(1000)
boot_age <- numeric(1000)
boot_zombies <- numeric(1000)
boot_education <- numeric(1000)

#loop 1000 times, each time resample from original sample
for(i in 1:1000) {
  boot_sample <- sample_50[sample(nrow(sample_50), nrow(sample_50), replace = TRUE), ]
  boot_height[i] <- mean(boot_sample$height)
  boot_weight[i] <- mean(boot_sample$weight)
  boot_age[i] <- mean(boot_sample$age)
  boot_zombies[i] <- mean(boot_sample$zombies_killed)
  boot_education[i] <- mean(boot_sample$years_of_education)
}

#get 95% CI from bootstrap - 2.5% and 97.5% quantiles
ci_bootstrap <- tibble(
  variable = c("height", "weight", "age", "zombies_killed", "years_of_education"),
  ci_lower_boot = c(
    quantile(boot_height, 0.025),
    quantile(boot_weight, 0.025),
    quantile(boot_age, 0.025),
    quantile(boot_zombies, 0.025),
    quantile(boot_education, 0.025)
  ),
  ci_upper_boot = c(
    quantile(boot_height, 0.975),
    quantile(boot_weight, 0.975),
    quantile(boot_age, 0.975),
    quantile(boot_zombies, 0.975),
    quantile(boot_education, 0.975)
  )
)

#bootstrap CIs*****
ci_bootstrap

# all three methods:
ci_all <- ci_comparison %>%
  left_join(ci_bootstrap, by = "variable")

ci_all

#How does this compare to the CIs generated in Step 9?
#ANSWER: The bootstrap CIs from Step 10 are very similar to both CI methods from Step 9. Compared to the single-sample CIs, the bootstrap intervals are nearly identical since both methods are drawing from the same original sample of 50. 
#Compared to the sampling distribution CIs from Step 9, the bootstrap CIs are slightly wider for some variables, which makes sense since the sampling distribution was built from 200 draws from the full population while bootstrapping only resamples from one sample. 
#The 3 methods capture the population mean within their bounds, suggesting that bootstrapping is a reliable way to estimate confidence intervals even without access to the full population.








