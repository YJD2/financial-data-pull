####### Working with Data in R Tutorial 4 ######
####### Cian O'Shea #######
#####Statistics and Regression Analysis #####

#### Task 1 Students ####

### 1. 
## Install our Packages ##
#install.packages("haven")
#install.packages("skimr")

## Load in our packages that we will be using ##

library(magrittr)
library(tidyverse)

### 2. 
## Import our data, there are variety of ways that we can do this. 
# Below I have imported it using an online link. I could also import it from my downloads folder.
# Note the dta file that stata uses.
grades <- read_dta(file = "https://www.dropbox.com/s/wwp2cs9f0dubmhr/grade5.dta?dl=1")

### 3. Describing the Dataset
## What is the unit of observation?


## How many observations are there


## View Dataset


# avgmath represents the average composite math scores in the class, 
# and avgverbal represents the average composite verbal score in the class.

## Use skim function to obtain common summary statistics


### 4. How might we get our first insights We might do some visualisation?

## Scatter plot for Average Reading Scores and Class Size


## Scatter plot for Average Maths Scores and Class Size


## Compute Correlation


# Both correlations are positive, consistent with the positive association suggested by the previous scatter plots.
# However they can be considered quite weak.

### Slide 6 Graphs

## Binned Scatter Plot: Maths and Class Size
grades_avg_cs <- grades %>%
  group_by(classize) %>%
  summarise(avgmath_cs = mean(avgmath),
            avgverb_cs = mean(avgverb))

g_math_cs = ggplot(grades_avg_cs, aes(x = classize, y = avgmath_cs)) + 
  geom_point(size = 2) +
  xlim(0,45) +
  ylim(0, 100) +
  labs(
    x = "Class size",
    y = "Average score",
    title = "Mathematics") +
  theme_bw(base_size = 20)
g_math_cs

## Binned Scatter Plots: Reading and Class Size 

g_verb_cs = ggplot(grades_avg_cs, aes(x = classize, y = avgverb_cs)) + 
  geom_point(size = 2) +
  xlim(0,45) +
  ylim(0, 100) +
  labs(x = "Class size",
       y = "Average score",
       title = "Reading") +
  theme_bw(base_size = 20)
g_verb_cs

### Slide 7: Focusing on Maths Scores

g_math_cs +
  ylim(50, 80) +
  theme_bw(base_size = 14)

## Including a Line
g_math_cs +
  ylim(50, 80) +
  theme_bw(base_size = 14) +
  geom_hline(yintercept = 65, col = "#d90502")

## Including a diagonal line 
g_math_cs +
  ylim(50, 80) +
  theme_bw(base_size = 14) +
  geom_abline(intercept = 55,slope = 0.6, col = "#d90502")

## Slide 15 Plotting a Regression line

plot_1 <- g_math_cs +
  ylim(50, 80) +
  theme_bw(base_size = 14)

plot_2 <- plot_1 +
  stat_smooth(data = grades_avg_cs, method = "lm", se = FALSE, colour = "#d90502") +
  annotate("text", x = 6.5, y = 64, label = "hat(y)", parse = TRUE, colour = "#d90502", size = 6)
plot_2

## Slide 23: OLS with R

# OLS regression of class size on average maths score
lm(avgmath_cs ~ classize, grades_avg_cs)

# you can also assign a linear regression as an object
our_first_lm <- lm(avgmath_cs ~ classize, grades_avg_cs)
our_first_lm

summary(our_first_lm)

#### Task 2: Students ####

# Run the following Code to aggregate data at the class size level

grades_avg_cs <- grades %>%
  group_by(classize) %>%
  summarise(avgmath_cs = mean(avgmath),
            avgverb_cs = mean(avgverb))

## 1.
# Regress average verabl score on class size and interpret. 


## Interpret: On average, a class size increase of one student is associated with a 0.13 increase in average verbal score
## 2. 
# Compute the OLS coefficients b0 and b1 using the formulas. 



## 3. Student Interpretation

## 4. Student Interpretation

#### Task 3: Student ####

## 1. Regress avgmath_cs on class size and assign it. 



## 2. Use summary function on math_reg


## 3. Compute the squared correlation and compare it with R-squared. 


## 4. Repeat steps 1 and 2 for avgverb_cs

