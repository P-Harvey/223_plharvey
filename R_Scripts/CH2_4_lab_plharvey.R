library(ggplot2)
library(GGally)
library(readxl)
library(dplyr)

# Load Data
data <- read_xlsx('data/sparrow.xlsx')
data <- data %>% select(-birdno)
length_mm <- mean(data$length)
wing_span_mm <- mean(data$wing_span)
beak_len_mm <- mean(data$beak)
humerus_len_mm <- mean(data$humerus)
sternum_len_mm <- mean(data$sternum)

mean(c(length_mm, wing_span_mm, beak_len_mm, humerus_len_mm, sternum_len_mm))

length_mm <- var(data$length)
wing_span_mm <- var(data$wing_span)
beak_len_mm <- var(data$beak)
humerus_len_mm <- var(data$humerus)
sternum_len_mm <- var(data$sternum)

var(c(length_mm, wing_span_mm, beak_len_mm, humerus_len_mm, sternum_len_mm))

min(cov(data))
max(cov(data))

ggpairs(data)

sum(eigen(cov(data))$values)
det(cov(data))

x <- read_xlsx('data/sparrow.xlsx') %>% select(birdno)

x <- read_xlsx('data/sparrow.xlsx')

trace_x = sum(diag(var(x)))

det_x = determinant(var(x), FALSE)

z_star = t(cov(x))*sample(x, 1, FALSE)

scatter.smooth(sort(x$length, decreasing = TRUE))
scatter.smooth(sort(x$wing_span, decreasing = TRUE))
scatter.smooth(sort(x$beak, decreasing = TRUE))
scatter.smooth(sort(x$humerus, decreasing = TRUE))
scatter.smooth(sort(x$sternum, decreasing = TRUE))

