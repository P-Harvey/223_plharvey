################################################################################
library(car)
library(dplyr)
library(ggplot2)
library(GGally)
library(MVN)
library(readxl)
library(tidyverse)
################################################################################
# y_i ~ N_3(0, 1)
y = matrix(rnorm(3),3)
# x_i = [y_i1 - y_i2, y_i1 - y_i3, y_i2 - y_i3]^T
x = matrix(data = c(y[1]-y[2], y[1]-y[3], y[2]-y[3]), 3)
# Specify covmat(x)^(-1/2)
x_d = 1/sqrt(cov(x))
# 3x3 Identity matrix
I = diag(1,3,3,FALSE)
# Sigma = covmat(y)
Sigma = cov(y)
# Eigenvalue decomposition of Sigma
E = matrix(eigen(Sigma)$values,3,3)
# Uh oh! It is singular:
solve(E)

y_1 = matrix(rnorm(3),3,1)
y_2 = matrix(rnorm(3),3,1)
y_3 = matrix(rnorm(3),3,1)
y = matrix(c(y_1, y_2, y_3),3,3)

y_d = 1/sqrt(cov(y))
Sigma = cov(y)
E = matrix(eigen(Sigma)$values,3,3)
solve(E)
################################################################################
data <- read_xlsx('data/Lebron.xlsx') %>% select(-Game_num)

y_1 <- matrix(data$Made1, length(data$Made1), 1)            # 1-point made
y_1_bar <- mean(y_1) # 6.673077
y_2 <- matrix(data$Miss1, length(data$Miss1), 1)            # 1-point miss
y_2_bar <- mean(y_2) # 2.330769
y_3 <- matrix(data$Per1, length(data$Per1), 1)              # (% 1)
y_3_bar <- mean(y_3) # 72.06846
y_4 <- matrix(data$Made2, length(data$Made2), 1)            # 2-point made
y_4_bar <- mean(y_4) # 10.27308
y_5 <- matrix(data$Miss2, length(data$Miss2), 1)            # 2-point miss
y_5_bar <- mean(y_5) # 10.45
y_6 <- matrix(data$Per2, length(data$Per2), 1)              # (% 2)
y_6_bar <- mean(y_6) # 49.67846
y_7 <- matrix(data$Made3, length(data$Made3), 1)            # 3-point made
y_7_bar <- mean(y_7) # 1.592308
y_8 <- matrix(data$Miss3, length(data$Miss3), 1)            # 3-point miss
y_8_bar <- mean(y_8) # 3.157692
y_9 <- matrix(data$Per3, length(data$Per3), 1)              # (% 3)
y_9_bar <- mean(y_9) # 30.56462

Y <- matrix(data=c(y_1,y_2,y_3,y_4,y_5,y_6,y_7,y_8,y_9),
            length(y_1), 9)                            # Made/Miss (Pcts)
colnames(Y) <- c('Made1','Miss1','Per1',
                 'Made2','Miss2','Per2',
                 'Made3','Miss3','Per3')
################################################################################
y_10<- matrix(data=c(data$Made1+2*data$Made2+3*data$Made3),
              length(data$Made1), 1)                        # Made Points
y_10_bar <- mean(y_10) # 31.99615

y_11<- matrix(data=c(data$Miss1+2*data$Miss2+3*data$Miss3),
              length(data$Miss1), 1)                        # Miss Points
y_11_bar <- mean(y_11) # 32.70385

Y_2  <- matrix(data=c(y_10,y_11),
               length(y_1), 2)                            # Made/Miss (Points)
colnames(Y_2) <- c('MadePts','MissPts')
################################################################################
y_bar_tab <- matrix(rbind(y_1_bar,  y_2_bar,  y_3_bar,
                          y_4_bar,  y_5_bar,  y_6_bar,
                          y_7_bar,  y_8_bar,  y_9_bar,
                          y_10_bar, y_11_bar), 11, 1)
colnames(y_bar_tab) <- c('y_bar')
rownames(y_bar_tab) <- c('y_1_bar',  'y_2_bar',  'y_3_bar',
                         'y_4_bar',  'y_5_bar',  'y_6_bar',
                         'y_7_bar',  'y_8_bar',  'y_9_bar',
                         'y_10_bar', 'y_11_bar')
as.table(y_bar_tab)
################################################################################
as.table(cov(Y))
################################################################################
mvn(Y)
cor(Y, method = c("spearman"))
################################################################################
ggpairs(data.frame(Y))
################################################################################
det(cov(Y))        # 2357352034.000000000
sum(diag(cov(Y)))  #       1081.005000000
################################################################################
det(cor(Y))        #          0.003445977
sum(diag(cor(Y)))  #          9.000000000
################################################################################
square_layout <- theme_classic()  + theme(aspect.ratio = 1)

labels <- labs(x = "Points Made", y = "Points Missed",
               title = "Relationship between Points Made and Missed",
               caption = "data from LeBron James' NBA history")

m_m_df <- data.frame(matrix(c(y_10, y_11),length(y_10),2))
colnames(m_m_df) <- c('Points_Made', 'Points_Missed')

made_miss <- ggplot(m_m_df, 
                    aes(x = Points_Made, 
                        y = Points_Missed))

made_miss + 
  geom_point(aes(),
             size = 2,
             alpha = 0.6) +
  geom_density_2d(aes()) +
  labels + labs(subtitle = "Points Possible") +
  square_layout
################################################################################
m_m_mean   <- colMeans(m_m_df)
m_m_Sigma  <- cov(m_m_df)
mahala_m_m <- data.frame(mahalanobis(m_m_df, m_m_mean, m_m_Sigma), 1:260)

colnames(mahala_m_m) <- c('Mahalanobis_Distance', 'Game')

outliers <- boxplot.stats(m_m_df$Points_Made)$out
out_ind  <- which(m_m_df$Points_Made %in% c(outliers))
outliers <- boxplot.stats(m_m_df$Points_Missed)$out
out_ind  <- c(out_ind, which(m_m_df$Points_Missed %in% c(outliers)))
out_ind  <- sort(out_ind) # 33  46  53  57 171 173 174 178 225 236

made_miss_out <- mahala_m_m[mahala_m_m$Mahalanobis_Distance >=6.45,]
colnames(made_miss_out) <- c('Mahalanobis_Distance', 'Game')

labels <- labs(x = "Game", y = "Distance",
               title = "Mahalanobis Distance",
               subtitle = "Points Scored per Game",
               caption = "data from LeBron James' NBA history")

ggplot(mahala_m_m,
       aes(x=Game,
           y=Mahalanobis_Distance)) +
  geom_point(aes(),
             size = 2,
             alpha = 0.6,
             color = 'black') +
  geom_point(made_miss_out,
             mapping = aes(made_miss_out$Game,
                           made_miss_out$Mahalanobis_Distance),
             size = 2,
             alpha = 0.6,
             color ='red') +
  labels +
  square_layout
################################################################################
par(mfrow=c(3,2))
qqPlot(y_1,
       main="Normal Q-Q (Made: 1 Pointer)")
qqPlot(y_2,
       main="Normal Q-Q (Miss: 1 Pointer)")
qqPlot(y_4,
       main="Normal Q-Q (Made: 2 Pointer)")
qqPlot(y_5,
       main="Normal Q-Q (Miss: 2 Pointer)")
qqPlot(y_7,
       main="Normal Q-Q (Made: 3 Pointer)")
qqPlot(y_8,
       main="Normal Q-Q (Miss: 3 Pointer)")
par(mfrow=c(1,1))
################################################################################
Y_3 = data.frame(y_1,y_2,y_4,y_5,y_7,y_8)
colnames(Y_3) <- c('Made1','Miss1','Made2','Miss2','Made3','Miss3')
mvn(Y_3)
result <- mvn(Y_3, mvnTest = "hz", multivariateOutlierMethod = "quan")
result <- mvn(Y_3, mvnTest = "hz", multivariateOutlierMethod = "adj")
################################################################################
Y_4 = data.frame(y_3,y_6,y_9)
colnames(Y_4) <- c('Per1','Per2','Per3')
mvn(Y_4)
result <- mvn(Y_4, mvnTest = "hz", multivariateOutlierMethod = "quan")
result <- mvn(Y_4, mvnTest = "hz", multivariateOutlierMethod = "adj")
################################################################################
y_12 <- matrix(data=(data$Per1+data$Per2+data$Per3)/3,
               length(data$Per1), 1)

qqnorm(y_12,
       main='Normal Q-Q\nAverage Shot Percentage Per Game')
qqline(y_12, col='red')
