library(ggplot2)
library(tidyverse)
library(dplyr)
library(GGally)
library(readxl)
library(factoextra)

path <- "/Users/patrick/Projects/STAT_223/223_plharvey/Data/track.xlsx"
data <- read_xlsx(path)

data <- column_to_rownames(data, var = "Country")

#data <- data %>% select(-Country)

p = ncol(data)

pc_S <- prcomp(x = data)

pc_R <- prcomp(x = data,
               scale. = T)

data.frame(
  PC = 1:ncol(data),
  
  variance = cov(data) |> 
    eigen() |> 
    pluck('values')
) |> 
  mutate(var_prop = variance/sum(variance),
         cumul_var_prop = cumsum(var_prop))

cov(data)
summary(cov(data))
ggpairs(data.frame(cov(data)))

eigen(cov(data))$values

data.frame(
  PC = 1:ncol(data),
  
  variance = cor(data) |> 
    eigen() |> 
    pluck('values')
) |> 
  mutate(var_prop = variance/sum(variance),
         cumul_var_prop = cumsum(var_prop))

cor(data)
summary(cor(data))
ggpairs(data.frame(cor(data)))

eigen(cor(data))$values

summary(pc_S)

fviz_pca(X = pc_S,
         axes = c(1, 2),      
         geom = "text", 
         alpha.ind = 0.1) 

pc_S |> 
  pluck("rotation") |> 
  data.frame() |> 
  select(PC1:PC2) |> 
  rownames_to_column(var = "meter") |> 
  pivot_longer(cols = contains("PC"),
               names_to = "PC",
               values_to = "weights") |> 
  
  ggplot(mapping = aes(x = PC,
                       y = meter,
                       fill = weights)) +
  
  geom_tile(mapping = aes(alpha = abs(weights)),
            color = "white", 
            size = 1,
            show.legend = F) + 
  
  geom_text(mapping = aes(label = round(weights,2)),
            color = "white",
            fontface = 2) + 
  
  labs(y = "Variables",
       x = "Principal Components") +
  
  theme_test() +
  
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0))+
  scale_fill_gradient2()

fviz_screeplot(X = pc_S,
               choice = "eigenvalue",
               geom = "line",
               linecolor = "steelblue",
               ncp = p) + 
  
  labs(title = "Screeplot using the Correlation Matrix",
       x = "Principal Component") + 
  
  # Average variance line
  geom_hline(yintercept = summary(pc_R)$sdev^2|> mean(),
             color = "darkred")

# Displaying the percentage explained by using choice = "variance":
fviz_screeplot(X = pc_S,
               choice = "variance",
               geom = "line",
               linecolor = "steelblue",
               ncp = p) + 
  
  labs(title = "Screeplot using the Correlation Matrix",
       x = "Principal Component")


summary(pc_R)

fviz_pca(X = pc_R,
         axes = c(1, 2),      
         geom = "text", 
         alpha.ind = 0.1) 

pc_R |> 
  pluck("rotation") |> 
  data.frame() |> 
  select(PC1:PC2) |> 
  rownames_to_column(var = "meter") |> 
  pivot_longer(cols = contains("PC"),
               names_to = "PC",
               values_to = "weights") |> 
  
  ggplot(mapping = aes(x = PC,
                       y = meter,
                       fill = weights)) +
  
  geom_tile(mapping = aes(alpha = abs(weights)),
            color = "white", 
            size = 1,
            show.legend = F) + 
  
  geom_text(mapping = aes(label = round(weights,2)),
            color = "white",
            fontface = 2) + 
  
  labs(y = "Variables",
       x = "Principal Components") +
  
  theme_test() +
  
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0))+
  scale_fill_gradient2()

fviz_screeplot(X = pc_R,
               choice = "eigenvalue",
               geom = "line",
               linecolor = "steelblue",
               ncp = p) + 
  
  labs(title = "Screeplot using the Correlation Matrix",
       x = "Principal Component") + 
  
  # Average variance line
  geom_hline(yintercept = summary(pc_R)$sdev^2|> mean(),
             color = "darkred")

# Displaying the percentage explained by using choice = "variance":
fviz_screeplot(X = pc_R,
               choice = "variance",
               geom = "line",
               linecolor = "steelblue",
               ncp = p) + 
  
  labs(title = "Screeplot using the Correlation Matrix",
       x = "Principal Component")
