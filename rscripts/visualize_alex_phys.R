library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv('/Users/dganguli/Desktop/deep_metamers_tidy.csv') %>% as.tbl()
df$category <- as.factor(df$category)

df$layer = factor(df$layer, 
                  levels = c("conv1", "pool1", "norm1", "conv2", "pool2", "norm2", "conv3", "conv4", "conv5", "pool5", "fc6", "fc7", "fc8")
)

df %>% 
  filter(layer != 'fc8') %>% 
  ggplot(aes(x=x,y=y,color=category)) + 
    geom_point() + 
    facet_wrap(~ layer) + theme_bw()


df <- read.csv('/Users/dganguli/Desktop/deep_metamers_mean_4v13_tidy.csv') %>%
  as.tbl()

df$layer = factor(df$layer, 
                  levels = c("conv1", "pool1", "norm1", "conv2", "pool2", "norm2", "conv3", "conv4", "conv5", "pool5", "fc6", "fc7", "fc8"))

df %>% 
  filter(layer != 'fc8') %>%
  ggplot(aes(x=mu_1, y=mu_2)) + 
  geom_point(alpha=0.5) + 
  facet_wrap(~ layer, scales="free") + 
  theme_bw() +
  xlab('Mean activity to texture 4') +
  ylab('Mean activity to texture 13')

df %>% 
  filter(layer != 'fc8') %>%
  group_by(layer) %>%
  summarize(rho = cor(mu_1, mu_2)) %>% 
  ggplot(aes(x=layer, y=rho)) +
  geom_bar(stat="identity") + theme_bw() + coord_flip()

