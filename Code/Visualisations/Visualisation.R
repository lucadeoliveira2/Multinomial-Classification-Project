# Libraries #

library(ggplot2)
library(ggthemes)
library(ggridges)
library(plotly)
library(corrplot)
head(df)

# Exploring Data #

# Sepal Length by Species

pl <- df %>%
  group_by(Species) %>%
  mutate(mean.SL = mean(Sepal.Length)) %>%
  ungroup() %>%
  ggplot(aes(x = Sepal.Length, y = Species, col = Species)) + geom_jitter(shape = 1, show.legend = FALSE) + geom_vline(aes(xintercept = mean(Sepal.Length)))
pl2 <- pl + stat_summary(geom = 'point', fun = 'mean', size = 4, show.legend = F) + geom_segment(aes(x = mean.SL, xend = mean(Sepal.Length), y = Species, yend = Species))
pl3 <- pl2 + geom_violin(aes(fill = Species), alpha = 0.1, show.legend = F) + annotate(geom = 'text', label = '< Mean Length for all Flowers', x = 7, y = 'setosa') + labs(x = 'Sepal Length', title = 'Sepal Length by Species')
pl4 <- pl3 + theme_clean() +theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c('forestgreen', 'salmon', 'turquoise')) + scale_fill_manual(values = c('forestgreen', 'salmon', 'turquoise'))
pl4

# Sepal Area and Petal Area

df %>%
  mutate(Sepal.Area = Sepal.Length*Sepal.Width, Petal.Area = Petal.Length*Petal.Width) %>%
  ggplot(aes(x = Petal.Area, y = Sepal.Area, col = Species)) + geom_point() + geom_smooth(se = FALSE) + scale_color_manual(values = c('forestgreen', 'salmon', 'turquoise'))

# Pairs Plots

pairs(iris[, 1:4], col = df$Species, oma=c(3,3,3,15))

# Correlations Plots

par(mfrow = c(2,2))
corrplot(cor(iris[,1:4]), method = 'number', title = 'Overall', type = 'upper', bg = 'white')
corrplot(cor(df[df$Species == 'virginica', 1:4]), method = 'number', title = 'Virginica', type = 'upper', bg = 'white')
corrplot(cor(df[df$Species == 'versicolor', 1:4]), method = 'number', title = 'Versicolor', type = 'upper', bg = 'white')
corrplot(cor(df[df$Species == 'setosa', 1:4]), method = 'number', title = 'Setosa', type = 'upper', bg = 'white')

# Petal Length

pl <- df %>%
  ggplot(aes(x = Petal.Length, y = after_stat(density))) + geom_histogram(data = df[df$Species == 'setosa',],bins = 20,alpha = 0.5, fill = 'forestgreen') + geom_histogram(data = df[df$Species == 'virginica',],alpha = 0.5, fill = 'turquoise') + geom_histogram(data = df[df$Species == 'versicolor',],alpha = 0.5, fill = 'salmon')
pl2 <- pl + annotate(geom = 'text', label = c('Setosa', 'Versicolor', 'Virginica'), x = c(2.2, 3.5, 6.5), y = c(1.25, 0.8, 0.8), col = c('forestgreen', 'salmon', 'turquoise')) + theme_clean() + labs(y = '', x = 'Petal Length (cm)', title = 'Petal Length Histogram')
pl3 <- pl2 + geom_density(aes(col = Species), show.legend = FALSE, lwd=1) + scale_color_manual(values = c('forestgreen', 'salmon', 'turquoise'))
pl3

# Measurements Histogram

pl <- ggplot(df, aes(y = after_stat(density))) + geom_histogram(aes(x = Sepal.Length), fill = 'red4', alpha = 0.5, bins = 25)
pl2 <- pl + geom_histogram(aes(x = Sepal.Width), fill = 'blue', alpha = 0.5, bins = 25) + geom_histogram(aes(x = Petal.Width), fill = 'green4', alpha = 0.5, bins = 25) + geom_histogram(aes(x = Petal.Length), fill = 'yellow2', alpha = 0.5, bins = 25)
pl3 <- pl2 + theme_clean() + annotate(geom = 'text', label = c('- Sepal Length', '- Sepal Width', '- Petal Width', '- Petal Length'),x = c(7.5,7.5,7.5,7.5), y = c(0.95, 0.9, 0.85, 0.8), col = c('red4', 'blue', 'green4', 'yellow'))
pl3

# Measurements Boxplot

pl <- df %>%
  pivot_longer(cols = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'), names_to = 'measurement', values_to = 'values') %>%
  ggplot(aes(x = measurement, y = values, fill = Species)) + geom_boxplot(alpha = 0.5)
pl + scale_fill_manual(values = c('forestgreen', 'salmon', 'turquoise')) + theme_clean()

# Measurements Distributions in Each Class

pl <- df %>%
  group_by(Species) %>%
  mutate(mean.PL = mean(Petal.Length), mean.PW = mean(Petal.Width), mean.SL = mean(Sepal.Length), mean.SW = mean(Sepal.Width)) %>%
  ungroup() %>%
  pivot_longer(cols = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'), names_to = 'measurement', values_to = 'values') %>%
  ggplot(aes(x = values, fill = measurement)) + geom_density(alpha = 0.5) + facet_grid(Species~., scales = 'free') + geom_vline(aes(xintercept = mean.PL), lty = 2) + geom_vline(aes(xintercept = mean.PW), lty = 2) + geom_vline(aes(xintercept = mean.SL), lty = 2) + geom_vline(aes(xintercept = mean.SW), lty = 2)
pl + labs(x = 'Measurements (in cm)', title = 'Distribution of Predictors in Each Class') + theme_clean()
