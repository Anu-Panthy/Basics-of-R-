data()
data(penguins)
head(penguins)
names(penguins)
library(ggplot2)
ggplot(penguins, aes(x = bill_len, y = flipper_len, color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

