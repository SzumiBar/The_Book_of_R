### Exercise 24.1 ###
#a)
library("MASS")
View(UScereal)
cereal <- UScereal
levels(x = cereal$mfr) <- factor(c("General Mills", "Kelloggs", rep(x = "Other", times = 4)))
cereal$shelf <- factor(cereal$shelf)

#b)
library("ggplot2")
ggplot(data = cereal, mapping = aes(x = protein, y = calories)) +
  geom_point(mapping = aes(col = shelf, shape = mfr)) +
  geom_smooth(method = "lm", mapping = aes(col = shelf)) +
  labs(x = "Protein", y = "Calories") +
  ggtitle("Question B") +
  theme(plot.title = element_text(hjust = 0.5)) -> i.

ggplot(data = cereal, mapping = aes(x = calories, fill = shelf)) +
  geom_density(alpha = 0.5) +
  labs(x = "Calories", y = "KDE") -> ii.

#c)
library("gridExtra")
grid.arrange(i., ii.)

#d)
ggplot(data = cereal, mapping = aes(x = protein, y = calories)) +
  geom_smooth(method = "loess", level = 0.9) +
  geom_point(mapping = aes(col = sugars, size = sodium, shape = shelf)) +
  facet_wrap(facets = ~ mfr)

#e)
library("car")
head(Salaries)

gg1 <- ggplot(data = Salaries, mapping = aes(x = yrs.service, y = salary)) +
  geom_point(mapping = aes(col = sex)) +
  geom_smooth(method = "loess", mapping = aes(col = sex)) +
  labs(x = "Years of service", y = "Salary")

#f)
gg2 <- ggplot(data = Salaries, mapping = aes(x = rank, y = salary)) +
  geom_boxplot(mapping = aes(fill = sex))

gg3 <- ggplot(data = Salaries, mapping = aes(x = discipline, y = salary)) +
  geom_boxplot(mapping = aes(col = sex))

gg4 <- ggplot(data = Salaries, mapping = aes(x = salary)) +
  geom_density(mapping = aes(fill = rank), alpha = 0.3) +
  labs(x = "Salary", y = "Kernel density estimation")

#g)
grid.arrange(gg1, gg2, gg3, gg4)

#h)
i. <- ggplot(data = Salaries, mapping = aes(x = salary)) +
  geom_density(mapping = aes(fill = sex), alpha = 0.7) +
  facet_wrap(facets = ~ rank)

ii. <- ggplot(data = Salaries, mapping = aes(x = yrs.service, y = salary)) +
  geom_point(mapping = aes(col = sex)) +
  geom_smooth(method = "lm") +
  facet_grid(rows = vars(discipline), cols = vars(rank), scales = "free_x")