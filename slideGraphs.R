library(tidyverse)


d <- tibble(
	factor = c("congruency", "awareness", "interaction"),
	beta = c(39, 103, -4),
	lower = c(24, 77, -26),
	upper = c(51, 131, 16))


p <- ggplot(d, aes(x = factor, y = beta, ymin = lower, ymax = upper))
p <- p + geom_bar(stat = "identity")
p <- p + geom_errorbar(width = 0.2)
p <- p + geom_hline(yintercept = 0)
ggsave("ecvp_plt1.pdf", height = 6, width = 6)

d2 <- tibble(
	congruency = c("congruent", "congruent", "incongruent", "incongruent"),
	awareness = c("aware", "unaware", "aware", "unaware")
	beta = c(39, 39+103-4, )
	)