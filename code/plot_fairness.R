library(ggplot2)
library(patchwork)

reshape2::melt(t(ret)) %>%
  #dplyr::filter(grepl("^I", Var2)) %>%
  ggplot(aes(x = Var1, y = value, group = Var2, color = Var2, fill = Var2)) +
  geom_line() +
  facet_wrap(vars(Var2)) +
  labs(x = "P(Censoring)", y = "Bias") +
  theme_bw() +
  theme(legend.position = "n") +

reshape2::melt(t(ret)) %>%
  dplyr::filter(!grepl("^I", Var2)) %>%
  ggplot(aes(x = Var1, y = value, group = Var2, color = Var2, fill = Var2)) +
  geom_line() +
  facet_wrap(vars(Var2), scales = "free") +
  labs(x = "P(Censoring)", y = "Bias") +
  theme_bw() +
  theme(legend.position = "n")
