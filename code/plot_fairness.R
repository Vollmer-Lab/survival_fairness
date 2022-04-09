library(ggplot2)
library(patchwork)
library(dplyr)
reval <- c(
  "IGS" = "SBS", "IGS_proper" = "RSBS", "ILL" = "ISL", "ILL_proper" = "RISL",
  "RCLL" = "RCLL", "calib_A" = "CalA", "calib_D" = "CalD"
)

res <- read.csv("code/survival_fairness.csv")[, 3:6] %>%
  dplyr::mutate(
    Measure = if_else(Measure %in% names(reval), reval[Measure], Measure)
  )
fres <- res %>% dplyr::mutate(Prop = factor(Prop))

# aggregated over datasets
p_box <- fres %>%
  ggplot(aes(x = Prop, y = Score)) +
  geom_boxplot() +
  geom_smooth(aes(x = Prop * 10 + 1, y = Score), data = res) +
  facet_wrap(vars(Measure), scales = "free_y") +
  labs(x = "σ", y = expression(L[F])) +
  scale_x_discrete(breaks = seq.int(0, 1, 0.2)) +
  theme_bw() +
  theme(legend.position = "n", axis.title = element_text(size = 14),
        strip.background = element_rect(fill = "white"))
ggsave("results/boxplots.png", p_box)

agg_res <- res %>%
  dplyr::group_by(Measure, Prop) %>%
  dplyr::summarise(Score = mean(Score, na.rm = TRUE)) %>%
  dplyr::ungroup()

agg_res %>%
  dplyr::mutate(Score = round(Score, 3)) %>%
  tidyr::pivot_wider(names_from = Prop, values_from = Score) %>%
  dplyr::select(Measure, `0`, `0.2`, `0.4`, `0.6`, `0.8`, `1`) %>%
  stargazer::stargazer(summary = FALSE, rownames = FALSE, out = "results/res.tex")

meas <- unique(res$Measure)
cor_mat <- data.frame(matrix(NA, length(meas), 3, FALSE, list(meas, c("r", "P", "m"))))
cor_mat[, 3] <- meas
for (i in seq_along(meas)) {
  cors <- agg_res %>%
    dplyr::filter(Measure == meas[[i]]) %>%
    dplyr::select(Prop, Score) %>%
    as.matrix() %>%
    Hmisc::rcorr(type = "spearman")
  cor_mat[i, 1] <- cors$r[1, 2]
  cor_mat[i, 2] <- cors$P[1, 2]
}
rm(cors)
cor_mat$P <- p.adjust(cor_mat$P) <= 0.05
cor_mat$P <- ifelse(cor_mat$P, "p <= 0.05", "p > 0.05")

p_cor <- ggplot(cor_mat, aes(x = m, y = r, fill = P)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Measure", y = "Spearman's Correlation") +
  theme_bw() +
  scale_fill_manual(values = c("p <= 0.05" = "#d365bbb7", "p > 0.05" = "white")) +
  guides(fill = guide_legend(ncol = 2)) +
  theme(
    legend.position = c(0.15, 0.05), legend.title = element_blank(),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 14)
  )
ggsave("results/corplots.png", p_cor)

# p_reg <- res %>%
#   ggplot(aes(x = Prop, y = Score)) +
#   geom_smooth() +
#   facet_wrap(vars(Measure), scales = "free_y") +
#   labs(x = "P(Censoring)", y = "Bias") +
#   theme_bw() +
#   theme(legend.position = "n")
# ggsave("results/reg_plots.png", p_reg)

mat <- data.frame(matrix(NA, length(meas), 3, FALSE, list(meas, c("Intercept", "Slope", "p"))))
for (i in seq_along(meas)) {
  s <- summary(lm(Score ~ Prop, res %>% dplyr::filter(Measure == meas[[i]])))
  mat[i, 1] <- coefficients(s)[1, 1]
  mat[i, 2] <- coefficients(s)[2, 1]
  mat[i, 3] <- coefficients(s)[2, 4]
}
mat %>%
  round(3) %>%
  dplyr::mutate(
    p = p.adjust(p),
    Slope = if_else(p <= 0.05, paste0(Slope, "*"), as.character(Slope))
  ) %>%
  dplyr::select(-p) %>%
  stargazer::stargazer(summary = FALSE, out = "results/lm.tex")
