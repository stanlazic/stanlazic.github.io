library(ashr)


## generate data

## set random seed for reproducibility
set.seed(123)

## set sample szie and number of tests
N_tests <- 100
sample_size <- 20

## generate null data
y_null <- matrix(rnorm(N_tests * sample_size),
                 nrow = N_tests, ncol = sample_size)

## copy null data
y_effect <- y_null

## for 5 genes, add an effect of 1.5
y_effect[1:5, 1:10] <- rnorm(5 * 10, 1.5, 1)

## make grouping variable
group <- gl(n = 2, k = 10, labels = c("Control", "Treated"))

## vectors to store results
est_null <- numeric(N_tests)
se_null <- numeric(N_tests)
p_null <- numeric(N_tests)

est_effect <- numeric(N_tests)
se_effect <- numeric(N_tests)
p_effect <- numeric(N_tests)

## perform analysis (t-test run as a linear model)
for (i in 1:N_tests) {
  m_null <- summary(lm(y_null[i, ] ~ group))
  est_null[i] <- m_null$coefficients["groupTreated", "Estimate"]
  se_null[i] <- m_null$coefficients["groupTreated", "Std. Error"]
  p_null[i] <- m_null$coefficients["groupTreated", "Pr(>|t|)"]

  m_effect <- summary(lm(y_effect[i, ] ~ group))
  est_effect[i] <- m_effect$coefficients["groupTreated", "Estimate"]
  se_effect[i] <- m_effect$coefficients["groupTreated", "Std. Error"]
  p_effect[i] <- m_effect$coefficients["groupTreated", "Pr(>|t|)"]
}


## fit ashr model
res_null <- ash(est_null, se_null)
res_effect <- ash(est_effect, se_effect)


## plot original and shrunken estimates
pdf("Fig1.pdf", height = 8, width = 10)
par(
  mfrow = c(2, 2),
  las = 1,
  pch = 16,
  cex = 1,
  mar = c(5, 5, 4, 1)
)

## variable to order the genes
ord <- order(est_null)

## plot null effects
plot(sort(est_null),
  ylim = c(-2.5, 2), ylab = "Estimate", xlab = "Sorted genes",
  main = "No true effects", cex = 0.7
)

## add reference line
abline(h = 0, lty = 2)

## add error bars
segments(
  1:N_tests, est_null[ord] + se_null[ord],
  1:N_tests, est_null[ord] - se_null[ord]
)
mtext("A", side = 3, line = 2, cex = 1.5, adj = 0, font = 2)

## variable to order the genes
ord2 <- order(res_null$result$PosteriorMean)

## plot shrunken null effects
plot(sort(res_null$result$PosteriorMean),
  ylim = c(-2.5, 2),
  ylab = "Shrunken estimate", xlab = "Sorted genes", main = "No true effects", cex = 0.7
)

## add reference line
abline(h = 0, lty = 2)

## add error bars
segments(
  1:N_tests, res_null$result$PosteriorMean[ord2] + res_null$result$PosteriorSD[ord2],
  1:N_tests, res_null$result$PosteriorMean[ord2] - res_null$result$PosteriorSD[ord2]
)
mtext("B", side = 3, line = 2, cex = 1.5, adj = 0, font = 2)


## variable to order the genes
ord <- order(est_effect)

## plot effects
plot(sort(est_effect),
  ylim = c(-2.5, 2), ylab = "Estimate", xlab = "Sorted genes",
  main = "Five true effects", cex = 0.7
)

## add reference line
abline(h = 0, lty = 2)

## add error bars
segments(
  1:N_tests, est_effect[ord] + se_effect[ord],
  1:N_tests, est_effect[ord] - se_effect[ord]
)
mtext("C", side = 3, line = 2, cex = 1.5, adj = 0, font = 2)


## variable to order the genes
ord2 <- order(res_effect$result$PosteriorMean)

## plot shrunken effects
plot(sort(res_effect$result$PosteriorMean),
  ylim = c(-2.5, 2),
  ylab = "Shrunken estimate", xlab = "Sorted genes", main = "Five true effects", cex = 0.7
)

## add reference line
abline(h = 0, lty = 2)

## add error bars
segments(
  1:N_tests, res_effect$result$PosteriorMean[ord2] + res_effect$result$PosteriorSD[ord2],
  1:N_tests, res_effect$result$PosteriorMean[ord2] - res_effect$result$PosteriorSD[ord2]
)
mtext("D", side = 3, line = 2, cex = 1.5, adj = 0, font = 2)


dev.off()
