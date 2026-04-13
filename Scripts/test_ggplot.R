library(ggplot2)
library(dplyr)

summary_table <- data.frame(
  Industry = rep(paste("Ind", 1:5), 2),
  Term = rep(c("GDP Change %", "Log(Revenue)"), each=5),
  Coefficient = c(runif(5, -0.05, 0.05), runif(5, 0.5, 1.0)),
  Std_Error = c(runif(5, 0.01, 0.02), runif(5, 0.1, 0.2)),
  Significant = "p < 0.05"
) |> mutate(
  CI_lower = Coefficient - 1.96 * Std_Error,
  CI_upper = Coefficient + 1.96 * Std_Error
)

p1 <- ggplot(summary_table, aes(x = Industry, y = Coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.3) +
  facet_wrap(~ Term, scales = "free_x") +
  coord_flip()

# Is p1 visually constrained? If x is free but not y, then y is shared. Since y is Coefficient, it's shared!
ggsave("Outputs/test_p1.png", p1)

p2 <- ggplot(summary_table, aes(x = Industry, y = Coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.3) +
  facet_wrap(~ Term, scales = "free") +
  coord_flip()

ggsave("Outputs/test_p2.png", p2)
