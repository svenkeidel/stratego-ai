library(ggplot2)
library(dplyr)
# library(tikzDevice)

desugar_arrows <- "Arrow Desugaring"
check_eval <- "PCF Check & Evaluation"
eval <- "PCF Evaluation"
norm <- "CCA Normalization"

# # Read data from csv file
# data <- read.csv(file="casestudy.csv", header=TRUE, sep=",") %>%
#   mutate(wittness = ifelse(wittness=="True", 1, 0)) %>%
#   # Rename function names
#   mutate(fun = recode(fun,
#     "check_0_0" = "check",
#     "check_eval_0_0" = check_eval,
#     "check_num_0_0" = "check_num",
#     "desugar_arrow_0_0" = desugar_arrows,
#     "eval_0_0" = eval,
#     "norm_0_0" = norm
#   )) %>%
#   # filter relevent experiments
#   filter(fun %in% c(check_eval,eval,desugar_arrows,norm))

# # reorder experiments
# data$fun <- factor(data$fun, levels=c(desugar_arrows, norm, eval, check_eval))
# data$depth <- factor(data$depth)
# data$depth <- factor(data$depth, levels=rev(levels(factor(data$depth))))

# # plot height of terms depending on the recursion depth
# # tikz(file = "height.tex", standAlone=F,width = 6, height = 3, sanitize=TRUE)
# ggplot(data, aes(height, fill = factor(depth))) +
#   geom_histogram(binwidth = 1) +
#   scale_y_sqrt() +
#   facet_wrap(~fun,scales="free") +
#   labs(fill = "depth", x="Term Height", y = element_blank())
# # dev.off()
# ggsave("height.pdf")

# reverseDepth <- function(data) {
#   dat <- data
#   dat$depth <- factor(data$depth, levels=rev(levels(factor(data$depth))))
#   return(dat)
# }

# # plot size of terms depending on the recursion depth
# plot <- ggplot(data, aes(size, fill = factor(depth))) +
#   geom_histogram(data=subset(data, fun==check_eval), binwidth = 1, position="dodge") +
#   geom_histogram(data=subset(data, fun==eval), binwidth = 1, position="dodge") +
#   geom_histogram(data=subset(reverseDepth(data), fun==desugar_arrows), binwidth = 10, position="identity") +
#   geom_histogram(data=subset(reverseDepth(data), fun==norm), binwidth = 10, position="identity") +
#   facet_wrap(~fun,scales="free") +
#   # scale_fill_grey(start = 0.2, end=0.75) +
#   scale_fill_brewer(palette="Blues") +
#   scale_y_sqrt() +
#   theme_bw() +
#   labs(fill = "Recursion\nDepth", x="Term Size", y = "Count")
# # tikz(file = "size.tex", standAlone=F,width = 6, height = 4, sanitize=TRUE)
# # print(plot)
# # dev.off()
# print(plot)
# ggsave("size.pdf")

# data <- data %>%
#   group_by(fun,depth) %>%
#   summarise(
#     n = n(),
#     wittnesses = sum(wittness) / n())

# # Plot percantages of wittnisses in result set
# plot <- ggplot(data, aes(depth,wittnesses, label=n)) +
#   geom_bar(stat="identity") +
#   facet_wrap(~fun, scales="free_x", nrow=1) +
#   scale_y_continuous(labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
#   scale_fill_brewer() +
#   geom_text(aes(y = 1.05), size=2) +
#   theme_bw() +
#   labs(x = "Recursion Depth", y=element_blank())
# # tikz(file = "wittnesses.tex", standAlone=F, width = 6, height = 2, sanitize=TRUE)
# # print(plot)
# # dev.off()
# print(plot)
# ggsave("wittnesses.pdf")


# Load other data set
data <- read.csv(file="grammar.csv", header=TRUE, sep=";") %>%
  mutate(fun = recode(fun,
    "check_0_0" = "check",
    "check_eval_0_0" = check_eval,
    "check_num_0_0" = "check_num",
    "desugar_arrow_0_0" = desugar_arrows,
    "eval_0_0" = eval,
    "norm_0_0" = norm
  )) %>%
  # filter relevent experiments
  filter(fun %in% c(check_eval,eval,desugar_arrows,norm))

plot <- ggplot(data, aes(rule, invocation)) +
  geom_boxplot() +
  facet_grid(depth~fun, scales="free") +
  coord_flip() +
  labs(x = "Rule", y = "Number of Rule Invocations")
print(plot)
ggsave("grammar_boxplot.pdf")

data <- data %>%
  group_by(fun,depth,ruleId,rule) %>%
  summarise(invocations = sum(invocation))

plot <- ggplot(data, aes(reorder(rule,invocations), invocations, fill=depth)) +
  geom_col() +
  facet_grid(fun~. , scales = "free_y") +
  coord_flip() +
  labs(x = "Rule", y = "Number of Rule Invocations")
print(plot)
ggsave("grammar_bar.pdf")