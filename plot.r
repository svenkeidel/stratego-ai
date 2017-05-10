library(ggplot2)
library(dplyr)
# library(tikzDevice)

desugar_arrows <- "Arrow Desugaring"
check_eval <- "PCF Check & Evaluation"
eval <- "PCF Evaluation"
norm <- "CCA Normalization"

readData <- function(csvFile) {
  data <- read.csv(file=csvFile, header=TRUE, sep=";") %>%
    # Rename function names
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
  return(data)
}

# # plot height of terms depending on the recursion depth
# readData("height.csv") %>%
#   ggplot(aes(x = factor(height), fill = factor(depth))) +
#     geom_bar(position = position_dodge(preserve="single")) +
#     facet_wrap(~fun) +
#     labs(fill = "Depth", x="Term Height", y = element_blank())
# ggsave("height.pdf")

# # plot size of terms depending on the recursion depth
# readData("size.csv") %>%
#   # mutate(depth = factor(depth, levels=rev(levels(factor(depth))))) %>%
#   ggplot(aes(x = factor(size), fill = factor(depth))) +
#     geom_bar(width=0.8, position = position_dodge(preserve="single")) +
#     facet_wrap(~fun) +
#     labs(fill = "Depth", x="Term Size", y = element_blank())
# ggsave("size.pdf")

# # Plot percantages of wittnesses in result set
# readData("wittness.csv") %>%
#   group_by(fun,depth) %>%
#   mutate(wittness = ifelse(wittness=="True", 1, 0)) %>%
#   summarise(
#     n = n(),
#     wittnesses = sum(wittness) / n()) %>%
#   ggplot(aes(depth,wittnesses, label=n)) +
#     geom_bar(stat="identity") +
#     facet_wrap(~fun, scales="free_x", nrow=1) +
#     scale_y_continuous(labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
#     scale_fill_brewer() +
#     geom_text(aes(y = 1.05), size=2) +
#     theme_bw() +
#     labs(x = "Recursion Depth", y=element_blank())
# ggsave("wittness.pdf")


# readData("rule.csv") %>%
#   mutate(depth = factor(depth, levels=rev(levels(factor(depth))))) %>%
#   group_by(fun,depth,ruleId,rule) %>%
#   summarise(invocations = sum(invocation)) %>%
#   ggplot(aes(reorder(rule,invocations), invocations, fill=factor(depth))) +
#     geom_col(position="identity") +
#     facet_grid(fun~. , scales = "free_y") +
#     coord_flip() +
#     labs(fill = "Depth", x = "Rule", y = "Number of Rule Invocations")
# ggsave("rule.pdf")

percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

classification_plot <- function(data, criterion_name, criterion_label) {
  data <- data %>%
    group_by(fun,depth,sample_distance,criterion) %>%
    summarise(
      n = n(),
      true_positive = sum(class == "true_positive"),
      false_positive = sum(class == "false_positive"),
      false_negative = sum(class == "false_negative"),
      precision = true_positive / (true_positive + false_positive),
      recall = true_positive / (true_positive + false_negative),
      f1score = 2 * (precision * recall) / (precision + recall)
    ) %>%
      filter(true_positive + false_positive > 0, true_positive + false_negative > 0)
  
  precision <- data %>% mutate(measure_value = precision) 
  recall <- data %>% mutate(measure_value = recall)
  f1score <- data %>% mutate(measure_value = f1score)
  newdata <- bind_rows("Precision" = precision, "Recall" = recall, "F1 Score" = f1score, .id = "measure") %>%
    mutate(measure = factor(measure, levels=c("Precision", "Recall", "F1 Score")))

  newdata %>%
    ggplot(aes(depth, y = measure_value)) +
      geom_bar(aes(fill = measure, group=measure), stat="identity", position="dodge") +
      facet_grid(criterion~fun, scales="free_x", labeller=labeller(criterion = function(crit) sprintf("%s ≤ %s", criterion_label, crit))) +
      scale_y_continuous(labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
      # scale_fill_brewer() +
      geom_text(aes(y = measure_value / 2, group=measure, label=percent(measure_value)), size=2, position = position_dodge(0.9)) +
      geom_text(
        data = data %>% select(fun,depth,sample_distance,criterion,n,true_positive,false_positive, false_negative),
        aes(y = 1.05, label = sprintf("tp=%d, fp=%d, fn=%d", true_positive, false_positive, false_negative)),
        size = 2
      ) +
      theme_bw() +
      labs(x = "Recursion Depth", y=criterion_label, fill=element_blank())
  ggsave(sprintf("precision_recall_%s.pdf", criterion_name), width=8, height=8, device=cairo_pdf)
}

data <- readData("classification.csv")

dat <- data.frame()
for(h in 3:5) {
  dat <- bind_rows(dat, data %>% filter(height <= h) %>% mutate(criterion = h))
}
classification <- classification_plot(dat, "height", "Height")

dat <- data.frame()
for(s in 3:10) {
  dat <- bind_rows(dat, data %>% filter(size <= s) %>% mutate(criterion = s))
}
classification <- classification_plot(dat, "size", "Size")

dat <- data.frame()
for(s in 3:8) {
  dat <- bind_rows(dat, data %>% filter(distance_sum <= s) %>% mutate(criterion = s))
}
classification <- classification_plot(dat, "distance_sum", "Complexity")
