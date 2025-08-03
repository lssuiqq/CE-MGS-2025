# Load necessary libraries
library(ggplot2)
library(reshape2)

# Create a data frame from your data
data <- data.frame(
  Topological_features = c("Average degree", "Network diameter", "Graph density", "Modularity", "Average clustering coefficient", "Average path length"),
  B_15 = c(9.675, 12, 0.083, 0.748, 0.807, 4.933),
  B_30 = c(16.267, 6, 0.121, 0.788, 0.814, 2.061),
  B_45 = c(5.029, 6, 0.073, 0.785, 0.853, 2.311),
  R_15 = c(52.983, 6, 0.304, 0.162, 0.788, 2.045),
  R_30 = c(22.918, 8, 0.119, 0.653, 0.748, 2.743),
  R_45 = c(5.842, 12, 0.078, 0.852, 0.785, 4.571)
)

# Reshape the data to long format for plotting
data_long <- melt(data, id.vars = "Topological_features", variable.name = "Group", value.name = "Value")

# Set the factor levels for Topological_features to ensure the desired order
data_long$Topological_features <- factor(data_long$Topological_features, levels = c("Average degree", "Network diameter", "Graph density", "Modularity", "Average clustering coefficient", "Average path length"))

# Create the faceted bar plot
ggplot(data_long, aes(x = Group, y = Value, fill = Group)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Topological_features, nrow = 1, scales = "free_y") +  # 6张图放在1行
  theme_grey() +
  labs(x = "Groups", y = "Value") +  # 去掉大标题
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_blank(),  # 去掉大标题
        legend.position = "none",       # 不显示图例
        panel.spacing = unit(1, "lines")) +  # 调整图间距
  scale_fill_manual(values = c("B_15" = "#6EA2DB", "B_30" = "#45A8AC", "B_45" = "#DC8F95", 
                               "R_15" = "#3751A6", "R_30" = "#4daf4a", "R_45" = "#ED5F23"))
