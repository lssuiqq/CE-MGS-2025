# 加载必要的库
library(vegan)
library(ggplot2)
library(ggrepel)
library(dplyr)

# Step 1: 加载 go 数据
go_data <- read.delim("go.txt", row.names = 1, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE, header = TRUE)

# Step 2: 加载 Mash 数据
mash_distances <- read.delim("mash_distances-1.tsv", sep = "\t", header = TRUE)
mash_matrix <- reshape2::dcast(mash_distances, Genome1 ~ Genome2, value.var = "Distance", fill = NA)
rownames(mash_matrix) <- mash_matrix$Genome1
mash_matrix <- mash_matrix[ , -1]  # 去掉第一列以生成对称矩阵
mash_matrix <- as.matrix(mash_matrix)

# Step 3: 确保 Mash 和 go 数据中的基因组顺序一致
shared_genomes <- intersect(rownames(go_data), rownames(mash_matrix))
go_data <- go_data[shared_genomes, ]
mash_matrix <- mash_matrix[shared_genomes, shared_genomes]

# Step 4: 计算 go 距离矩阵
go_dist <- vegdist(go_data, method = "jaccard", binary = TRUE)

# Step 5: 运行 Procrustes 分析
procrustes_result <- protest(go_dist, as.dist(mash_matrix), permutations = 999)

# 打印 Procrustes 分析的 p 值和 m2 值
cat("Procrustes analysis p-value:", procrustes_result$signif, "\n")
cat("Procrustes analysis m2 value:", procrustes_result$ss, "\n")

# Step 6: 提取 Procrustes 分析结果
go_points <- as.data.frame(procrustes_result$Yrot)
go_points$Genome <- rownames(go_points)
colnames(go_points)[1:2] <- c("Dimension1_function", "Dimension2_function")

phylogeny_points <- as.data.frame(procrustes_result$X)
phylogeny_points$Genome <- rownames(phylogeny_points)
colnames(phylogeny_points)[1:2] <- c("Dimension1_phylogeny", "Dimension2_phylogeny")

# 合并数据用于可视化
combined_data <- merge(go_points, phylogeny_points, by = "Genome")

# Step 7: 加载 group 信息
group_data <- read.delim("group.txt", sep = "\t", header = TRUE)

# 检查是否正确加载
head(group_data)

# 将 group 信息与 combined_data 合并
# `Sample` 对应的是 `Genome` 列
combined_data <- merge(combined_data, group_data, by.x = "Genome", by.y = "Sample")

# 定义分类和颜色
phylum_order <- c('p__Actinomycetota', 'p__Bacillota', 'p__Pseudomonadota', 'p__Bacteroidota',
                  'p__Thermoproteota', 'p__Chloroflexota', 'p__Acidobacteriota', 'p__Nitrospirota',
                  'p__Gemmatimonadota', 'p__Desulfobacterota_B', 'p__Patescibacteria', 'p__Desulfobacterota_D')
phylum_colors <- c('#656396', '#22896B', '#346E9A', '#E1C144', '#BD2621', '#9FC067',
                   '#CB759C', '#D87524', '#967026', '#B73172', '#AFA3C4', '#5BAEB5')

# 确保分类顺序一致
combined_data$Phylum <- factor(combined_data$Phylum, levels = phylum_order)

# 检查合并后的数据结构
head(combined_data)

# Step 8: 可视化
ggplot() +
  # 用于功能点 (圆形)
  geom_point(data = combined_data, 
             aes(x = Dimension1_function, y = Dimension2_function, color = Phylum), 
             shape = 18, size = 2.5, alpha = 0.75) +
  # 用于系统发育点 (三角形)
  geom_point(data = combined_data, 
             aes(x = Dimension1_phylogeny, y = Dimension2_phylogeny, color = Phylum), 
             shape = 5, size = 1.5, alpha = 0.75) +
  # 连线
  geom_segment(data = combined_data, 
               aes(x = Dimension1_function, y = Dimension2_function, 
                   xend = Dimension1_phylogeny, yend = Dimension2_phylogeny, color = Phylum),
               alpha = 0.5, linetype = "solid") +
  # 手动设置颜色
  scale_color_manual(values = phylum_colors) +
  labs(x = "Dimension1",
       y = "Dimension2") +
  theme_test() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# 6 , 4