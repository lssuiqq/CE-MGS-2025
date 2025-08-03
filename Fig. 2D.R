# 加载必要的包
if (!require("ComplexHeatmap")) install.packages("ComplexHeatmap")
if (!require("circlize")) install.packages("circlize")
library(ComplexHeatmap)
library(circlize)

# 示例数据加载
data <- read.csv("mag_phylum.csv")

# 提取需要可视化的数据部分（包括OBS列）
data_matrix <- as.matrix(data[, 2:ncol(data)])

# 将Phylum作为行名
rownames(data_matrix) <- data$Phylum

# 将数据格式化为没有小数点的字符串
formatted_data_matrix <- format(round(data_matrix, 0), nsmall = 0)

# 将0值替换为空白字符串
formatted_data_matrix[data_matrix == 0] <- ""

# 计算每一行和每一列的总和
row_sums <- rowSums(data_matrix)
col_sums <- colSums(data_matrix)

# 创建颜色调色板
color_palette <- colorRampPalette(c("white", "#66C2A5"))(99)
colors <- c("#CCCCCC", color_palette)

# 设置Phylum的颜色对应关系
phylum_colors <- c(
  "Actinomycetota" = "#6A669E",
  "Thermoproteota" = "#C82320",
  "Pseudomonadota" = "#3573A3",
  "Bacillota" = "#228F6F",
  "Chloroflexota" = "#A5C868",
  "Acidobacteriota" = "#D77AA5",
  "Gemmatimonadota" = "#E27820",
  "Desulfobacterota_B" = "#9B7225",
  "Patescibacteria" = "#B4A6C9",
  "Bacteroidota" = "#EBC940",
  "Desulfobacterota_D" = "#5EB7BF"
)

# 根据Phylum的颜色设置右侧行总和的柱状图
row_barplot <- rowAnnotation(
  Sum = anno_barplot(
    row_sums, 
    gp = gpar(fill = phylum_colors[rownames(data_matrix)]), 
    border = FALSE, 
    axis_param = list(gp = gpar(fontsize = 8))
  ),
  width = unit(2, "cm")             
)

# 设置列名称的颜色对应关系
column_colors <- c(
  "OBS" = "#e9b383",
  "ORS" = "#7c9d97",
  "B_15" = "#6EA2DB",
  "B_30" = "#45A8AC",
  "B_45" = "#DC8F95",
  "R_15" = "#3751A6",
  "R_30" = "#4daf4a",
  "R_45" = "#ED5F23"
)

# 创建热图上方的柱状图注释
col_barplot <- HeatmapAnnotation(
  Sum = anno_barplot(
    col_sums, 
    gp = gpar(fill = column_colors[colnames(data_matrix)]), 
    border = FALSE,
    axis_param = list(gp = gpar(fontsize = 8))
  ),
  height = unit(1.5, "cm")  # 设置柱状图的高度
)

# 生成热图主体
heatmap <- Heatmap(data_matrix, 
                   name = "Values", 
                   col = colorRamp2(c(0, max(data_matrix)), c("white", "#66C2A5")),
                   cluster_rows = FALSE, 
                   cluster_columns = FALSE,
                   show_row_names = FALSE,    # 关闭默认的行名显示
                   show_column_names = TRUE, 
                   column_names_gp = gpar(fontsize = 8, rot = 90),
                   rect_gp = gpar(col = "white", lwd = 0.5),  # 每个格子白色边框
                   top_annotation = col_barplot,  # 添加顶部柱状图
                   cell_fun = function(j, i, x, y, width, height, fill) {
                     grid.text(formatted_data_matrix[i, j], x, y, gp = gpar(fontsize = 7))
                   })

# 组合热图、左侧Phylum注释和右侧柱状图
phylum_annotation <- rowAnnotation(Phylum = anno_text(rownames(data_matrix), 
                                                      gp = gpar(fontsize = 8)))

ht_list <- phylum_annotation + heatmap + row_barplot

# 绘制热图
draw(ht_list)

# 在热图矩阵外围添加黑色边框
decorate_heatmap_body("Values", {
  grid.rect(gp = gpar(col = "black", lwd = 1, fill = NA))
})
