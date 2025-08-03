library(vegan)
library(permute)
library(lattice)
library(ggrepel)
data <- read.delim(file = "go.txt", row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, header = T)
cmic_group <- read.delim(file = "group-go.txt")
cmic_group$ST <- factor(cmic_group$ST, levels=c('B-15', 'B-30', 'B-45', 'R-15', 'R-30', 'R-45'))
cmic_group$Phylum <- factor(cmic_group$Phylum, levels=c('p__Actinomycetota', 'p__Bacillota',
                                                        'p__Pseudomonadota', 'p__Bacteroidota'))
dim(data)
head(data)

cmic_dist <- vegdist(data, method="jaccard", binary=T) #bray jaccard

cmic_pcoa <- cmdscale(cmic_dist, k=3, eig=T)

cmic_pcoa_points <- as.data.frame(cmic_pcoa$points)
sum_eig <- sum(cmic_pcoa$eig)
eig_percent <- round(cmic_pcoa$eig/sum_eig*100,1)

colnames(cmic_pcoa_points) <- paste0("PCoA", 1:3)

cmic_pcoa_result <- cbind(cmic_pcoa_points, cmic_group)

head(cmic_pcoa_result)

library(ggplot2)
color=c('#656396', '#22896B', '#346E9A', '#E1C144')#颜色变量
p1 <- ggplot(cmic_pcoa_result, aes(x=PCoA1, y=PCoA2, color=Phylum, fill=Phylum)) + #shape=ST
  labs(x=paste("PCoA axis1 (", eig_percent[1], "%)", sep=""),
       y=paste("PCoA axis2 (", eig_percent[2], "%)", sep="")) +
  geom_point(size=1.5, alpha=0.75) +
  stat_ellipse(aes(group=Phylum), # 椭圆仅用线型区分,, linetype=Phylum
               linetype = 2,
               geom = "polygon",
               level=0.95,alpha=0.3) +    # 设置椭圆线宽
  scale_fill_manual(values = color)+#椭圆颜色
  #scale_shape_manual(values = c(0, 1, 2, 7, 10, 14, 3, 4)) +
  scale_color_manual(values = c('#656396', '#22896B', '#346E9A', '#E1C144')) +
  #scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "dashed", "dotted")) +
  #geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 0.5) +  # 添加 y=0 的虚线
  #geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 0.5) +  # 添加 x=0 的虚线
  theme_test() +
  theme(axis.text.x = element_text(color="black", size=6.5), 
        axis.text.y = element_text(color="black", size=6.5),
        axis.title = element_text(color="black", face="bold", size=7),
        legend.title = element_blank(), 
        legend.text = element_text(color="black", size=6.5),
        legend.key.size = unit(0.2, "inches"))

p1

#添加边际图
#library(ggExtra)
#ggMarginal(p1, type="histogram", size=8, margins="both", groupColour=TRUE, groupFill=TRUE)
#ggMarginal(p, data, x, y, type = c("density", "histogram", "boxplot", "violin", "densigram"), margins = c("both", "x", "y"), size = 5, ..., xparams = list(), yparams = list(), groupColour = FALSE, groupFill = FALSE)
#  type = c("density", "histogram", "boxplot", "violin", "densigram")

# 4.5   3.2