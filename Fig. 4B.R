library(ggplot2)
library(vegan)
library(permute)
library(lattice)
library(ggrepel)
library(ggExtra)

# 读取NMDS数据文件
df = read.delim("dis_cbm_90.txt",
                header = T,    # 指定第一行是列名
                row.names = 1  # 指定第一列是行名
)
#df=t(df) # 对数据进行转置，如果想对基因分组则不用转置

# 读取样本分组数据文件
dfGroup = read.delim("group.txt",row.names = 1)
dfGroup$ST <- factor(dfGroup$ST,
                     levels=c('BCES', 'RCES', 'OBS', 'ORS'))
dfGroup$Type <- factor(dfGroup$Type,
                       levels=c('BCL', 'BCM', 'BCH', 'RCL', 'RCM', 'RCH', 'OBS', 'ORS'))
dfGroup$Media <- factor(dfGroup$Media,
                        levels=c('T', 'N', 'A', 'R', 'M', 'OBS', 'ORS'))
dfGroup$Concentration <- factor(dfGroup$Concentration,
                                levels=c('1', '0.1', '0.01'))

# NMDS计算
dfNmds<-metaMDS(df,distance="bray",k = 2)
summary(dfNmds)

#应力函数值（<=0.2合理）
dfNmds_stress <- dfNmds$stress
dfNmds_stress
#检查观测值非相似性与排序距离之间的关系——没有点分布在线段较远位置表示该数据可以使用NMDS分析
stressplot(dfNmds)

# 绘图前的数据整理
data = data.frame(dfNmds$points)
data$Type = dfGroup$Type

# 绘图
color=c('#6EA2DB','#45A8AC','#DC8F95','#3751A6','#4daf4a','#e41a1c','black','black')#颜色变量
p1 <- ggplot(data,aes(x = MDS1,y = MDS2,
                      color = Type,
                      group = Type,
                      fill = Type,
                      shape = Type)
)+
  theme_bw()+ #主题设置
  geom_point(size=2)+
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed", size = 0.5, color = 'grey')+
  geom_hline(yintercept = 0,lty="dashed", size = 0.5, color = 'grey')+#图中虚线
  stat_ellipse(             # 添加置信区间
    linetype = 2,
    geom = "polygon",
    level = 0.95,
    alpha=0.3)+
  scale_shape_manual(values = c(19,19,19,17,17,17,4,3))+
  scale_color_manual(values = c('#6EA2DB',
                                '#45A8AC',
                                '#DC8F95',
                                '#3751A6',
                                '#4daf4a',
                                '#ED5F23',
                                'black',
                                'black'))+    #点的颜色设置
  scale_fill_manual(values = color)+#椭圆颜色
  #geom_text_repel(                # 添加文本标签
  #  size=1.5, aes(label = rownames(data), max.overlaps=200),color = "black"
  #)+
  labs(                     # 在副标题处添加stress
    subtitle = paste("stress=",round(dfNmds$stress,3),sep="")
  )+
  theme(axis.title.x=element_text(size=12),#修改X轴标题文本
        axis.title.y=element_text(size=12,angle=90),#修改y轴标题文本
        axis.text.y=element_text(size=10),#修改x轴刻度标签文本
        axis.text.x=element_text(size=10),#修改y轴刻度标签文本
        panel.grid=element_blank())#隐藏网格线
p1

#添加边际图
ggMarginal(p1, type="histogram", size=8, margins="both", groupColour=TRUE, groupFill=TRUE)
#ggMarginal(p, data, x, y, type = c("density", "histogram", "boxplot", "violin", "densigram"), margins = c("both", "x", "y"), size = 5, ..., xparams = list(), yparams = list(), groupColour = FALSE, groupFill = FALSE)
#  type = c("density", "histogram", "boxplot", "violin", "densigram")