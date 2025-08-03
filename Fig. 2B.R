library(ggplot2)
library(patchwork)
library(gghalves)
require(tidyverse)
require(ggpubr)
require(reshape2) 

data <- read.delim("genome_quality.txt",header=T)
head(data)
data$Group <- as.factor(data$Group) #指定因子
data$Group <- factor(data$Group, levels=c("B_15","B_30","B_45","R_15","R_30","R_45","OBS","ORS"))#设置顺序

#绘图开始，将数据与图形建立映射，存到变量p,（使用Species及Sepal.Length为例来进行绘制）：
p1a<-ggplot(data,aes(x=Group,
                   y=Completeness,
                   fill=Group,
                   color=Group))
p1a

#自定义颜色
mycolor <- c("#6EA2DB","#45A8AC","#DC8F95","#3751A6","#4daf4a","#ED5F23","#7c9d97","#e9b383")
p1b <- p1a + scale_color_manual(values=mycolor)+scale_fill_manual(values=mycolor)
p1b

#先画一半小提琴图(geom_half_violin)，得到p1：
p1c <- p1b + geom_half_violin()
p1c

#一半小提琴图的参数调整：
#position：位置调整，这里将其向右水平移动0.1；
#side：显示哪一侧， "I"代表左侧，"R"代表右侧，默认"I"；
#adjust：调整带宽，这里设为1.2使宽带略变平滑；
#trim：小提琴图尾部的数据修整，默认为"T",表示将尾部修整到数据范围；"F"表示不修剪尾部；
p1d <- p1b + geom_half_violin(position=position_nudge(x=0.1,y=0),
                        side='R',adjust=1.2,color=NA,alpha=0.7) #trim=F,
p1d

#在半个小提琴图左侧加上散点图，得到p3：
p1e <- p1d + geom_point()
p1e

#调整散点，得到p4：
p1f <- p1d+geom_point(aes(x = as.numeric(Group)-0.1,#散点位置向左平移0.1
                    y = Completeness,
                    color = Group),
                  position = position_jitter(width =0.03),size =0.2, shape = 20)#调整散点，使取值相同的原重合散点分散开
p1f

#在p4散点和二分之一小提琴图中间添加箱线图，得到p5：
p1g <- p1f + geom_boxplot(outlier.shape = NA, #NA隐藏离群点；
                    width =0.1,
                    alpha=0.7)
p1g

#将图形翻转
#p1h <- p1g + coord_flip()
#p1h

#去掉灰底
p1i <- p1g + theme_classic()+
  theme(legend.position="none") + 
  theme(text = element_text(size=15)) + 
  #ylim(0.0,1.3)+
  ylab("Completeness (%)")+xlab("Sample")+
  theme(axis.text.x=element_text(vjust=1,size=10,color = "black"))+
  theme(axis.text.y=element_text(vjust=0.5,size=10,color = "black"))+
  theme(axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 0.5),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 0.5))
p1i


#绘图开始，将数据与图形建立映射，存到变量p,（使用Species及Sepal.Length为例来进行绘制）：
p2a<-ggplot(data,aes(x=Group,
                     y=Contamination,
                     fill=Group,
                     color=Group))
p2a

#自定义颜色
mycolor <- c("#6EA2DB","#45A8AC","#DC8F95","#3751A6","#4daf4a","#ED5F23","#7c9d97","#e9b383")
p2b <- p2a + scale_color_manual(values=mycolor)+scale_fill_manual(values=mycolor)
p2b

#先画一半小提琴图(geom_half_violin)，得到p1：
p2c <- p2b + geom_half_violin()
p2c

#一半小提琴图的参数调整：
#position：位置调整，这里将其向右水平移动0.1；
#side：显示哪一侧， "I"代表左侧，"R"代表右侧，默认"I"；
#adjust：调整带宽，这里设为1.2使宽带略变平滑；
#trim：小提琴图尾部的数据修整，默认为"T",表示将尾部修整到数据范围；"F"表示不修剪尾部；
p2d <- p2b + geom_half_violin(position=position_nudge(x=0.1,y=0),
                              side='R',adjust=1.2,color=NA,alpha=0.7)
p2d

#在半个小提琴图左侧加上散点图，得到p3：
p2e <- p2d + geom_point()
p2e

#调整散点，得到p4：
p2f <- p2d+geom_point(aes(x = as.numeric(Group)-0.1,#散点位置向左平移0.1
                          y = Contamination,
                          color = Group),
                      position = position_jitter(width =0.03),size =0.2, shape = 20)#调整散点，使取值相同的原重合散点分散开
p2f

#在p4散点和二分之一小提琴图中间添加箱线图，得到p5：
p2g <- p2f + geom_boxplot(outlier.shape = NA, #NA隐藏离群点；
                          width =0.1,
                          alpha=0.7)
p2g

#将图形翻转
#p2h <- p2g + coord_flip()
#p2h

#去掉灰底
p2i <- p2g + theme_classic()+
  theme(legend.position="none") + 
  theme(text = element_text(size=15)) + 
  #ylim(0.0,1.3)+
  ylab("Contamination (%)")+xlab("Sample")+
  theme(axis.text.x=element_text(vjust=1,size=10,color = "black"))+
  theme(axis.text.y=element_text(vjust=0.5,size=10,color = "black"))+
  theme(axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 0.5),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 0.5))
p2i

#绘图开始，将数据与图形建立映射，存到变量p,（使用Species及Sepal.Length为例来进行绘制）：
p3a<-ggplot(data,aes(x=Group,
                     y=Strain_heterogeneity,
                     fill=Group,
                     color=Group))
p3a

#自定义颜色
mycolor <- c("#6EA2DB","#45A8AC","#DC8F95","#3751A6","#4daf4a","#ED5F23","#7c9d97","#e9b383")
p3b <- p3a + scale_color_manual(values=mycolor)+scale_fill_manual(values=mycolor)
p3b

#先画一半小提琴图(geom_half_violin)，得到p1：
p3c <- p3b + geom_half_violin()
p3c

#一半小提琴图的参数调整：
#position：位置调整，这里将其向右水平移动0.1；
#side：显示哪一侧， "I"代表左侧，"R"代表右侧，默认"I"；
#adjust：调整带宽，这里设为1.2使宽带略变平滑；
#trim：小提琴图尾部的数据修整，默认为"T",表示将尾部修整到数据范围；"F"表示不修剪尾部；
p3d <- p3b + geom_half_violin(position=position_nudge(x=0.1,y=0),
                              side='R',adjust=1.2,color=NA,alpha=0.7)
p3d

#在半个小提琴图左侧加上散点图，得到p3：
p3e <- p3d + geom_point()
p3e

#调整散点，得到p4：
p3f <- p3d+geom_point(aes(x = as.numeric(Group)-0.1,#散点位置向左平移0.1
                          y = Strain_heterogeneity,
                          color = Group),
                      position = position_jitter(width =0.03),size =0.2, shape = 20)#调整散点，使取值相同的原重合散点分散开
p3f

#在p4散点和二分之一小提琴图中间添加箱线图，得到p5：
p3g <- p3f + geom_boxplot(outlier.shape = NA, #NA隐藏离群点；
                          width =0.1,
                          alpha=0.7)
p3g

#将图形翻转
#p3h <- p3g + coord_flip()
#p3h

#去掉灰底
p3i <- p3g + theme_classic()+
  theme(legend.position="none") + 
  theme(text = element_text(size=15)) + 
  #ylim(0.0,1.3)+
  ylab("Strain heterogeneity (%)")+xlab("Sample")+
  theme(axis.text.x=element_text(vjust=1,size=10,color = "black"))+
  theme(axis.text.y=element_text(vjust=0.5,size=10,color = "black"))+
  theme(axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 0.5),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 0.5))
p3i

#绘图开始，将数据与图形建立映射，存到变量p,（使用Species及Sepal.Length为例来进行绘制）：
p4a<-ggplot(data,aes(x=Group,
                     y=Genome_size,
                     fill=Group,
                     color=Group))
p4a

#自定义颜色
mycolor <- c("#6EA2DB","#45A8AC","#DC8F95","#3751A6","#4daf4a","#ED5F23","#7c9d97","#e9b383")
p4b <- p4a + scale_color_manual(values=mycolor)+scale_fill_manual(values=mycolor)
p4b

#先画一半小提琴图(geom_half_violin)，得到p1：
p4c <- p4b + geom_half_violin()
p4c

#一半小提琴图的参数调整：
#position：位置调整，这里将其向右水平移动0.1；
#side：显示哪一侧， "I"代表左侧，"R"代表右侧，默认"I"；
#adjust：调整带宽，这里设为1.2使宽带略变平滑；
#trim：小提琴图尾部的数据修整，默认为"T",表示将尾部修整到数据范围；"F"表示不修剪尾部；
p4d <- p4b + geom_half_violin(position=position_nudge(x=0.1,y=0),
                              side='R',adjust=1.2,color=NA,alpha=0.7)
p4d

#在半个小提琴图左侧加上散点图，得到p4：
p4e <- p4d + geom_point()
p4e

#调整散点，得到p4：
p4f <- p4d+geom_point(aes(x = as.numeric(Group)-0.1,#散点位置向左平移0.1
                          y = Genome_size,
                          color = Group),
                      position = position_jitter(width =0.03),size =0.2, shape = 20)#调整散点，使取值相同的原重合散点分散开
p4f

#在p4散点和二分之一小提琴图中间添加箱线图，得到p5：
p4g <- p4f + geom_boxplot(outlier.shape = NA, #NA隐藏离群点；
                          width =0.1,
                          alpha=0.7)
p4g

#将图形翻转
#p4h <- p4g + coord_flip()
#p4h

#去掉灰底
p4i <- p4g + theme_classic()+
  theme(legend.position="none") + 
  theme(text = element_text(size=15)) + 
  #ylim(0.0,1.3)+
  ylab("Genome size (Mb)")+xlab("Sample")+
  theme(axis.text.x=element_text(vjust=1,size=10,color = "black"))+
  theme(axis.text.y=element_text(vjust=0.5,size=10,color = "black"))+
  theme(axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 0.5),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 0.5))
p4i

#绘图开始，将数据与图形建立映射，存到变量p,（使用Species及Sepal.Length为例来进行绘制）：
p5a<-ggplot(data,aes(x=Group,
                     y=GC,
                     fill=Group,
                     color=Group))
p5a

#自定义颜色
mycolor <- c("#6EA2DB","#45A8AC","#DC8F95","#3751A6","#4daf4a","#ED5F23","#7c9d97","#e9b383")
p5b <- p5a + scale_color_manual(values=mycolor)+scale_fill_manual(values=mycolor)
p5b

#先画一半小提琴图(geom_half_violin)，得到p1：
p5c <- p5b + geom_half_violin()
p5c

#一半小提琴图的参数调整：
#position：位置调整，这里将其向右水平移动0.1；
#side：显示哪一侧， "I"代表左侧，"R"代表右侧，默认"I"；
#adjust：调整带宽，这里设为1.2使宽带略变平滑；
#trim：小提琴图尾部的数据修整，默认为"T",表示将尾部修整到数据范围；"F"表示不修剪尾部；
p5d <- p5b + geom_half_violin(position=position_nudge(x=0.1,y=0),
                              side='R',adjust=1.2,color=NA,alpha=0.7)
p5d

#在半个小提琴图左侧加上散点图，得到p5：
p5e <- p5d + geom_point()
p5e

#调整散点，得到p5：
p5f <- p5d+geom_point(aes(x = as.numeric(Group)-0.1,#散点位置向左平移0.1
                          y = GC,
                          color = Group),
                      position = position_jitter(width =0.03),size =0.2, shape = 20)#调整散点，使取值相同的原重合散点分散开
p5f

#在p5散点和二分之一小提琴图中间添加箱线图，得到p5：
p5g <- p5f + geom_boxplot(outlier.shape = NA, #NA隐藏离群点；
                          width =0.1,
                          alpha=0.7)
p5g

#将图形翻转
#p5h <- p5g + coord_flip()
#p5h

#去掉灰底
p5i <- p5g + theme_classic()+
  theme(legend.position="none") + 
  theme(text = element_text(size=15)) + 
  #ylim(0.0,1.3)+
  ylab("GC content (%)")+xlab("Sample")+
  theme(axis.text.x=element_text(vjust=1,size=10,color = "black"))+
  theme(axis.text.y=element_text(vjust=0.5,size=10,color = "black"))+
  theme(axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 0.5),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 0.5))
p5i


#绘图开始，将数据与图形建立映射，存到变量p,（使用Species及Sepal.Length为例来进行绘制）：
p6a<-ggplot(data,aes(x=Group,
                     y=Contig_N50,
                     fill=Group,
                     color=Group))
p6a

#自定义颜色
mycolor <- c("#6EA2DB","#45A8AC","#DC8F95","#3751A6","#4daf4a","#ED5F23","#7c9d97","#e9b383")
p6b <- p6a + scale_color_manual(values=mycolor)+scale_fill_manual(values=mycolor)
p6b

#先画一半小提琴图(geom_half_violin)，得到p1：
p6c <- p6b + geom_half_violin()
p6c

#一半小提琴图的参数调整：
#position：位置调整，这里将其向右水平移动0.1；
#side：显示哪一侧， "I"代表左侧，"R"代表右侧，默认"I"；
#adjust：调整带宽，这里设为1.2使宽带略变平滑；
#trim：小提琴图尾部的数据修整，默认为"T",表示将尾部修整到数据范围；"F"表示不修剪尾部；
p6d <- p6b + geom_half_violin(position=position_nudge(x=0.1,y=0),
                              side='R',adjust=1.2,color=NA,alpha=0.7)
p6d

#在半个小提琴图左侧加上散点图，得到p6：
p6e <- p6d + geom_point()
p6e

#调整散点，得到p6：
p6f <- p6d+geom_point(aes(x = as.numeric(Group)-0.1,#散点位置向左平移0.1
                          y = Contig_N50,
                          color = Group),
                      position = position_jitter(width =0.03),size =0.2, shape = 20)#调整散点，使取值相同的原重合散点分散开
p6f

#在p6散点和二分之一小提琴图中间添加箱线图，得到p6：
p6g <- p6f + geom_boxplot(outlier.shape = NA, #NA隐藏离群点；
                          width =0.1,
                          alpha=0.7)
p6g

#将图形翻转
#p6h <- p6g + coord_flip()
#p6h

#去掉灰底
p6i <- p6g + theme_classic()+
  theme(legend.position="none") + 
  theme(text = element_text(size=15)) + 
  #ylim(0.0,1.3)+
  ylab("Contig N50 (Kbp)")+xlab("Sample")+
  theme(axis.text.x=element_text(vjust=1,size=10,color = "black"))+
  theme(axis.text.y=element_text(vjust=0.5,size=10,color = "black"))+
  theme(axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 0.5),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 0.5))
p6i

figure <- ggarrange(p1i, p2i, p3i, p4i, p5i, p6i,
                    labels = NULL,
                    ncol = 1, nrow = 6,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv",
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure

#拼图
#library(cowplot)
#plot_grid(p1i,p2i,nrow=1)

