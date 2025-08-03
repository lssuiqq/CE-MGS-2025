library(ggplot2)
library(ggExtra)

dat <- read.delim(file = "checkm1_checkm2.txt", header = T)
dat$Quality <- factor(dat$Quality, levels=c("MQ MAG","HQ MAG"))

p1 <- ggplot(dat,aes(x=Completeness_checkM2,y=Contamination_checkM2, color=Quality))+
  geom_point(aes(color = Quality),alpha=1,size=0.3)+
  #geom_smooth(method="lm",formula = y ~ x, se=T)+
  #loess, lm
  scale_color_manual(values=c("#6BB88A","#99749F"))+
  scale_fill_manual(values=c("#6BB88A","#99749F"))+
    theme_test()+
  theme(legend.position = "right",legend.background = element_blank(),legend.key = element_rect(colour = 'gray'))+
  theme(axis.text.x = element_text(size = 5,hjust = 0.5,vjust = 0), 
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  theme(axis.text.y = element_text(size = 5,hjust = 1,vjust = .5),
        axis.title = element_text(size = 6.5), legend.title = element_blank(), 
        legend.text = element_text(size = 6.5))+
  labs(x="Completeness (checkM2)",y="Contamination (checkM2)")+
  geom_vline(xintercept=c(90),linetype = "solid",linewidth=0.5,colour = "grey",alpha=0.75)+
  geom_hline(yintercept=c(5.0),linetype = "solid",linewidth=0.5,colour = "grey",alpha=0.75)
#“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”.
  
p1

#添加边际图
p2 <- ggMarginal(p1, type="histogram", bins=51, size=6, margins="both", groupColour=TRUE, groupFill=TRUE)

p2
#ggMarginal(p, data, x, y, type = c("density", "histogram", "boxplot", "violin", "densigram"), margins = c("both", "x", "y"), size = 5, ..., xparams = list(), yparams = list(), groupColour = FALSE, groupFill = FALSE)
#  type = c("density", "histogram", "boxplot", "violin", "densigram")

