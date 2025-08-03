#一、数据导入与过滤
library(igraph)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(psych)
#读入stgb绝对丰度表；
stgb <- read.delim("stgb_b15.txt",header = T, row.names = 1)
#转成矩阵，之后的相关性计算需要矩阵对象；
stgb<-as.matrix(stgb)
dim(stgb)
head(stgb)

#将丰度值大于1的值替换为1，便于计算不同stgb的检测率；
dt<-stgb
dt[dt>1]<-1
#将样本发现率低于30%的stgb过滤掉；
no<-which(rowSums(dt)/ncol(dt)>0.3)
length(no)
stgb<-stgb[no,]

#二、相关性系数计算
#计算相关性系数；
sp.cor<-rcorr(t(stgb),type="spearman")

#提取r、p值矩阵；
r.cor<-sp.cor$r
p.cor<-sp.cor$P

#使用Benjamini-Hochberg("FDR-BH")法进行多重检验校正；
p.adj <- p.adjust(p.cor, method="BH")

#指定阈值；
r.cutoff=0.6
p.cutoff=0.05

#在同时考虑“正相关”和“负相关”情况下；
r.matrix<-r.cor
p<-p.adj 
#将矩阵中不符合条件的r值替换为0；
r.matrix[which(abs(r.cor) < r.cutoff)]=0
r.matrix[which(p.adj>p.cutoff)]=0
#r.matrix[p.adj>0.05|abs(r.cor)<0.6] = 0

# 将occor.r保存为csv文件
write.csv(r.matrix,file="b15_0.05_0.6_30%.r.csv")

#删掉相关系数矩阵数据全都为0（对角线处的1不计）的行和列；
r.matrix<-r.matrix[which(rowSums(r.matrix)!=1),]
r.matrix<-r.matrix[,which(colSums(r.matrix)!=0)]

#查看过滤后的矩阵；
dim(r.matrix)
r.matrix[1:7,1:7]


#三、生成网络图
#使用邻接矩阵（即相关系数矩阵）创建网络；
#将矩阵转换为正数


b15<-graph.adjacency(r.matrix,weight=T,mode="undirected")
#去掉冗余的边（multiple edges、loop edges）；
b15<-simplify(b15)

#生成网络图的结点标签（OTU id）和degree属性；
V(b15)$label <- V(b15)$name
V(b15)$degree <- degree(b15)

#查看网络图的对象结构;
print(b15)


#四、导出graph对象
#将网络图导出为"graphml"、"gml"格式，方便导入Gephi中使用；
write_graph(b15, "b15-1.graphml", format = "graphml")
write_graph(b15, "b15-1.gml", format = "gml")
#支持的格式有"edgelist", "pajek", "ncol", "lgl","graphml", "dimacs", "gml", "dot", "leda"), ...
#导出"graphml"格式的网络对象，之后可用Gephi和Cytoscape等网络图可视化软件直接打开，做进一步调整、美化。


#五、使用igraph绘制网格图
#虽然直接使用igraph绘制个性化网络图没有Gephi简单，igraph主要用于计算网络图的拓扑性质（topological properties），我这里仍然做了一些尝试。

#计算群体结构（short random walks）；
c <- cluster_walktrap(b15)
#使用默认颜色列表；
V(b15)$color <- c$membership+1
#绘制网络图；
layout <- layout.fruchterman.reingold #力导向布局，最常用
#也可以尝试其他的Layout;
layout <- layout_as_tree
layout <- layout_nicely
layout <- layout_on_sphere
#……
#绘制网络图；
plot.igraph(b15,vertex.size=5,
            #vertex.label=NA,
            vertex.lable.cex=0.1,
            edge.curved=F,
            edge.size=1.5,
            layout = layout.fruchterman.reingold)
#默认的结点大小为15，这里改为4，默认边的粗细为1，这里改为1.5；edge.curved若改为TURE,可实现类似文章开头那样的效果。
