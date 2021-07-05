getwd()
setwd("D:\\data")
airdata <- read.csv('air_data.csv',stringsAsFactors = FALSE)
airdata

#1数据清洗
#列表显示缺失值
table(airdata$GENDER)
sum(is.na(airdata))
#mice包中的md.pattern（）函数可以生成一个以矩阵或数据框形式展示缺失值模式的表格
library(mice)
md.pattern(airdata[15:20])
md.pattern(subset(airdata, select=c(SUM_YR_1,SUM_YR_2)))
#图形探究缺失数据
#VIM包中提供大量能可视化数据集中缺失值模式的函数：aggr（）、matrixplot（）、scattMiss（）
library("VIM")
aggr(airdata)
aggr(subset(airdata, select=c(SUM_YR_1,SUM_YR_2)),prop=F,numbers=TRUE)
# 丢弃票价为空的记录
delet_na <- airdata[-which(is.na(airdata$SUM_YR_1) |
                              is.na(airdata$SUM_YR_2)), ]
# 丢弃票价为0、平均折扣率不为0、总飞行公里数大于0的记录
index <- ((delet_na$SUM_YR_1 == 0 & delet_na$SUM_YR_2 == 0)
          * (delet_na$avg_discount != 0)
          * (delet_na$SEG_KM_SUM > 0))
deletdata <- delet_na[-which(index == 1), ]
# 保存清洗后的数据
cleanedfile <- deletdata
write.csv(cleanedfile,'cleanedfile.csv')


 #2构建航空客户价值分析的关键特征
# LRFMC模型：将客户关系长度L，消费时间间隔R，消费频率F，飞行里程M和折扣系数的平均值C作为航空公司识别客户价值的关键特征记为LRFMC模型。
# L；会员入会时间距观测窗口结束的月数。
# R：客户最近一次乘坐公司飞机距观测窗口结束的月数。
# F：客户在观测窗口内乘坐公司飞机的次数。
# M：客户在观测窗口内累计的飞行里程。
# C：客户在观测窗口内乘坐舱位所对应的折扣系数的平均值
# 数据读取
datafile <- read.csv('cleanedfile.csv', header = TRUE)
# 属性选择
col <- c(11,3,24,12,18,30)
datafile[, col]
# 保存属性选择后的数据
write.csv(datafile[, col],'datareductionfile.csv')
datafile <- read.csv('datareductionfile.csv', header = TRUE)
# 定义一个矩阵
names(datafile )
outfile <- matrix(data=NA, nrow = nrow(datafile), ncol = 5, byrow = TRUE, dimnames = list(c(1:nrow(datafile)),c("L","R","F","M","C")))
# 数据变换
outfile[,1] <- round((as.Date(datafile[,2]) - as.Date(datafile[,3]))/30,2)
outfile[,2] <- datafile[,4]
outfile[,3] <- datafile[,5]
outfile[,4] <- datafile[,6]
outfile[,5] <- datafile[,7]
summary(outfile)
write.csv(outfile,'datachange.csv',row.names = FALSE)
datafile <- read.csv('datachange.csv', header = TRUE)


# 3数据去中心化后的标准化
zscoredfile <- scale(datafile)
# 数据写出
write.csv(zscoredfile, 'standardizeddata.csv',row.names = FALSE)
inputfile <- read.csv('standardizeddata.csv', header = TRUE)



#4 聚类分析
result <- kmeans(inputfile, 5)
# 结果输出
type <- result$cluster
table(type)  # 查看类别分布
type
centervec <- result$center
centervec # 查看簇中心点

#5绘制雷达图
max <- apply(centervec,2,max)
min <- apply(centervec, 2, min)
df = data.frame(rbind(max,min,centervec))
library(fmsb)
radarchart(df=df,seg=5,plty=1,vlcex = 0.7)

