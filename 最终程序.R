# 数据清洗

# 1.1 删除缺失值和重复观测
music0 = read.csv("bilibili data.csv")                                    # 读入数据
music0 = na.omit(music0)                                                  # 删去缺失值
music0 = music0[!duplicated(music0), ]                                    # 删去重复观测
music0 = music0[which(nchar(as.character(music0$总时长)) == 5), ]         # 删去时长格式不对的观测

# 1.2 将数据转为合适类型
music0$主播粉丝数 = as.numeric(gsub(pattern = "万", replacement = "e+04", x = music0$主播粉丝数)) # 将万改为4个0
music0$视频投稿时间 = as.Date(substr(music0$视频投稿时间, start = 0, stop = 10))  # 只保留日期
music0$播放量 = as.numeric(gsub("万", "e+04", music0$播放量))                # 万换成4个0
music0$硬币数 = as.numeric(gsub("万", "e+04", music0$硬币数))                # 万换成4个0
# 标签转为字符型数据
music0$标签1 = as.character(music0$标签1)                                    # 标签1变为字符类型
music0$标签2 = as.character(music0$标签2)                                    # 标签2变为字符类型
music0$标签3 = as.character(music0$标签3)                                    # 标签3变为字符类型
music0$标签4 = as.character(music0$标签4)                                    # 标签4变为字符类型
music= music0                                                                # 清洗完毕，转为正式数据

# 2. 描述性分析

# 2.1 因变量---播放量的分布

# 播放量的分布
hist(music$播放量[which(music$播放量 <= quantile(music$播放量, 0.9))], las = 1, col = rgb(138/255, 215/255, 218/255), xlab = "", ylab = "", main = "", ylim = c(0,800), axes = T, sub = "仅显示前90%数据")   # 前90%的样本的直方图
list(mean = mean(music$播放量), median = median(music$播放量))                # 播放量的均值和中位数

# 2.2 主播信息与播放量的相关性探究
# 2.2.1 粉丝数与播放量的相关性探究### 2.2.2 投稿数与播放量对数相关性探究
```{r warning=F}
# 投稿数按照三位点分为三组，做播放量对数分组箱线图
music$group.pub = ifelse(music$主播投稿数 >= quantile(music$主播投稿数, 0), 1, 0) +    # 投稿数分三组
  ifelse(music$主播投稿数 >= quantile(music$主播投稿数, 0.33), 1, 0) +
  ifelse(music$主播投稿数 >= quantile(music$主播投稿数, 0.66), 1, 0)   
boxplot(log(music$播放量) ~ music$group.pub, col = rgb(61/255,188/255,192/255),        # 投稿数分组箱线图
        names = c("1~6", "7~26", "27~1631"), las = 1, axes = T,
        xlab = "主播投稿数", ylab = "播放量对数")

# 粉丝数按照三分位点分为三组，做播放量对数分组箱线图
music$group.fans = ifelse(music$主播粉丝数 >= quantile(music$主播粉丝数, 0), 1, 0) +  # 粉丝数分三组
  ifelse(music$主播粉丝数 >= quantile(music$主播粉丝数, 0.33), 1, 0) +
  ifelse(music$主播粉丝数 >= quantile(music$主播粉丝数, 0.66), 1, 0)  
boxplot(log(music$播放量) ~ music$group.fans, col = rgb(61/255,188/255,192/255),      # 粉丝数分组的播放量对数箱线图
        names = c("0~53", "54~1076", "1077~269万1千"), las = 1, axes = T,
        xlab = "主播粉丝数", ylab = "播放量对数")

# 2.3 视频信息与播放量的相关性探究
# 2.3.1 视频名称长度与播放量的相关性探究

# 视频名称长度按照三分位点分为三组，做播放量对数的分组箱线图
music$视频名称 = as.character(music$视频名称)                   # 视频名称变量转为字符型
music$group.name = ifelse(nchar(music$视频名称) >= quantile(nchar(music$视频名称), 0), 1, 0) +    # 按照视频名称长度分三组
  ifelse(nchar(music$视频名称) >= quantile(nchar(music$视频名称), 0.33), 1, 0) +
  ifelse(nchar(music$视频名称) >= quantile(nchar(music$视频名称), 0.66), 1, 0) 
boxplot(log(music$播放量) ~ music$group.name, col = rgb(61/255,188/255,192/255),    # 播放量对数分组箱线图
        names = c("1~16", "17~25", "26~80"), las = 1, xlab = "视频名称长度(个字符)", ylab = "播放量对数")

# 2.3.2 视频标签与播放量的相关性探究

# 保留频数最高的前十个标签，按照是否含此标签做播放量均值柱状图

# 分析视频标签出现频率最高的前十个
labell = cbind(music$标签1, music$标签2, music$标签3, music$标签4)               # 标签整合在一起
sort(table(c(music$标签1, music$标签2, music$标签3, music$标签4)), decreasing = T)[1:10]    # 标签频数排序
# 设置是否含有此标签的示性变量
music$原创 = apply(labell, 1, function(x){ifelse("原创" %in% x, 1, 0)})            # 每个观测是否含有第1标签
music$音乐 = apply(labell, 1, function(x){ifelse("音乐" %in% x, 1, 0)})            # 每个观测是否含有第2标签
music$原创音乐 = apply(labell, 1, function(x){ifelse("原创音乐" %in% x, 1, 0)})    # 每个观测是否含有第3标签
music$纯音乐 = apply(labell, 1, function(x){ifelse("纯音乐" %in% x, 1, 0)})        # 每个观测是否含有第4标签
music$电音 = apply(labell, 1, function(x){ifelse("电音" %in% x, 1, 0)})            # 每个观测是否含有第5标签
music$原创曲 = apply(labell, 1, function(x){ifelse("原创曲" %in% x, 1, 0)})        # 每个观测是否含有第6标签
music$古风 = apply(labell, 1, function(x){ifelse("古风" %in% x, 1, 0)})            # 每个观测是否含有第7标签
music$MV = apply(labell, 1, function(x){ifelse("MV" %in% x, 1, 0)})                # 每个观测是否含有第8标签
music$国人原创 = apply(labell, 1, function(x){ifelse("国人原创" %in% x, 1, 0)})    # 每个观测是否含有第9标签

# 各标签下是否含此标签的播放量均值
mean.原创 = sapply(split(music$播放量, as.factor(music$原创)), mean)
mean.音乐 = sapply(split(music$播放量, as.factor(music$音乐)), mean)
mean.原创音乐 = sapply(split(music$播放量, as.factor(music$原创音乐)), mean)
mean.原创曲 = sapply(split(music$播放量, as.factor(music$原创曲)), mean)
mean.国人原创 = sapply(split(music$播放量, as.factor(music$国人原创)), mean)
mean.古风 = sapply(split(music$播放量, as.factor(music$古风)), mean)
mean.电音 = sapply(split(music$播放量, as.factor(music$电音)), mean)
mean.纯音乐 = sapply(split(music$播放量, as.factor(music$纯音乐)), mean)
mean.MV = sapply(split(music$播放量, as.factor(music$MV)), mean)

# 是否含此标签的播放量均值柱状图
mean.data = cbind(mean.古风, mean.MV, mean.原创, mean.原创曲, mean.电音, mean.音乐, mean.原创音乐, mean.国人原创, mean.纯音乐)                                                        # 将均值综合在一个矩阵
barplot(mean.data, beside = T, names.arg = c("古风", "MV", "原创", "原创曲", "电音", "音乐", "原创音乐", "国人原创", "纯音乐"), 
        ylab = "平均播放量", ylim = c(0, 12000), border = NA, las = 1, col = c(rgb(61/255,188/255,192/255),rgb(235/255,117/255,19/255)))

# 2.3.3 视频时长与播放量相关性的探究

# 按照时长以10min、30min为间隔分为三组，做播放量对数分组箱线图
minute = substr(music$总时长, 1, 2)                                    # 提取时长中的分钟
sec = substr(music$总时长, 4, 5)                                       # 提取时长中的秒
time = as.numeric(minute) * 60 + as.numeric(sec)                       # 时长换算为秒
music$group.time = ifelse(time >= 16, 1, 0) + ifelse(time >= 601, 1, 0) + ifelse(time >= 1801, 1, 0)  # 总时长分三组

# 3. 回归分析
# 3.1 双对数线性模型

library(car)
music = music[-which((music$主播投稿数 == 0) | (music$主播粉丝数 == 0)), ]     # 为做对数，将0值观测去掉
lm1 = lm(log(播放量) ~ log(主播投稿数) + log(主播粉丝数) + as.factor(group.name) + as.factor(group.time) + 
           原创+ 音乐 + 原创音乐 + 纯音乐 + 电音 + 原创曲 + 国人原创 + 古风 + MV, data = music)        # 简单线性回归,"低"为基准组
summary(lm1)    # 简单双对数线性回归结果


# 3.2 利用逐步回归进行变量选择

# 逐步选择
step(lm1)                                           # 逐步回归
lm2 = lm(formula = log(播放量) ~ log(主播投稿数) + log(主播粉丝数) +    # 选用逐步回归的最终模型
           音乐 + 原创音乐 + 纯音乐 + 原创曲 + 国人原创 + 古风, data = music) 
summary(lm2)                                        # 最终模型的回归结果
plot(lm2, which = 1:1)                              # 最终模型的残差图
vif(lm2)                                            # 最终模型中各变量的方差膨胀因子

