# 载入 E7 数据:每个模型文件第一行 source("load_data.R")

library(tidyverse) # utils.R 里的数据处理要用

# 前提:R 的工作目录是 sandbox(数据在 sandbox/Data/)
# 用 getwd() 检查,不对就 setwd("C:/Users/Lin/sandbox")
source("246/Bayesian/exercise/E7/utils.R")

# 数据已经在 sandbox/Data/ 里,不需要再下载
# setup.data()

d <- load.FSE(cleanup = TRUE)
d <- by.project.language(d)
d <- d[, -c(2:5, 7:8)]
d$project <- as.integer(d$project)

# 检查:应该是 1127 行 3 列,列名 project / n_bugs / language_id
dim(d)
colnames(d)