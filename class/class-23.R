install.pacakges("haven")  # 必要な場合のみインストール
library('haven')

data <- read_dta("/Users/seito/Documents/develop/cs312/class/jtpa.dta")
head(data)

always_taker <- data[data$training == 1,]
nrow(always_taker) #26

compliers <- data[data$assignmt == 1 & data$training == 1,]
nrow(compliers)

never_takers <- data[data$assignmt == 1 & data$training == 0,]
nrow(never_takers) #
