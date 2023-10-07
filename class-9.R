set.seed(123)
data <- c(132, 145, 120, 155, 140, 128, 150, 136, 125, 142, 138, 147, 130, 143, 137, 126, 134, 139, 133, 131, 141, 149, 148, 123, 129, 124, 127, 146, 121, 122)

n = 10000
bootstrap_samples <- matrix(nrow = n, ncol = length(data))

for (i in 1:n) {
  bootstrap_samples[i, ] <- sample(data, replace = TRUE)
}

bootstrap_means <- apply(bootstrap_samples, 1, mean)
head(bootstrap_means)


#cross
# ライブラリの読み込み
library(caret)

# データの読み込み
data(iris)

# k分割交差検証の設定
set.seed(123)  # 乱数シードの設定
k <- 5          # フォールド数

# k分割交差検証の実行（ここでは決定木モデルを使用）
ctrl <- trainControl(method = "cv", number = k)
model <- train(Species ~ ., data = iris, method = "rpart", trControl = ctrl)

# 結果の表示
print(model)


install.packages("MASS")
library(MASS)