# 必要なライブラリの読み込み
library(ggplot2)

# ダミーデータの生成
set.seed(123) # 乱数生成のシードを設定
n <- 25 # サンプル数
typing_speed <- runif(n, min = 40, max = 260) # タイピング速度: 40〜100 WPMの範囲
study_continuation <- 0.5 * typing_speed + rnorm(n, mean = 0, sd = 9) # 継続率: タイピング速度に基づいて生成

# データフレームの作成
data <- data.frame(typing_speed, study_continuation)

# 単回帰分析の実行
model <- lm(study_continuation ~ typing_speed, data = data)

# 結果の要約
summary(model)

# 散布図と回帰直線の描画
ggplot(data, aes(x = typing_speed, y = study_continuation)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  xlab("Typing Speed (WPM)") +
  ylab("Rate of Programming Study Retention (weeks)")

