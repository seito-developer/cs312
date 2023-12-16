# データを設定
city_data <- c(55, 53, 22)
suburb_data <- c(143, 81, 149)

# 各群の雇用者と非雇用者の合計
total_city <- sum(city_data)
total_suburb <- sum(suburb_data)

# 各群の雇用者の割合
employment_rate_city <- city_data[1] / total_city
employment_rate_suburb <- suburb_data[1] / total_suburb

# 結果の表示
employment_rate_city
employment_rate_suburb