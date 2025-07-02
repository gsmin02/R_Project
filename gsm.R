print("Hello")

#===============================================================================
# 저장된 CSV 데이터 불러오기

csv_data <- read.csv("true_cost_fast_fashion.csv", header=TRUE)
summary(csv_data)
head(csv_data)

#===============================================================================
# DB 연동 관련 패키지 준비

install.packages("RMySQL")
install.packages("DBI")

library(RMySQL)
library(DBI)

#===============================================================================
# RMySQL을 통한 DB 연결

# true_cost_fast_fashion -> tcff 약자로 변경
tcff_db <- dbConnect(RMySQL::MySQL(),
                  host = "localhost",
                  port = 3306,
                  dbname = "db_r_202112401",
                  user = "root",
                  password = "1100")

#===============================================================================
# DB 테이블 생성
# GPT 활용 : (CSV 데이터 주며) MySQL 테이블 생성 스키마 작성해줘

# 테이블명 지정
table_name <- "true_cost_fast_fashion"

# paste0() : 구분자 없이 내용을 이어 붙이는 함수
create_table_sql <- paste0(
  "CREATE TABLE IF NOT EXISTS ", table_name, " (",
  "id INT AUTO_INCREMENT PRIMARY KEY, ",
  "Brand VARCHAR(255) NOT NULL, ",
  "Country VARCHAR(255) NOT NULL, ",
  "Year INT NOT NULL, ",
  "Release_Cycles_Per_Year INT, ",
  "Working_Hours_Per_Week INT, ",
  "Child_Labor_Incidents INT, ",
  "Shopping_Frequency_Per_Year INT, ",
  "Monthly_Production_Tonnes DECIMAL(10, 2), ",
  "Avg_Item_Price_USD DECIMAL(8, 2), ",
  "Carbon_Emissions_tCO2e DECIMAL(12, 3), ",
  "Water_Usage_Million_Litres DECIMAL(10, 2), ",
  "Landfill_Waste_Tonnes DECIMAL(10, 2), ",
  "Avg_Worker_Wage_USD DECIMAL(8, 2), ",
  "Return_Rate_Percent DECIMAL(5, 2), ",
  "Avg_Spend_Per_Customer_USD DECIMAL(8, 2), ",
  "Instagram_Mentions_Thousands INT, ",
  "TikTok_Mentions_Thousands INT, ",
  "Sentiment_Score DECIMAL(4, 3), ",
  "Social_Sentiment_Label VARCHAR(50), ",
  "GDP_Contribution_Million_USD DECIMAL(10, 2), ",
  "Env_Cost_Index DECIMAL(4, 3), ",
  "Sustainability_Score DECIMAL(5, 2), ",
  "Transparency_Index DECIMAL(5, 2), ",
  "Compliance_Score DECIMAL(5, 2), ",
  "Ethical_Rating DECIMAL(4, 3)",
  ")"
)
# 테이블 생성, 없는 경우만 NOT EXISTS
dbExecute(tcff_db, create_table_sql)

#===============================================================================
# CSV 데이터 삽입
# GPT 활용 : (CSV 데이터 주며) INSERT문 간단하게 작성할 방법 추천해줘

# csv 데이터 행 이름 지정
cols <- c(
  "Brand", "Country", "Year", "Monthly_Production_Tonnes",
  "Avg_Item_Price_USD", "Release_Cycles_Per_Year", "Carbon_Emissions_tCO2e",
  "Water_Usage_Million_Litres", "Landfill_Waste_Tonnes", "Avg_Worker_Wage_USD",
  "Working_Hours_Per_Week", "Child_Labor_Incidents", "Return_Rate_Percent",
  "Avg_Spend_Per_Customer_USD", "Shopping_Frequency_Per_Year",
  "Instagram_Mentions_Thousands", "TikTok_Mentions_Thousands",
  "Sentiment_Score", "Social_Sentiment_Label", "GDP_Contribution_Million_USD",
  "Env_Cost_Index", "Sustainability_Score", "Transparency_Index",
  "Compliance_Score", "Ethical_Rating"
)

# 읽어온 csv 파일을 DB에 저장
# csv에서 한줄씩 읽어오기
for (i in 1:nrow(csv_data)) {
  # 하나의 열씩 뽑기
  rows <- csv_data[i, ]
  
  # 자료형에 따른 따옴표 붙이기 작업
  value <- sapply(cols, function(it) {
    obj <- rows[[it]]
    # 문자열이면 'obj'
    if (is.character(obj)) {
      return(paste0("'", obj, "'"))
    }
    # 아니면 obj 그대로
    else {
      return(as.character(obj))
    }
  })
  
  # 각 데이터 쉼표로 구분
  col_values <- paste(value, collapse = ", ")
  # SQL 문 작성
  sql <- paste0("INSERT INTO ", table_name, " (",
                       paste(cols, collapse = ", "),
                       ") VALUES (", col_values, ")")
  # sql문 테스트용
  # print(sql)
  dbExecute(tcff_db, sql)
}
# 작업 확인용
print("완료됨")

#===============================================================================
# 데이터 확인 및 DB 연결 해제

# 저장된 데이터 확인
# db_data <- dbReadTable(tcff_db, table_name)
# print(head(db_data))

# 데이터 분석 활용할 데이터 불러오기
db_data <- dbGetQuery(tcff_db, paste0("SELECT * FROM ", table_name))

# db 연결 해제
dbDisconnect(tcff_db)

# 데이터 확인
print(str(db_data))

#===============================================================================
# 데이터 시각화 : 히트맵
# 출처 : https://blog.naver.com/pmw9440/221576168716

library(corrplot)

# 데이터 변환
numeric_df <- db_data[sapply(db_data, is.numeric)]
# id를 제외한 데이터 불러오기
numeric_df <- numeric_df[, !names(numeric_df) == "id"]

# 상관관계 계산
corr_matrix <- cor(numeric_df)
# 행렬 변환
corr_matrix_m <- as.matrix(corr_matrix)

# 히트맵 시각화
# symm : 대각선으로 일정하게 나타남
# Colv, Rowv : matrix 순서 유지
heatmap(corr_matrix_m,
        col = colorRampPalette(c("blue", "white", "red"))(256),
        main = "상관관계 히트맵",
        symm = TRUE,
        Colv = NA,
        Rowv = NA,
        cexRow = 0.8,
        cexCol = 0.8,
        margins = c(8, 8))

#===============================================================================
# 데이터 시각화 : 산점도
# 출처 : Chapter7 데이터 시각화
# 출처2 : https://blog.naver.com/padosori60/220935469036

library(dplyr)
library(ggplot2)

features <- c(
  "Monthly_Production_Tonnes",
  "Avg_Item_Price_USD",
  "Carbon_Emissions_tCO2e",
  "Water_Usage_Million_Litres",
  "Sustainability_Score"
)
pairs(numeric_df[, features],
      main = "산점도 행렬",
      cex = 0.2,
      col = "blue",
)

#===============================================================================
# 데이터 시각화 : 히스토그램

# Avg_Item_Price_USD
hist(numeric_df[["Avg_Item_Price_USD"]],
     main = "평균 제품 가격(달러)",
     xlab = "Avg_Item_Price_USD",
     col = "skyblue")

# Carbon_Emissions_tCO2e
hist(numeric_df[["Carbon_Emissions_tCO2e"]],
     main = "탄소 배출량",
     xlab = "Carbon_Emissions_tCO2e",
     col = "skyblue")

# Water_Usage_Million_Litres
hist(numeric_df[["Water_Usage_Million_Litres"]],
     main = "물 사용량",
     xlab = "Water_Usage_Million_Litres",
     col = "skyblue")

#===============================================================================
# 데이터 예측 : 다중 선형 회귀

lm_model <- lm(Sustainability_Score
               ~ .
               , data = numeric_df)

print(summary(lm_model))
print(coef(lm_model))
#===============================================================================
# 회귀 계수를 통한 각 특성별 영향 시각화
# 출처: Chapter7 데이터 시각화

coef <- coef(lm_model)
coef <- coef[!names(coef) == "(Intercept)"]

coef_plot <- data.frame(
  Feature = names(coef),
  Coefficient = as.numeric(coef)
)

coef_plot$Feature <- factor(coef_plot$Feature,
                                    levels = coef_plot$Feature[
                                      order(abs(coef_plot$Coefficient),
                                            decreasing = FALSE)])

weight_plot <- ggplot(coef_plot, aes(x = Feature, y = Coefficient)) +
  geom_bar(stat = "identity", fill="skyblue") +
  coord_flip() +
  labs(title = "예측 모델 회귀 계수",
       x = "특성",
       y = "회귀 계수 값")

print(weight_plot)
#===============================================================================