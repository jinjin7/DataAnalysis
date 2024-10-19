# 데이터 불러오기
HR = read.csv('C:\\Users\\gabe7\\Downloads\\archive (1)\\HR_comma_sep.csv')

# 데이터 파악하기
head(HR, 10)
tail(HR, 10)

summary(HR)

# R에서는 데이터 타입을 String이라고 한다.
str(HR)

# 데이터 strings 변경
HR$Work_accident=as.factor(HR$Work_accident)
HR$left=as.factor(HR$left)
HR$promotion_last_5years=as.factor(HR$promotion_last_5years)

summary(HR$left)

# 조건에 맞는 값 할당하기 ifelse
HR$satisfaction_level_group1 = ifelse(HR$satisfaction_level > 0.5, 'High', 'Low')
HR$satisfaction_level_group1 = as.factor(HR$satisfaction_level_group1)
summary(HR$satisfaction_level)

HR$satisfaction_level_group2 = ifelse(HR$satisfaction_level > 0.8, 'High',
                                      ifelse(HR$satisfaction_level > 0.5, 'Mid', 'Low'))
HR$satisfaction_level_group2 = as.factor(HR$satisfaction_level)
summary(HR$satisfaction_level_group2)

# 조건에 맞는 데이터 추출하기 subset
HR_High = subset(HR, salary == 'high')
summary(HR_High$salary)

HR_High_IT = subset(HR, salary == 'high' & sales == 'IT')
HR_High_IT2 = subset(HR, salary == 'high' | sales == 'IT')

# 분위수 계산
quantile(HR$satisfaction_level, probs = c(0,1, 0.3, 0.6, 0.9))

# 단일 변수의 합 구하기
sum(HR$satisfaction_level)
# 단일 변수의 평균 구하기
mean(HR$last_evaluation)
# 단일 변수의 표준편차 구하기
sd(HR$satisfaction_level)
# 다중 변수의 합, 평균 구하기
# 행은 rowSums, rowMeans 활용
colMeans(HR[1:5])
colSums(HR[1:5])

# 빈도 테이블 작성하기
TABLE = as.data.frame(table(HR$sales))
TABLE2 = as.data.frame(xtabs(~ HR$salary + HR$sales))

IMDB = read.csv("C:\\Users\\gabe7\\Downloads\\IMDB-Movie-Data.csv")

# 결측치 확인
sum(is.na(IMDB$Metascore))
colSums(is.na(IMDB))

# 결측치 전부 삭제
IMDB2 = na.omit(IMDB)
colSums(is.na(IMDB2))

# 특정 변수에 결측치가 존재하는 행만 삭제하는 경우
# 12번째 열에 결측치가 있는 경우에만 삭제
IMDB3 = IMDB[complete.cases(IMDB[,12]),]
colSums(is.na(IMDB3))

# 결측치를 특정값으로 대체할 경우
IMDB$Metascore2 = IMDB$Metascore
IMDB$Metascore2[is.na(IMDB$Metascore2)]=58.99

# 결측치 생략하고 계산할 경우
mean(IMDB$Revenue..Millions., na.rm=TRUE)

# 결측치 처리를 위한 데이터의 분포 탐색
library(ggplot2)

ggplot(IMDB,aes(x=Revenue..Millions.)) +
  geom_histogram(fill='royalblue', alpha = 0.4) +
  ylab('') +
  xlab("Revenue_Millions") +
  theme_classic()

ggplot(IMDB,aes(x="",y=Revenue..Millions.)) +
  geom_boxplot(fill='red', alpha=0.4,outlier.color = 'red') +
  xlab('') +
  ylab("Revenue_Millions") +
  theme_classic()

summary(IMDB$Revenue..Millions.)
# 이런 분포는 항상 평균과 중위수의 차이를 확인해봐야한다.
# 이런 분포는 데이터의 결측치를 평균으로 대체하면 매우 위험하다.
# 평균은 극단값에 영향을 받기 때문이다.
# 평균보다는 중위수로 대체하는게 안전하다.

# 이상치 : 패턴에서 벗어난 값 또는 중심에서 좀 많이 떨어져 있는 값
# 이상치 뽑아내기
# 박스 플롯이 이상치 탐색을 가장 하기 좋다
ggplot(IMDB,aes(x=as.factor(Year),y=Revenue..Millions.))+
  geom_boxplot(aes(fill=as.factor(Year)),outlier.colour = 'red',alpha=I(0.4))+
  xlab("년도") + ylab("수익") + guides(fill = FALSE) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
# 상자 안에 그려져 있는 직선은 중위수를 나타낸다.
# 상자 외부에 있는 직선은 울타리라고 부른다.
# 이 울타리를 벗어난 값들을 Outlier라고 부른다.
# Outlier는 통계추정에 있어서 방해가 된다.

# 문자열 데이터 다루기 
# 문자 추출
substr(IMDB$Actors[1], 1, 5)

# 문자열 붙이기
paste(IMDB$Actors[1], "_",'A')
paste(IMDB$Actors[1],"_",'A',sep="") # 띄어쓰기 없이 붙이기
paste(IMDB$Actors[1],"_","Example",sep="|") # |로 붙이기

# 문자열 분리
strsplit(as.character(IMDB$Actors[1]), split=",")

# 문자열 대체 
IMDB$Genre2 = gsub(","," ",IMDB$Genre)


