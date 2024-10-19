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

