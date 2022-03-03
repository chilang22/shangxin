
#####통사적 ID 태그 분석

##엑셀 파일 읽어들이기
install.packages("readxl")  # 패키지 설치
library(readxl) # 패키지 불러오기

a <- read_excel("SHANGXIN_SYN.xlsx") # "SHANGXIN_SYN.xlsx" 불러들이기

attach(a)  # 데이터프레임 a의 열을 읽도록 붙이기

b <- table(WORD, SYN)  # WORD의 변수가 행이름, SYN의 변수가 열이름으로 교차하는 빈도표 생성
c <- prop.table(b, 1)  # 절대빈도표를 각 행의 합을 1(100%)로하는 상대빈도표로 전환
s <- round(c, 4)  # 상대빈도 소숫점아래 4자리까지 출력 

write.table(s,"SYN_RE.txt",quote = F,sep = "\t") #  상대빈도표 내보내기(저장)


### 통사적 프로필에 대한 단순 이진 대응분석(CA분석)

install.packages(c("FactoMineR", "factoextra")) # 패키지 설치

library("FactoMineR")  # 패키지 불러오기
library("factoextra")  # 패키지 불러오기

d <- read.table("SYN_KOR.txt", header= TRUE, sep="\t", row.names= 1, encoding="UTF-8") # 변수명을 한국어와 중국어로 변환한 "SYN_KOR.txt" 상대빈도표 읽어들이기

# "FactoMineR"의 함수 CA()로 대응분석 후 "factoextra"의 함수 fviz_ca_biplot() 으로 행과 열 변수의 biplot 그리기
# repel= TRUE 로 문자겹침 방지

s <- t(s) # 상대빈도표 d의 행과 열 전환
res.ca <- CA(s, graph = FALSE) #상대빈도표 S에 대한 대응분석
fviz_ca_biplot(res.ca, repel = TRUE) #대응분석 biplot 출력


### 통사적 프로필에 대한 HAC분석(계층적 집적 군집화)
## 패키지 "pvclust"의 함수 pvclust() 이용 

install.packages("pvclust")  # 패키지 설치
library(pvclust) # 패키지 불러오기

## "canberra" 거리측도와 "ward.D" 그룹화 알고리즘 적용

PVClust <- pvclust(s, method.hclust= "ward.D", method.dist= "canberra", nboot=1000) # 통사적 프로필(상대빈도표) s에 대한 계층적 집적 군집화 (nboot=1000)
PVClust <- pvclust(s, method.hclust= "ward.D", method.dist= "canberra", nboot=10000) # 통사적 프로필(상대빈도표) s에 대한 계층적 집적 군집화 (nboot=10000)

plot(PVClust, frame= T)  # 군집화 결과 덴드로그램


##### 의미적 ID 태그 분석

## 엑셀 읽어들이기

s1 <- read_excel("WHYS.xlsx") # "WHYS.xlsx" (감정의 동기)불러들이기
attach(s1) # 열이름으로 작업할 수 있도록 붙이기
b <- prop.table(table(WHYS, WORD), margin = 2) # WHYS의 변수(행)와 WORD의 변수(열)이 교차하는 절대빈도표를 각 열의 합을 1(100%)로하는 상대빈도표로 전환
d <- round(b, 4) # 상대빈도 소숫점아래 4자리까지 출력 

s2 <- read_excel("DE_N.xlsx") # "SHANGXIN_DE_N.xlsx" (관형어일 때 중심명사의 의미)불러들이기
attach(s2)
b <- prop.table(table(DE_N, WORD), margin = 2)
e <- round(b, 4)

s3 <- read_excel("DE2_V.xlsx") # "DE2_V.xlsx" (부사어일 때 중심동사의 의미)불러들이기
attach(s3)
b <- prop.table(table(DE2_V, WORD), margin = 2)
f <- round(b, 4)

s4 <- read_excel("BU_DONG.xlsx") # "BU_DONG.xlsx" (보어일 때 술어동사)불러들이기
attach(s4)
b <- prop.table(table(BU_DONG, WORD), margin = 2)
g <- round(b, 4)

s5 <- read_excel("BUYU.xlsx") # "BUYU.xlsx"(술보술일 때 보어의 의미) 불러들이기
attach(s5)
b <- prop.table(table(BUYU, WORD), margin = 2)
h <- round(b, 4)

s6 <- read_excel("CO_WHERE.xlsx") # "CO_WHERE.xlsx"(공기하는 감정경험역) 불러들이기
attach(s6)
b <- prop.table(table(WHERE, WORD), margin = 2)
i <- round(b, 4)

s7 <- read_excel("YUANYIN.xlsx") # "YUANYIN.xlsx"(원인격 부사구 유무) 불러들이기
attach(s7)
b <- prop.table(table(YUANYIN, WORD), margin = 2)
j <- round(b, 4)

s8 <- read_excel("SHIYI.xlsx") # "SHIYI.xlsx"(사역구문 여부) 불러들이기
attach(s8)
b <- prop.table(table(SHIYI, WORD), margin = 2)
k <- round(b, 4)

l <- rbind(d, e, f, g, h, i, j, k)  # 의미적 ID태그 전체를 행으로 묶어 "의미적 행동프로필" 생성


### "의미적 프로필에 대한 HAC분석(계층적 집적 군집화)
## 패키지 "pvclust"의 함수 pvclust() 이용 

install.packages("pvclust")  # 패키지 설치
library(pvclust) # 패키지 불러오기

## "canberra" 거리측도와 "ward.D" 그룹화 알고리즘 적용

PVClust <- pvclust(l, method.hclust= "ward.D", method.dist= "canberra", nboot=1000) # "의미적 태그 행동프로필" l에 대한 계층적 집적 군집화 (nboot=1000)
plot(PVClust, frame= T)  # 군집화결과 덴드로그램

PVClust <- pvclust(l, method.hclust= "ward.D", method.dist= "canberra", nboot=10000) # "의미적 태그 행동프로필" l에 대한 계층적 집적 군집화 (nboot=10000)
plot(PVClust, frame= T)  # 군집화 결과 덴드로그램


##### 통사적 ID태그, 의미적ID 태그 통합 분석

t <- rbind(s, l)  # 통사적 태그와 의미적 태그를 행으로 묶어서 "통합 프로필" 생성
t <- read.table("TOTAL.txt", header= TRUE, sep="\t", row.names= 1, encoding="UTF-8") # 또는 통합된 텍스트 파일 "TOTAL.txt" 불러오기

### "통합 프로필"에 대한 HAC분석(계층적 집적 군집화)
## 패키지 "pvclust"의 함수 pvclust() 이용 
## "canberra" 거리측도와 "ward.D" 그룹화 알고리즘 적용

PVClust <- pvclust(t, method.hclust= "ward.D", method.dist= "canberra", nboot=1000) # "통사의미 통합 행동프로필" t에 대한 계층적 집적 군집화 (nboot=1000)
plot(PVClust, frame= T)  # 군집화 결과 덴드로그램

PVClust <- pvclust(t, method.hclust= "ward.D", method.dist= "canberra", nboot=10000) # "통사의미 통합 행동프로필" t에 대한 계층적 집적 군집화 (nboot=10000)
plot(PVClust, frame= T)  # 군집화 결과 덴드로그램


citation("FactoMineR")  # 적용 패키지 인용하기
citation("factoextra")
citation("pvclust")


### 각주 43) 리홍메이와 나의 NANHOU와 NANGUO의 심리적 신체적 의미 2x2빈도표에 대한  카이제곱 이질성 검정 

HONG <- matrix(c(6, 71, 194, 129), ncol=2, dimnames=list(WORD =c("nanguo", "nanshou"), SHENXIN=c("SHEN", "XIN")))
MY <- matrix(c(6, 152, 447, 370), ncol=2, dimnames=list(WORD =c("nanguo", "nanshou"), SHENXIN=c("SHEN", "XIN")))

test.HONG <- chisq.test(HONG, correct=FALSE)
test.MY <- chisq.test(MY, correct=FALSE)


# 두 빈도표의 카이제곱 통계량 더하기
test.MY$statistic + test.HONG$statistic

# 두 빈도표를 더한 후 카이제곱 통계량 계산
chisq.test(MY + HONG, correct=FALSE)$statistic

test.MY$parameter #나의 자료 카이제곱 검정 자유도
test.HONG$parameter #리홍메이 자료의 카이제곱 검정자유도
chisq.test(MY + HONG, correct=FALSE)$parameter # 두 빈도표를 더한 후 카이제곱 검정의 자유도

# 카이제곱 이질성 통계량과 자유도 구하기
het.chisq <- 205.927 - 204.1904   # 1.7366
het.df <- 1 + 1 - 1  # 자유도 = 1
het.chisq
het.df

# 카이제곱 이질성 검정 p값 계산
pchisq(het.chisq, het.df, lower.tail=FALSE)


### 각주 44), 45) 2x2 빈도표에 대한 카이제곱 독립성 검정
T <- matrix(c(304, 436, 66, 11), ncol=2, dimnames=list(WORD=c("nanshou", "nanguo"), MEAN=c("shangxin", "bushufu"))) 
chisq.test(T, correct=FALSE)
test <- chisq.test(T, correct=FALSE)


## 빈도표에 대한 카이제곱 이질성 검정에 대해서는 슈테판 그리스 저, 최재웅 옮김, 󰡔언어학자를 위한 통계학 – R활용󰡕(고려대학교 출판문화원, 2013), pp. 272~275 참조.
## 카이제곱독립성검정에 대해서 위의 책 pp.258~272 참조.


