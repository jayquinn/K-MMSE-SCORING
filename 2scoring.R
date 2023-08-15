rawdata = read.csv("mmse.csv",header=T,sep=",")
rawdata %>% select(C401:C419,year,gender) -> dat
#C417: Klosa자료상 읽기성공시 1, 눈감기 성공시 3으로 코딩되어 2점 만점을 갖는 문항으로 입력되어있지만
#MMSE의 채점 규칙에 따라 눈감기까지 성공해야만 점수 획득하는 1점 만점을 갖는 이분 문항으로 수정하여 사용함. 
dat$C417<-ifelse(dat$C417==3,1,
                     ifelse(dat$C417==1,0,dat$C417))
#데이터 변환
dat %>% 
  mutate(across(starts_with("c"),
                ~ case_when(
                  .==-8 ~ NA_real_,
                  .==-9 ~ 0,
                  .== 5 ~ 0,
                  TRUE ~ .)),
         age = 2018 - year,.keep = "unused") %>%
  filter(complete.cases(across(starts_with("c")))) -> response

# 5 오답
# -9 모르겠음: 0 처리
# -8 응답거부: NA 처리
#19문항
response19<-response
#5문항
response05<-response %>% mutate(C401 = C401+C402+C403+C404+C405,
                                C402 = C406,
                                C403 = C407+C408+C409+C410+C411,
                                C404 = C412,
                                C405 = C413+C414+C415+C416+C417+C418+C419,
                                .keep = "unused")
############################
#####점수산출시작###########
############################

#SUM score
score.SUM = rowSums(response05[,1:5])
#CFA factor score
model.cfa<-'F1 =~ C401 + C402 + C403 + C404 + C405'
results.cfa<-cfa(model=model.cfa,data = response05[,1:5],
                 estimator = "MLM")
summary(results.cfa, fit.measures = T,standardized = T, rsquare=TRUE)
score.CFA<-lavPredict(results.cfa,method = "regression",fsm = T)

#PCM
model.pcm <- 'F1 = 1-19
              CONSTRAIN = (1-19, a1)' 
results.pcm <- mirt(data=response19[,1:19], model=model.pcm, itemtype="gpcm", SE=TRUE, verbose=FALSE)
coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
score.PCM<-fscores(results.pcm,method = 'EAP')
itemfit.PCM <- itemfit(results.pcm,'infit')
M2(results.pcm)

#GPCM
model.gpcm <- 'F1 = 1-19' 
results.gpcm <- mirt(data=response19[,1:19], model=model.gpcm, itemtype="gpcm", SE=TRUE, verbose=FALSE)
coef.gpcm <- coef(results.gpcm,IRTpars=TRUE,simplify = T)
score.GPCM<-fscores(results.gpcm,method = 'EAP')
itemfit.GPCM <- itemfit(results.gpcm,'infit')
M2(results.gpcm)

#점수 취합
score.frame<-cbind(score.SUM,score.CFA,score.PCM,score.GPCM,response$age,response$gender)
colnames(score.frame)<-c("SUM","CFA","PCM","GPCM","age","gender")
as.data.frame(score.frame) -> score.frame

#23/24 기준으로 기준점 설정
cutoff = mean(score.frame$SUM < 24)

#23/24 기준으로 마커
score.frame %>% mutate(markerSUM = case_when(SUM <= quantile(score.frame$SUM,cutoff) ~ '1',
                                             SUM > quantile(score.frame$SUM,cutoff) ~ '0'),
                       markerCFA = case_when(CFA <= quantile(score.frame$CFA,cutoff) ~ '1',
                                             CFA > quantile(score.frame$CFA,cutoff) ~ '0'),
                       markerPCM = case_when(PCM <= quantile(score.frame$PCM,cutoff) ~ '1',
                                             PCM > quantile(score.frame$PCM,cutoff) ~ '0'),
                       markerGPCM = case_when(GPCM <= quantile(score.frame$GPCM,cutoff) ~ '1',
                                              GPCM > quantile(score.frame$GPCM,cutoff) ~ '0')) -> sf

sf %>% mutate(agegroup = case_when(age >= 85 ~ '4',
                                   age >= 75 & age < 85 ~ '3',
                                   age >= 65 & age < 75 ~ '2',
                                   age >= 55 & age < 65 ~ '1')) %>%
  mutate_at(vars(c(starts_with("marker"), "gender","agegroup")), as.factor) -> sf
