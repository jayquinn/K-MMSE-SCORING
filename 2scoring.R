#23/24
#score.SUM %>% quantile(probs = c(0.294944247))
cutoff = 0.294944247
dat<-read.csv("C:/git/K-MMSE-SCORING/mmse.csv",header=T,sep=",")
attach(dat) #코드 정리시작
#401 시간지남력 - 연월일 0123 
a401<-ifelse(C401==3,3,
             ifelse(C401==2,2,
                    ifelse(C401==1,1,
                           ifelse(C401==-8,NA,
                                  ifelse(C401==-9,0,
                                         ifelse(C401==5,0,NA))))))
#402 시간지남력 - 요일 01
a402<-ifelse(C402==1,1,
             ifelse(C402==-8,NA,
                    ifelse(C402==-9,0,
                           ifelse(C402==5,0,NA))))
#403 시간지남력- 계절 01
a403<-ifelse(C403==1,1,
             ifelse(C403==-8,NA,
                    ifelse(C403==-9,0,
                           ifelse(C403==5,0,NA))))

#404 장소지남력 - 현위치 01
a404<-ifelse(C404==1,1,
             ifelse(C404==-8,NA,
                    ifelse(C404==-9,0,
                           ifelse(C404==5,0,NA))))
#405 장소지남력 - 시/구/동/번지 01234
a405<-ifelse(C405==4,4,
             ifelse(C405==3,3,
                    ifelse(C405==2,2,
                           ifelse(C405==1,1,
                                  ifelse(C405==-8,NA,
                                         ifelse(C405==-9,0,
                                                ifelse(C405==5,0,NA)))))))

#406 기억력 테스트(3단어) 0123
a406<-ifelse(C406==3,3,
             ifelse(C406==2,2,
                    ifelse(C406==1,1,
                           ifelse(C406==-8,NA,
                                  ifelse(C406==-9,0,
                                         ifelse(C406==5,0,NA))))))
#407 주의 집중 및 계산 (뺄셈 1) 01
a407<-ifelse(C407==1,1,
             ifelse(C407==-8,NA,
                    ifelse(C407==-9,0,
                           ifelse(C407==5,0,NA))))
#408 주의 집중 및 계산 (뺄셈 2) 01
a408<-ifelse(C408==1,1,
             ifelse(C408==-8,NA,
                    ifelse(C408==-9,0,
                           ifelse(C408==5,0,NA))))
#409 주의 집중 및 계산 (뺄셈 3) 01
a409<-ifelse(C409==1,1,
             ifelse(C409==-8,NA,
                    ifelse(C409==-9,0,
                           ifelse(C409==5,0,NA))))
#410 주의 집중 및 계산 (뺄셈 4) 01
a410<-ifelse(C410==1,1,
             ifelse(C410==-8,NA,
                    ifelse(C410==-9,0,
                           ifelse(C410==5,0,NA))))
#411 주의 집중 및 계산 (뺄셈 5) 01
a411<-ifelse(C411==1,1,
             ifelse(C411==-8,NA,
                    ifelse(C411==-9,0,
                           ifelse(C411==5,0,NA))))


#412 기억력 테스트(3단어 재확인) 0123
a412<-ifelse(C412==3,3,
             ifelse(C412==2,2,
                    ifelse(C412==1,1,
                           ifelse(C412==-8,NA,
                                  ifelse(C412==-9,0,
                                         ifelse(C412==5,0,NA))))))
#413 소지품의 용도 (소지품 1) 01
a413<-ifelse(C413==1,1,
             ifelse(C413==-8,NA,
                    ifelse(C413==-9,0,
                           ifelse(C413==5,0,NA))))
#414 소지품의 용도 (소지품 2) 01
a414<-ifelse(C414==1,1,
             ifelse(C414==-8,NA,
                    ifelse(C414==-9,0,
                           ifelse(C414==5,0,NA))))

#415 따라서 말하기 01
a415<-ifelse(C415==1,1,
             ifelse(C415==-8,NA,
                    ifelse(C415==-9,0,
                           ifelse(C415==5,0,NA))))
#416 명령시행_종이 뒤집기, 접기, 건네주기 0123
a416<-ifelse(C416==3,3,
             ifelse(C416==2,2,
                    ifelse(C416==1,1,
                           ifelse(C416==-8,NA,
                                  ifelse(C416==-9,0,
                                         ifelse(C416==5,0,NA))))))
#417 명령시행_읽고 눈감기 01 #코드상 읽기 1, 눈감기 3으로 되어있는데 원전상 눈을 감아야만 점수 획득임.
a417<-ifelse(C417==3,1,
             ifelse(C417==1,0,
                    ifelse(C417==-8,NA,
                           ifelse(C417==-9,0,
                                  ifelse(C417==5,0,NA)))))
#418 명령시행_기분 또는 날씨에 대해 쓰기 01
a418<-ifelse(C418==1,1,
             ifelse(C418==-8,NA,
                    ifelse(C418==-9,0,
                           ifelse(C418==5,0,NA))))
#419 명령시행_제시된 그림 똑같이 그리기01
a419<-ifelse(C419==1,1,
             ifelse(C419==-8,NA,
                    ifelse(C419==-9,0,
                           ifelse(C419==5,0,NA))))
#420 도움받은정도 -9 모르겠음 -8 응답거부 1도움전혀받지않음 2가끔 3대부분
a420<-ifelse(C420==3,3,
             ifelse(C420==2,2,
                    ifelse(C420==1,1,
                           ifelse(C420==-8,NA,
                                  ifelse(C420==-9,NA,NA)))))
#취합
response<-data.frame(a401,a402,a403,a404,a405,a406,a407,a408,a409,a410,a411,a412,a413,a414,a415,a416,a417,a418,a419)
#인지기능 저하자 마킹
response$diag<-ifelse(dat$diag==5,0,
                      ifelse(dat$diag==3,1,
                             ifelse(dat$diag==1,1,dat$diag))) #5 치매아님 -9 모르겠음 -8 응답거부 1 치매 3 경도인지장애
#나이 입력
response$age = 2018-dat$year
response$gender = dat$gender # 1 = 남자, 5 = 여자
#검사 응답이 모두 NA인 행제거
response = response %>% filter(complete.cases(.))
detach(dat)
colSums(is.na(response))
#19문항
response19<-response
#5문항
response05<-response %>% mutate(a401 = a401+a402+a403+a404+a405,
                                a402 = a406,
                                a403 = a407+a408+a409+a410+a411,
                                a404 = a412,
                                a405 = a413+a414+a415+a416+a417+a418+a419,
                                .keep = "unused")
############################
#####점수산출시작###########
############################

#SUM score
score.SUM<-vector("double",nrow(response05))
for ( i in 1:nrow(response05) ){
  score.SUM[[i]]<-sum(response05[i,1:5],na.rm=T)}

#CFA factor score
model.cfa<-'F1 =~ a401 + a402 + a403 + a404 + a405'
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
score.frame<-cbind(score.SUM,score.CFA,score.PCM,score.GPCM,response$diag,response$age,response$gender); colnames(score.frame)<-c("SUM","CFA","PCM","GPCM","diag","age","gender")
as.data.frame(score.frame) -> score.frame

#23/24기준에 마커
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
                                   age >= 55 & age < 65 ~ '1')) -> sf
sf <- mutate_at(sf, vars(starts_with("marker")), as.factor)
sf$diag <- as.factor(sf$diag)
sf$gender <- as.factor(sf$gender)
