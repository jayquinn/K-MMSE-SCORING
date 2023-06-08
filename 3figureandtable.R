#(그림 1) 특질 수준과 원점수 간 선형 함수 및 로지스틱 함수
p <- ggplot(data = data.frame(x = c(-3, 3)), aes(x))
p + stat_function(fun = function(x) 30/(1+exp(-x)), n = 100)  + geom_hline(yintercept = c(0,30), linetype = 'solid') + 
  stat_function(fun = function(x) 15+(7.5*x), n = 100) + 
  scale_x_continuous(name = "특질 수준",breaks = seq(-3,3,by = 1)) + 
  scale_y_continuous(name = "원점수",breaks = seq(0,30,by = 5),limits = c(0,30)) + theme_bw()+
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_segment(aes(x = -2,y = 0,xend = -1,yend = 0),arrow = arrow(end = "both",type = "closed",length = unit(0.25, "cm")),size = 0.5) + 
  geom_segment(aes(x = -3.3,y = 30/(1+exp(-(-2))),xend = -3.3,yend = 30/(1+exp(-(-1)))),arrow = arrow(end = "both",type = "closed",length = unit(0.25, "cm")),size = 0.5) +
  geom_segment(aes(x = 0,y = 0,xend = 1,yend = 0),arrow = arrow(end = "both",type = "open",length = unit(0.25, "cm")),size = 0.5) + 
  geom_segment(aes(x = -3.3,y = 15+(7.5*(-0))),xend = -3.3,yend = 15+(7.5*(1)),arrow = arrow(end = "both",type = "open",length = unit(0.25, "cm")),size = 0.5) +
  geom_segment(aes(x = -1,y = 0,xend = -0,yend = 0),arrow = arrow(end = "both",type = "closed",length = unit(0.25, "cm")),size = 0.5) + 
  geom_segment(aes(x = -3.1,y = 30/(1+exp(-(-1))),xend = -3.1,yend = 15+(7.5*(-0))),arrow = arrow(end = "both",type = "closed",length = unit(0.25, "cm")),size = 0.5) + 
  geom_segment(aes(x = 1,y = 0,xend = 2,yend = 0),arrow = arrow(end = "both",type = "open",length = unit(0.25, "cm")),size = 0.5) + 
  geom_segment(aes(x = -3.1,y = 15+(7.5*(1)),xend = -3.1,yend = 15+(7.5*(2))),arrow = arrow(end = "both",type = "open",length = unit(0.25, "cm")),size = 0.5) +
  geom_segment(aes(x = -2,y = 30/(1+exp(-(-2))),xend = -3.3,yend = 30/(1+exp(-(-2)))),linetype=2) +
  geom_segment(aes(x = -2,y = 0,xend = -2,yend = 30/(1+exp(-(-2)))),linetype=2) + 
  geom_segment(aes(x = -1,y = 30/(1+exp(-(-1))),xend = -3.3,yend = 30/(1+exp(-(-1)))),linetype=2) +
  geom_segment(aes(x = -1,y = 0,xend = -1,yend = 30/(1+exp(-(-1)))),linetype=2) + 
  geom_segment(aes(x = 0,y = 30/(1+exp(-(0))),xend = -3.3,yend = 30/(1+exp(-(0)))),linetype=2) +
  geom_segment(aes(x = 0,y = 0,xend = 0,yend = 30/(1+exp(-(0)))),linetype=2) + 
  geom_segment(aes(x = 1,y = 0,xend = 1,yend = 30/(1+exp(-(1)))),linetype=2) + 
  geom_segment(aes(x = -1,y = 0,xend = -1,yend = 15+(7.5*(-1))),linetype=2) + 
  geom_segment(aes(x = 0,y = 15+(7.5*(0)),xend = -3.1,yend = 15+(7.5*(0))),linetype=2) +
  geom_segment(aes(x = 0,y = 0,xend = 0,yend = 15+(7.5*(0))),linetype=2) +
  geom_segment(aes(x = 1,y = 15+(7.5*(1)),xend = -3.3,yend = 15+(7.5*(1))),linetype=2) +
  geom_segment(aes(x = 1,y = 0,xend = 1,yend = 15+(7.5*(1))),linetype=2) +
  geom_segment(aes(x = 2,y = 0,xend = 2,yend = 15+(7.5*(2))),linetype=2) + 
  annotate("text", x = -1.5, y = 0.75, label = "A", fontface=2, size = 4) + 
  annotate("text", x = -0.5, y = 0.75, label = "B", fontface=2, size = 4) + 
  annotate("text", x = 0.5, y = 0.75, label = "C", fontface=2, size = 4) + 
  annotate("text", x = 1.5, y = 0.75, label = "D", fontface=2, size = 4) + 
  annotate("text", x = -3.5, y = 30/(1+exp(-(-2))) + (30/(1+exp(-(-1))) - 30/(1+exp(-(-2))))/2, label = "A'", fontface=2, size = 4) + 
  annotate("text", x = -3.5, y = 15+(7.5*(-1)) + ((15+(7.5*(0))) - (15+(7.5*(-1))))/2, label = "B'", fontface=2, size = 4) + 
  annotate("text", x = -3.5, y = 30/(1+exp(-(0))) + (30/(1+exp(-(1))) - 30/(1+exp(-(0))))/2, label = "C'", fontface=2, size = 4) + 
  annotate("text", x = -3.5, y = 15+(7.5*(1)) + ((15+(7.5*(2))) - (15+(7.5*(1))))/2, label = "D'", fontface=2, size = 4) +
  theme(text = element_text(size = 15))

#(그림 1) 대안
p <- ggplot(data = data.frame(x = c(-3, 3)), aes(x))
p + stat_function(fun = function(x) 30/(1+exp(-x)), n = 100)  + geom_hline(yintercept = c(0,30), linetype = 'solid') + 
  stat_function(fun = function(x) 15+(7.5*x), n = 100) + 
  #테마 시작
  scale_x_continuous(name = "특질 수준",breaks = seq(-3,3,by = 1)) + 
  scale_y_continuous(name = "원점수",breaks = seq(0,30,by = 5),limits = c(0,30)) + theme_bw() +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 15)) +
  #테마 끝
  #X축 ABCD 시작
  geom_segment(aes(x = -2,y = 0,xend = -1,yend = 0),arrow = arrow(end = "both",type = "open",length = unit(0.25, "cm")),size = 0.5) +
  geom_segment(aes(x = -2,y = 0,xend = -1,yend = 0),arrow = arrow(end = "both",type = "open",length = unit(0.25, "cm")),size = 0.5) + 
  geom_segment(aes(x = 0,y = 0,xend = 1,yend = 0),arrow = arrow(end = "both",type = "closed",length = unit(0.25, "cm")),size = 0.5) + 
  geom_segment(aes(x = -1,y = 0,xend = -0,yend = 0),arrow = arrow(end = "both",type = "open",length = unit(0.25, "cm")),size = 0.5) + 
  geom_segment(aes(x = 1,y = 0,xend = 2,yend = 0),arrow = arrow(end = "both",type = "closed",length = unit(0.25, "cm")),size = 0.5) + 
  annotate("text", x = -1.5, y = 0.75, label = "A", fontface=2, size = 4) + 
  annotate("text", x = -0.5, y = 0.75, label = "B", fontface=2, size = 4) + 
  annotate("text", x = 0.5, y = 0.75, label = "C", fontface=2, size = 4) + 
  annotate("text", x = 1.5, y = 0.75, label = "D", fontface=2, size = 4) + 
  #X축 ABCD 끝
  #세로선 시작
  geom_segment(aes(x = -1,y = 0,xend = -1,yend = 15+(7.5*(-1))),linetype=2) + 
  geom_segment(aes(x = 0,y = 0,xend = 0,yend = 15+(7.5*(0))),linetype=2) +
  geom_segment(aes(x = 1,y = 0,xend = 1,yend = 30/(1+exp(-(1)))),linetype=2) + 
  geom_segment(aes(x = 2,y = 0,xend = 2,yend = 30/(1+exp(-(2)))),linetype=2) + 
  #세로선 끝
  #가로선 시작
  geom_segment(aes(x = -1,y = 15+(7.5*(-1)),xend = -3.3,yend = 15+(7.5*(-1))),linetype=2) +
  geom_segment(aes(x = 0,y = 15+(7.5*(0)),xend = -3.3,yend = 15+(7.5*(0))),linetype=2) + 
  geom_segment(aes(x = 1,y = 30/(1+exp(-(1))),xend = -3.3,yend = 30/(1+exp(-(1)))),linetype=2) + 
  geom_segment(aes(x = 2,y = 30/(1+exp(-(2))),xend = -3.3,yend = 30/(1+exp(-(2)))),linetype=2) +
  #가로선 끝
  #Y축 A'B'C'D 시작
  geom_segment(aes(x = -3.1,y = 0,xend = -3.1,yend = 15+(7.5*(-1))),arrow = arrow(end = "both",type = "open",length = unit(0.25, "cm")),size = 0.5) +
  geom_segment(aes(x = -3.3,y = 15+(7.5*(-1))),xend = -3.3,yend = 15+(7.5*(0)),arrow = arrow(end = "both",type = "open",length = unit(0.25, "cm")),size = 0.5) +
  geom_segment(aes(x = -3.1,y = 30/(1+exp(-(0))),xend = -3.1,yend = 30/(1+exp(-(1)))),arrow = arrow(end = "both",type = "closed",length = unit(0.25, "cm")),size = 0.5) +
  geom_segment(aes(x = -3.3,y = 30/(1+exp(-(1))),xend = -3.3,yend = 30/(1+exp(-(2)))),arrow = arrow(end = "both",type = "closed",length = unit(0.25, "cm")),size = 0.5) +
  annotate("text", x = -3.5, y = 15+(7.5*(-2)) + ((15+(7.5*(-1))) - (15+(7.5*(-2))))/2, label = "A'", fontface=2, size = 4) + 
  annotate("text", x = -3.5, y = 15+(7.5*(-1)) + ((15+(7.5*(0))) - (15+(7.5*(-1))))/2, label = "B'", fontface=2, size = 4) + 
  annotate("text", x = -3.5, y = 30/(1+exp(-(0))) + (30/(1+exp(-(1))) - 30/(1+exp(-(0))))/2, label = "C'", fontface=2, size = 4) + 
  annotate("text", x = -3.5, y = 30/(1+exp(-(1))) + (30/(1+exp(-(2))) - 30/(1+exp(-(1))))/2, label = "D'", fontface=2, size = 4)
#Y축 A'B'C'D 끝


#(그림 2) 세 범주를 갖는 문항의 범주반응함수 
p <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) 
p +
  stat_function(fun = function(x) 1/(1+8*exp(-2*x)), n = 100) + stat_function(fun = function(x) 1/(1+8*exp(2*x)), n = 100) + stat_function(fun = dnorm, args=list(mean=0, sd=1),n = 100) + 
  scale_x_continuous(name = "특질 수준",breaks = seq(-3,3,by = 1)) + scale_y_continuous(name = "반응확률",breaks = seq(0,1,by = 0.5),limits = c(0,1)) + theme_bw() + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_segment(aes(x = -0.66,y = 0.5,xend = -0.66,yend = 0.35),arrow = arrow(type = "open",length = unit(0.25, "cm")),size = 0.5) + 
  geom_segment(aes(x = 0.66,y = 0.5,xend = 0.66,yend = 0.35),arrow = arrow(type = "open",length = unit(0.25, "cm")),size = 0.5) + 
  annotate(geom='text', x=-0.66, y=0.525, size=5, label = bquote(~delta[1])) + 
  annotate(geom='text', x=0.66, y=0.525, size=5,label = bquote(~delta[2])) + 
  annotate(geom='text', x=-2, y=0.95, size=5,label = "0") +
  annotate(geom='text', x=0, y=0.45, size=5,label = "1") +
  annotate(geom='text', x=2, y=0.95, size=5,label = "2") +
  theme(text = element_text(size = 20))


#(그림 3) 전체 표본 및 연령집단별 K-MMSE 총점 분포
A = ggplot(data = sf, aes(x = SUM)) + 
  theme_bw() + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + theme(text = element_text(size = 15)) + 
  theme(axis.title.y = element_text(margin = margin()))+
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", bins = 30) + 
  stat_function(fun = function(x) dnorm(x, mean = mean(sf$SUM), sd = sd(sf$SUM)) * nrow(sf) * 1.7) +
  scale_x_continuous("총점") + #n 뒤에 ,color = "black", size =  //  걍 이거 해 ,limits = c(-8.5,4)
  scale_y_continuous("빈도",sec.axis=sec_axis(
    trans = ~./(max(table(sf$SUM)) / max(density(sf$SUM)$y)),name = "밀도"))

myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%select(SUM)%>%skew(),digits = 2)))
B = ggplot(data = sf, aes(sample = SUM)) + stat_qq()  + theme_bw() +  stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_y_continuous("표본 분위수") + 
  scale_x_continuous("정규분포 분위수")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) + 
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.3,
           y = 3.5,
           size = 5.5)
TOPA = plot_grid(A,B,ncol=2,rel_widths = c(2.9,1),rel_heights = 0.5)

#SUM boxplot + qq
A = ggplot(sf,aes(x = agegroup,y = SUM)) + theme(axis.text = element_text(size = 15)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", position=position_dodge(.75), 
               color="black", size=2) + theme_bw() + 
  scale_y_continuous(name = "총점") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text=element_text(size=13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = c("55~64", "65~74","75~84","85~"), name = "연령")


myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==1)%>%select(SUM)%>%skew(),digits = 2)))
B = ggplot(data = filter(sf,agegroup == 1), aes(sample = SUM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("55~64")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + 
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.1,
           y = 4,
           size = 5.5)
myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==2)%>%select(SUM)%>%skew(),digits = 2)))
C = ggplot(data = filter(sf, agegroup == 2), aes(sample = SUM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("65~74")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + 
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.1,
           y = 4,
           size = 5.5)
myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==3)%>%select(SUM)%>%skew(),digits = 1)))
D = ggplot(data = filter(sf, agegroup == 3), aes(sample = SUM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("75~84")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + 
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.1,
           y = 4.5,
           size = 5.5)
myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==4)%>%select(SUM)%>%skew(),digits = 1)))
E = ggplot(data = filter(sf, agegroup == 4), aes(sample = SUM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("85~")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 0.9,
           y = -6.5,
           size = 5.5)
posterA = grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4), nrow = 2, heights = c(0.5,0.5))
grid.arrange(TOPA,posterA, ncol = 1, heights = c(0.25,0.5))

#(그림4) 전체 표본 및 연령집단별 K-MMSE 요인점수 분포
A = ggplot(data = sf, aes(x = CFA)) + 
  theme_bw() + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + theme(text = element_text(size = 15)) +
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", bins = 30) + 
  stat_function(fun = function(x) dnorm(x, mean = mean(sf$CFA), sd = sd(sf$CFA)) * nrow(sf) * 0.3) +
  scale_x_continuous("요인점수") + #n 뒤에 ,color = "black", size =  //  걍 이거 해 ,limits = c(-8.5,4)
  scale_y_continuous("빈도",
                     sec.axis=sec_axis(trans = ~./(max(table(sf$CFA)) / max(density(sf$CFA)$y)),name = "밀도"))



myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%select(CFA)%>%skew(),digits = 2)))
B = ggplot(data = sf, aes(sample = CFA)) + stat_qq()  + theme_bw() +  stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_y_continuous("표본 분위수") + 
  scale_x_continuous("정규분포 분위수")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) + 
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.1,
           y = -4,
           size = 5.5)
TOPA = plot_grid(A,B,ncol=2,rel_widths = c(2.9,1),rel_heights = 0.5)

#CFA boxplot + qq
A = ggplot(sf,aes(x = agegroup,y = CFA)) + theme(axis.text = element_text(size = 15)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", position=position_dodge(.75), 
               color="black", size=2) + theme_bw() + 
  scale_y_continuous(name = "요인점수") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text=element_text(size=13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = c("55~64", "65~74","75~84","85~"), name = "연령")

myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==1)%>%select(CFA)%>%skew(),digits = 2)))
B = ggplot(data = filter(sf,agegroup == 1), aes(sample = CFA)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("55~64")+
  theme(axis.text = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.1,
           y = -3.9,
           size = 5.5)


myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==2)%>%select(CFA)%>%skew(),digits = 2)))
C = ggplot(data = filter(sf, agegroup == 2), aes(sample = CFA)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("65~74")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.1,
           y = -3.8,
           size = 5.5)

myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==3)%>%select(CFA)%>%skew(),digits = 1)))
D = ggplot(data = filter(sf, agegroup == 3), aes(sample = CFA)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("75~84")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.1,
           y = -3.8,
           size = 5.5)

myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==4)%>%select(CFA)%>%skew(),digits = 1)))
E = ggplot(data = filter(sf, agegroup == 4), aes(sample = CFA)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("85~")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1,
           y = -5.55,
           size = 5.5)

posterA = grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4), nrow = 2, heights = c(0.5,0.5))
grid.arrange(TOPA,posterA, ncol = 1, heights = c(0.25,0.5))


#(그림 5) K-MMSE 부분점수모형 및 일반화부분점수모형의 검사정보함수
Theta <- matrix(seq(-4,1,.01))
pcm.info <- testinfo(results.pcm, Theta)
gpcm.info = testinfo(results.gpcm,Theta)
plot(Theta, pcm.info,ylim = c(0,30),cex.axis=1.2,
     type = 'l',ylab = "", xlab="")
par(new=TRUE)
plot(Theta,gpcm.info,type = 'l',cex = 1,cex.axis=1.2,
     lty = "dashed",ylim = c(0,30),ylab = bquote(~I(theta)), xlab=bquote(~theta))
legend("topleft",legend = c("PCM","GPCM"),
       cex=0.6,lty=c(1,2),text.font=4.5)


#(그림 6) 전체 표본 및 연령집단별 K-MMSE PCM특질점수 분포
A = ggplot(data = sf, aes(x = PCM)) + 
  theme_bw() + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + theme(text = element_text(size = 15)) +
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", binwidth = 0.37) + #bins = 30, binwidth = 0.33
  stat_function(fun = function(x) dnorm(x, mean = mean(sf$PCM), sd = sd(sf$PCM)) * nrow(sf) * 0.38) +
  scale_x_continuous("PCM특질점수") + #n 뒤에 ,color = "black", size =  //  걍 이거 해 ,limits = c(-8.5,4)
  scale_y_continuous("빈도",sec.axis=sec_axis(
    trans = ~./(max(table(sf$PCM)) / max(density(sf$PCM)$y)),name = "밀도"))

myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%select(PCM)%>%skew(),digits = 2)))
B = ggplot(data = sf, aes(sample = PCM)) + stat_qq()  + theme_bw() +  stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_y_continuous("표본 분위수") + 
  scale_x_continuous("정규분포 분위수")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) + 
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.1,
           y = -3,
           size = 5.5)
TOPA = plot_grid(A,B,ncol=2,rel_widths = c(2.9,1),rel_heights = 0.5)

#PCM boxplot + qq
A = ggplot(sf,aes(x = agegroup,y = PCM)) + theme(axis.text = element_text(size = 15)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", position=position_dodge(.75), 
               color="black", size=2) + theme_bw() + 
  scale_y_continuous(name = "PCM특질점수") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text=element_text(size=13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = c("55~64", "65~74","75~84","85~"), name = "연령")

myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==1)%>%select(PCM)%>%skew(),digits = 1)))
B = ggplot(data = filter(sf,agegroup == 1), aes(sample = PCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("55~64")+
  theme(axis.text = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.35,
           y = -2.85,
           size = 5.5)


myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==2)%>%select(PCM)%>%skew(),digits = 1)))
C = ggplot(data = filter(sf, agegroup == 2), aes(sample = PCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("65~74")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.35,
           y = -2.95,
           size = 5.5)

#myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==3)%>%select(PCM)%>%skew(),digits = 1)))
myexpr = substitute(italic(g[1])==k,list(k=-0.1))
D = ggplot(data = filter(sf, agegroup == 3), aes(sample = PCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("75~84")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1,
           y = -3,
           size = 5.5)

#myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==4)%>%select(PCM)%>%skew(),digits = 1)))
myexpr = substitute(italic(g[1])==k,list(k=0.1))
E = ggplot(data = filter(sf, agegroup == 4), aes(sample = PCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("85~")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.1,
           y = -3.45,
           size = 5.5)

posterA = grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4), nrow = 2, heights = c(0.5,0.5))
grid.arrange(TOPA,posterA, ncol = 1, heights = c(0.25,0.5))



#(그림 7) 전체 표본 및 연령집단별 K-MMSE GPCM특질점수 분포
A = ggplot(data = sf, aes(x = GPCM)) + 
  theme_bw() + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + theme(text = element_text(size = 15)) +
  geom_histogram(aes(y = ..count..), colour = 1, fill = "white", binwidth = 0.38)+ #bins = 30
  stat_function(fun = function(x) dnorm(x, mean = mean(sf$GPCM), sd = sd(sf$GPCM)) * nrow(sf) * 0.38) +
  scale_x_continuous("GPCM특질점수") + #n 뒤에 ,color = "black", size =  //  걍 이거 해 ,limits = c(-8.5,4)
  scale_y_continuous("빈도",sec.axis=sec_axis(
    trans = ~./(max(table(sf$GPCM)) / max(density(sf$GPCM)$y)),name = "밀도"))

myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%select(GPCM)%>%skew(),digits = 1)))
B = ggplot(data = sf, aes(sample = GPCM)) + stat_qq()  + theme_bw() +  stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_y_continuous("표본 분위수") + 
  scale_x_continuous("정규분포 분위수")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) + 
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.25,
           y = -3.3,
           size = 5.5)
TOPA = plot_grid(A,B,ncol=2,rel_widths = c(2.9,1),rel_heights = 0.5)

#GPCM boxplot + qq
A = ggplot(sf,aes(x = agegroup,y = GPCM)) + theme(axis.text = element_text(size = 15)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", position=position_dodge(.75), 
               color="black", size=2) + theme_bw() + 
  scale_y_continuous(name = "GPCM특질점수") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text=element_text(size=13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = c("55~64", "65~74","75~84","85~"), name = "연령")

myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==1)%>%select(GPCM)%>%skew(),digits = 1)))
B = ggplot(data = filter(sf,agegroup == 1), aes(sample = GPCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("55~64")+
  theme(axis.text = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.35,
           y = -2.9,
           size = 5.5)


myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==2)%>%select(GPCM)%>%skew(),digits = 1)))
C = ggplot(data = filter(sf, agegroup == 2), aes(sample = GPCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("65~74")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.35,
           y = -2.9,
           size = 5.5)

#myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==3)%>%select(GPCM)%>%skew(),digits = 1)))
myexpr = substitute(italic(g[1])==k,list(k=0))
D = ggplot(data = filter(sf, agegroup == 3), aes(sample = GPCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("75~84")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.5,
           y = -2.9,
           size = 5.5)

myexpr = substitute(italic(g[1])==k,list(k=format(sf%>%filter(agegroup==4)%>%select(GPCM)%>%skew(),digits = 1)))
E = ggplot(data = filter(sf, agegroup == 4), aes(sample = GPCM)) + stat_qq()  + theme_bw() + stat_qq_line() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(" ") + 
  scale_x_continuous(" ") + ggtitle("85~")+
  theme(axis.text = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "text",
           label = deparse(myexpr),
           parse=TRUE,
           x = 1.3,
           y = -3.35,
           size = 5.5)

posterA = grid.arrange(A, arrangeGrob(B,C,D,E,ncol=4), nrow = 2, heights = c(0.5,0.5))
grid.arrange(TOPA,posterA, ncol = 1, heights = c(0.25,0.5))

#(그림 8) 전체 표본 K-MMSE 검사점수 간 피어슨 상관계수 및 카파 계수
lower.panel = function(x,y){
  points(x = x, y = y, pch = 1, cex = 0.65)
  fit<-loess.smooth(x=x,y=y)
  abline(v = quantile(x,cutoff))
  abline(h = quantile(y,cutoff))
}
upper.panel = function(x,y){
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0,1,0,1))
  pearson = round(cor(x,y),3)
  kappa = round(Kappa(matrix(c(nrow(sf[,1:4] %>% filter(x > quantile(x,cutoff) & y > quantile(y, cutoff))),
                               nrow(sf[,1:4] %>% filter(x > quantile(x,cutoff) & y <= quantile(y, cutoff))),
                               nrow(sf[,1:4] %>% filter(x <= quantile(x,cutoff) & y > quantile(y, cutoff))),
                               nrow(sf[,1:4] %>% filter(x <= quantile(x,cutoff) & y <= quantile(y, cutoff)))),ncol=2))[[1]][1],3)
  txtp = paste0("r = ",pearson)
  text(0.5,0.6,txtp,cex = 2)
  text(0.5,0.4,as.expression(bquote(~kappa == .(kappa))),cex = 2)
}
pairs(sf[,1:4],
      upper.panel = upper.panel,
      lower.panel = lower.panel)

#(그림 9) 연령집단에 따른 K-MMSE 검사점수 간 피어슨 상관계수
sf %>% group_by(agegroup) %>% summarise("SUM-CFA" = cor(SUM,CFA),
                                        "SUM-PCM" = cor(SUM,PCM),
                                        "SUM-GPCM" = cor(SUM,GPCM),
                                        "CFA-PCM" = cor(CFA,PCM),
                                        "CFA-GPCM" = cor(CFA,GPCM),
                                        "PCM-GPCM" = cor(PCM,GPCM)) %>% 
  pivot_longer(cols = "SUM-CFA":"PCM-GPCM") -> tablep
tablep %>% filter(agegroup == 1) -> pstarts
tablep$name = factor(tablep$name, levels = c("SUM-CFA","SUM-PCM","SUM-GPCM",
                                             "CFA-PCM","CFA-GPCM","PCM-GPCM"))
ggplot(data = tablep, aes(x = agegroup, y = value, group = name)) + 
  geom_line(aes(linetype=name)) +
  scale_shape_manual(values=1:nlevels(as.factor(tablep$name))) + 
  geom_point(aes(shape=name)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  geom_point(aes(shape = factor(name)), size = 3,stroke = 1.4) + 
  theme(legend.key.width = unit(2,'cm')) +
  theme(text = element_text(size = 18)) + 
  scale_y_continuous(name = "피어슨 상관계수") +
  scale_x_discrete(labels = c("55~64", "65~74","75~84","85~"), name = "연령") +
  geom_text_repel(
    aes(label = name), data = pstarts,
    fontface ="plain", color = "black", size = 5
  )

#(그림 10) 연령집단에 따른 K-MMSE 검사점수 간 카파 계수
sf %>% group_by(agegroup) %>% summarise("SUM-CFA" = round(confusionMatrix(markerSUM,markerCFA)[[3]][[2]],3),
                                        "SUM-PCM" = round(confusionMatrix(markerSUM,markerPCM)[[3]][[2]],3),
                                        "SUM-GPCM" = round(confusionMatrix(markerSUM,markerGPCM)[[3]][[2]],3),
                                        "CFA-PCM" = round(confusionMatrix(markerCFA,markerPCM)[[3]][[2]],3),
                                        "CFA-GPCM" = round(confusionMatrix(markerCFA,markerGPCM)[[3]][[2]],3),
                                        "PCM-GPCM" = round(confusionMatrix(markerPCM,markerGPCM)[[3]][[2]],3)) %>% 
  pivot_longer(cols = "SUM-CFA":"PCM-GPCM") -> tablekp
tablekp %>% filter(agegroup == 1) -> kpstarts
tablekp$name = factor(tablekp$name, levels = c("SUM-CFA","SUM-PCM","SUM-GPCM","CFA-PCM","CFA-GPCM","PCM-GPCM"))
ggplot(data = tablekp, aes(x = agegroup, y = value, group = name)) + 
  geom_line(aes(linetype=name)) + 
  scale_shape_manual(values=1:nlevels(as.factor(tablep$name))) + 
  geom_point(aes(shape=name)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_point(aes(shape = factor(name)), size = 3,stroke = 1.4) + 
  theme(legend.key.width = unit(2,'cm')) +
  theme(text = element_text(size = 20)) + 
  scale_y_continuous(name = "카파 계수") +
  scale_x_discrete(labels = c("55~64", "65~74","75~84","85~"), name = "연령")+
  geom_text_repel(
    aes(label = name), data = kpstarts,
    fontface ="plain", color = "black", size = 5
  )



#(표 1) 연령집단별 남녀 표본 수
sf %>% group_by(agegroup, gender) %>% summarise(num = n())
sf %>% group_by(agegroup) %>% summarise(num = n())

#(표 2) MMSE의 측정영역 및 문항 구성
#코드 요구되지 않음

#(표 3) K-MMSE 측정영역점수 기술통계
describe(response05[,1:5])#총점 기술통계
alpha(response05[,1:5])#총점 문항총점상관, 알파
results.cfa %>% lavInspect(what = "std") #CFA 표준화된 요인부하량(lambda)
(score.CFA<-lavPredict(results.cfa, 
                       method = "regression",
                       fsm = T)) #CFA 회귀계수(fsm)

#(표 4) K-MMSE 부분점수모형의 문항 특성
coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
itemfit(results.pcm,'infit') #문항 적합도

#(표 5) K-MMSE 일반화부분점수모형의 문항 특성
coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
itemfit(results.gpcm,'infit') #문항 적합도


#부록 I. K-MMSE 부분점수 모형의 문항특성곡선 및 검사특성곡선
for(i in 1:19) {
  assign(paste0("plot_",i),itemplot(results.pcm,i, type = 'trace', main = paste0("문항",i) , auto.key = none, par.settings = bwtheme))
}
plot_20 = plot(results.pcm, type = 'score',main = "검사특성곡선", theta_lim = c(-6,6), lwd=1,par.settings=bwtheme)
grid.arrange(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,plot_9,plot_10,plot_11,plot_12,plot_13,plot_14,plot_15,plot_16,plot_17,plot_18,plot_19,plot_20,ncol=5,vp=viewport(width=1, height=0.95))

#부록 II. K-MMSE 일반화부분점수 모형의 문항특성곡선 및 검사특성곡선
for(i in 1:19) {
  assign(paste0("plot_",i),itemplot(results.gpcm,i, type = 'trace', main = paste0("문항",i) , auto.key = none, par.settings = bwtheme))
}
plot_20 = plot(results.gpcm, type = 'score',main = "검사특성곡선", theta_lim = c(-6,6), lwd=1,par.settings=bwtheme)
grid.arrange(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,plot_9,plot_10,plot_11,plot_12,plot_13,plot_14,plot_15,plot_16,plot_17,plot_18,plot_19,plot_20,ncol=5,vp=viewport(width=1, height=0.95))


#그외
anova(results.pcm,results.gpcm) #PCM-GPCM 우도비검정

