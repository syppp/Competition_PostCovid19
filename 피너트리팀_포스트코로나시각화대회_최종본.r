install.packages(c("rgdal","ggmap","sp","maptools","viridis","magrittr","scales","gridExtra","data.table",
                  "tidyverse","lubridate","factoextra","tfplot","tsfa","cluster","IRdisplay","foreign","extrafont",
                  "showtext","grid"))

suppressPackageStartupMessages({ 
    library(rgdal,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(ggmap,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(sp,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(maptools,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(viridis,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(magrittr,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(scales,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(gridExtra,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(data.table, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(tidyverse, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(lubridate, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(gridExtra, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(factoextra, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(tfplot, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(tsfa, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(factoextra, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(cluster, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(IRdisplay, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(foreign,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(extrafont,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(showtext,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
    library(grid,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
})



setwd('C:/Users/samsung/Desktop/대학교/시각화공모전/KT_data_20200703')
getwd()

#행정구역 지도를 가져와 줍니다.
korea_map_shp = rgdal::readOGR("C:/Users/qtuej/Desktop/피샛/공모전/KT_data_20200703/지도/CTPRVN.shp")
korea_map = fortify(korea_map_shp)


korea_map %>% str()

#지역별 확진자 수를 지도에 표시하기 위해 확진자 수 데이터를 가져오고, 알맞게 전처리해 줍니다.

TimeProvince <-fread('C:/Users/qtuej/Desktop/피샛/공모전/KT_data_20200703/COVID_19/TimeProvince.csv',
                     stringsAsFactors=FALSE, 
                     encoding = "UTF-8")

TimeProvince$date <- as.Date(TimeProvince$date)
TimeProvince$date <- as.character(TimeProvince$date,'%m/%d')

TimeProvince$province=TimeProvince$province %>% as.factor()

confirm_added=TimeProvince %>%
  group_by(province)%>% 
  summarize(N=max(confirmed))

confirm_added$province=plyr::revalue(confirm_added$province,c("서울"="0","부산"="1","대구"="2",
                                                              "인천"="3","광주"="4","대전"="5",
                                                              "울산"="6","세종"="7","경기도"="8",
                                                              "강원도"="9","충청북도"="10","충청남도"="11",
                                                              "전라북도"="12","전라남도"="13","경상북도"="14",
                                                              "경상남도"="15","제주도"="16"))


colnames(confirm_added)<-c("id","confirmed")

# 지도 데이터와 확진자 수 데이터를 해당하는 지역에 알맞게 병합하고, 지도에 나타내 줍니다.
korea_map=merge(korea_map,confirm_added, by="id")
mycorona=ggplot() + geom_polygon(data=korea_map, aes(x=long, y=lat, group=group, fill=confirmed))
mycorona+scale_fill_gradient(low = "#F1C5C5", high = "#D92027")+ 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

#전처리

age <- fread('COVID_19/TimeAge.csv',
             stringsAsFactors=FALSE,
             data.table=FALSE,
             encoding = "UTF-8")
age <- age[age$date == '2020-06-30',] 
age <- age[c(3:7),]
#plot_age

c <-ggplot(age, aes(x = "", y = confirmed, fill = age)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y") +
  geom_text(aes(label = paste0(age,"\n",round(confirmed/128,1),"%")), 
           position = position_stack(vjust = 0.5),color = "white") +
  scale_fill_manual(values = c("#CD0000","#F06E6E","#D25A5A","#CD4646","#FF7878","#F4A0A0","#FA8282")) +
  theme_void()   +
  theme(text =element_text(face = "bold"))

#plot_gender


gender <- fread('COVID_19/TimeGender.csv', stringsAsFactors=FALSE,data.table=FALSE, encoding = "UTF-8")

gender <- gender[gender$date == '2020-06-30',]

d <-ggplot(gender, aes(x = "", y = confirmed, fill = sex)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y") +
  geom_text(aes(label = paste0(sex,"\n",round(confirmed/128,1),"%")), 
           position = position_stack(vjust = 0.5),color = "white") +
  scale_fill_manual(values = c("#45556b","#637a9a")) +
  theme_void() +theme(text =element_text(face = "bold"))

grid.arrange(c,d, nrow=2, top = textGrob("나이대 및 성별 누적확진자 비율",gp=gpar(fontsize=20,font=2)))  



##### 전처리

Time <-fread('COVID_19/Time.csv', stringsAsFactors=FALSE,data.table=FALSE, encoding = "UTF-8")
Time$date <- as.Date(Time$date)
str(Time)

c <- c(rep(0,163))
c[1] <- 1
for (i in 2:length(Time$confirmed)){
  if (Time$confirmed[i] - Time$confirmed[i-1] > 0) {
    c[i] <- Time$confirmed[i] - Time$confirmed[i-1]
  }else{
    c[i] <- 0
  }
}

Time <- cbind(Time, c)
#plot 

ggplot(Time, aes(x =date, y = `c`)) + geom_line(color='#c00000', size = 0.9) + coord_fixed(ratio = 0.1) +
 theme(panel.background = element_rect(fill = "#dee3eb" , color = "#6a6a6a"),plot.background = element_rect(fill = "white",
                                size = 0.5, linetype = "solid"),panel.grid.major = element_line(color = "white"),
      plot.title = element_text(face = "bold",hjust = 0.5, size =20),text =element_text(face = "bold", size =15)) +
 ggtitle("COVID-19 실확진자수 추이") +  xlab("date(month)") + ylab("실확진자수(명)")+
 geom_point(mapping =aes(x =Time$date[which.max(Time$`c`)] ,y = 813 ), color="black", size =2.5)+ 
 annotate("text", x=Time$date[which.max(Time$`c`)], y=813,fontface=2,label="\n2020-02-29(813명)\n이유 : 신천지 집단감염",hjust = 1.1, size = 4.6 , color = "#1E3269") +
 geom_point(mapping =aes(x =Time$date[104] ,y = 2 ), color="black", size =2.5) +
 annotate("text", x=Time$date[104], y=2, label="2020-05-02(2명)\n\n\n", size = 5 ,fontface=2, color = "#1E3269") +
 geom_hline(yintercept=78.53, linetype='dashed', color='#FA8282', size=0.7,alpha = 0.7)+
 annotate("text", x=Time$date[5], y=120, label="평균(약 78명)", size = 4 , fontface=2, color = "#D25A5A")

#전처리
#1. fpopl data 를 주별 유동인구 평균 데이터로 전처리
fpopl <-fread('fpopl.csv', stringsAsFactors=FALSE,data.table=FALSE, encoding = "UTF-8")
fpopl %<>% select(base_ymd,adstrd_code,popltn_cascnt)
code <-fread('adstrd_master.csv', stringsAsFactors=FALSE,data.table=FALSE, encoding = "UTF-8")
code %<>% select(adstrd_code,signgu_nm)

doraga <- left_join(fpopl,code,by="adstrd_code")
doraga %<>% select(-adstrd_code)
doraga$base_ymd <- as.character(doraga$base_ymd)
doraga$base_ymd <- as.Date(doraga$base_ymd,"%Y%m%d")
doraga$base_ymd <- week(doraga$base_ymd)


week <- c(1:24)
fpopl_mean <- c(rep(0,24))
df <- data.frame(week,fpopl_mean)


for (i in 1:24){
  a <- doraga %>% filter(base_ymd == i) %>% summarise(n=mean(popltn_cascnt))
  df$fpopl_mean[i] <- a$n
}

#2. 전처리한 유동인구 데이터와 실확진자 수 비교를 위해time 데이터와 병합

Time$date <- week(Time$date)
Time %<>% group_by(date) %>% summarize(c_mean= mean(c)) %>% unique()
colnames(Time) <- c("week","c_mean")

time_fpopl <- plyr::join(Time, df, by = "week")
time_fpopl <- time_fpopl[time_fpopl$week <= 24,] 

#plot 
period <-data.frame(시기 = c("2"),start =c(8), end=c(11))
ggplot() +
  geom_bar(data=time_fpopl, aes(x = week, y = fpopl_mean*2 ,color = "유동인구 수") ,fill = '#1E3269',alpha= 0.3,stat ="identity", position="dodge")+ 
  ylab("명") +geom_smooth(data=time_fpopl, aes(x = week, y = fpopl_mean*2),method = 'loess', formula ='y ~ x',span = 0.3, color = "#ff8888",size = 0.9,se =F, linetype= "dashed")+
  geom_line(data=time_fpopl, aes(x = week, y = c_mean, color = "실확진자 수"),size =0.9)+
  scale_colour_manual("", values=c("실확진자 수" = "#c00000", "유동인구 수" = "#1E3269"))  + coord_fixed(ratio = 0.025) +
  theme(panel.background = element_rect(fill = "#dee3eb" , color = "black"),plot.background = element_rect(fill = "white",
                                size = 0.5, linetype = "solid"),legend.position="bottom", plot.title = element_text(face = "bold",hjust = 0.5, size =20),
        text = element_text(face = "bold",hjust = 0.5, size =15)) +
  ggtitle("주 단위 실확진자와 유동인구 추이 비교")+
  scale_y_continuous(
    
    # Features of the first axis
    name = "주별 평균 실확진자 수",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./2, name="주별 평균 유동인구 수")
  )+
 geom_rect(data=period, aes(NULL,NULL,xmin=start,xmax=end),fill="#adb9ca",
            ymin=0,ymax=573, colour="white", size=0.5, alpha=0.35)

display_png(file="period2.png")
display_png(file="period.png")

########################
### Delivery 전처리 ###
#######################
delivery <- fread('delivery.csv', 
                  header = T, 
                  stringsAsFactors = F, 
                  data.table = F, 
                  encoding = 'UTF-8'
                  )

code = delivery %>% select(ends_with('code')) %>% colnames()
id = c('SERIAL_NUMBER', colnames(select(delivery, ends_with('ID'))))

delivery = delivery %>% select(-c(code, id))
delivery = delivery %>% separate(PROCESS_DT, sep = '-', into = c('year', 'month', 'day'), remove = FALSE)

rm(list = c('code', 'id'))

index <- data.frame('period' = c('기', '승', '전-1', '전-2'),
                    'start' = c('2020-01-01', '2020-02-22', '2020-03-08', '2020-05-06'),
                    'end' = c('2020-02-22', '2020-03-08', '2020-05-06', '2020-06-21'))


p1 <- delivery %>% group_by(PROCESS_DT) %>% 
  summarize(N = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = PROCESS_DT, y = N, group = 1)) + 
  geom_line(linetype = "dashed", color = 'black') + 
  geom_smooth(method = 'lm', se = FALSE, formula = y~x, color = 'firebrick', linetype = 'longdash') +
  geom_point(color = "red", size = 1) +
  geom_rect(data = index,
            aes(NULL, NULL, fill = period, xmin = start, xmax = end), 
            ymin = 0, ymax = 47000, alpha=0.5) +
  scale_fill_manual(values=c("기" = "#d6dce5", "승" = "#adb9ca", '전-1' = '#d6dce5', '전-2' = '#adb9ca')) +
  labs(x = "일자", y = '평균 배달 건수', title = '일별 평균 배달 건수', fill = '코로나 시기') +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = 'bottom', legend.box = 'vertical') + 
  theme(panel.background = element_rect(fill = "white" , color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5, size =20),
        text = element_text(face = "bold",hjust = 0.5,size =15),
        plot.margin = margin(30,40,30,40))


p2 <- delivery %>% filter(DLVR_REQUST_STTUS_VALUE == 1) %>% 
  mutate('SELL_AMOUNT' = GOODS_AMOUNT - DLVR_AMOUNT - CALL_RLAY_FEE_AMOUNT) %>% 
  group_by(PROCESS_DT) %>% 
  summarize(MEAN = mean(SELL_AMOUNT)) %>% 
  ungroup() %>% 
  ggplot(aes(x = PROCESS_DT, y = MEAN, group = 1)) + 
  geom_line(linetype = "dashed", color = 'black') + 
  geom_smooth(method = 'lm', se = FALSE, formula = y~x, color = 'firebrick', linetype = 'longdash') +
  geom_point(color = "red", size = 1) +
  geom_rect(data = index,
            aes(NULL, NULL, fill = period, xmin = start, xmax = end), 
            ymin = 0, ymax = 45224, alpha=0.5) +
  scale_fill_manual(values=c("기" = "#d6dce5", "승" = "#adb9ca", '전-1' = '#d6dce5', '전-2' = '#adb9ca')) +
  labs(x = "일자", y = '평균 배달 매출', title = '일별 평균 배달 매출', fill = '코로나 시기') +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = 'bottom', legend.box = 'vertical') + 
  theme(panel.background = element_rect(fill = "white" , color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5, size =20),
        text = element_text(face = "bold",hjust = 0.5,size =15),
        plot.margin = margin(30,40,30,40))
grid.arrange(p1, p2, ncol = 1)

display_png(file="delivery_increase.JPG")  

delivery$bad = ifelse(delivery$DLVR_STORE_SIDO %in% c('경기도', '경상북도', '대구광역시', '서울'), 1, 0)

delivery %>% 
  group_by(PROCESS_DT, bad) %>% 
  summarize(N = n()) %>% 
  ungroup() %>%
  ggplot(aes(x = PROCESS_DT, y = N)) + 
  geom_line(aes(group = as.factor(bad), color = as.factor(bad)), size = 0.6) +
  geom_smooth(aes(group = as.factor(bad), color = as.factor(bad)), 
              method = 'lm', se = FALSE, formula = y~x, linetype = 'longdash') +
  geom_rect(data = index,
            aes(NULL, NULL, fill = period, xmin = start, xmax = end), 
            ymin = 0, ymax = 30000, alpha=0.5) +
  scale_fill_manual(values=c("기" = "#d6dce5", "승" = "#adb9ca", '전-1' = '#d6dce5', '전-2' = '#adb9ca')) +
  scale_linetype_discrete('코로나 심각 수준(지역)', labels = c('완만', '심각'), aes(Color = as.factor(bad))) +
  scale_color_manual(labels = c('완만', '심각'),values=c( "#1E3269","#c00000")) +
  scale_x_discrete(breaks = NULL) +
  labs(x = '일자', color = '코로나 심각 수준(지역)', y = '배달 건수', fill = '코로나 시기') +
  ggtitle('코로나 심각지역 vs 완만지역 일별 배달건수') +
  theme(legend.position = 'bottom', legend.box = 'vertical') + 
  theme(panel.background = element_rect(fill = "white" , color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5, size =20),
        text = element_text(face = "bold",hjust = 0.5,size =15),
        plot.margin = margin(30,40,30,40))

month_index <- data.frame('period' = c('기', '승', '전-1'),
                          'start' = c('01', '02', '03'),
                          'end' = c('02', '03', '05'))

p1 <- delivery  %>% 
  filter(DLVR_REQUST_STTUS_VALUE == 1) %>% 
  mutate('SELL_AMOUNT' = GOODS_AMOUNT - DLVR_AMOUNT - CALL_RLAY_FEE_AMOUNT) %>% 
  group_by(month, DLVR_STORE_INDUTY_NM) %>% 
  summarise(MEAN = mean(SELL_AMOUNT)) %>% 
  ungroup() %>%
  filter(month != "06") %>% 
  mutate('NEW_cate' = ifelse(DLVR_STORE_INDUTY_NM %in% c('심부름', '도시락') , 1, 0)) %>%  
  ggplot(aes(x = month, y = MEAN)) +
  geom_smooth(aes(group = DLVR_STORE_INDUTY_NM, 
                  color= DLVR_STORE_INDUTY_NM, 
                  linetype = as.factor(NEW_cate), 
                  size = as.factor(NEW_cate)), method = 'lm', se = FALSE, formula = y~x) +
  geom_rect(data = month_index,
            aes(NULL, NULL, fill = period, xmin = start, xmax = end), 
            ymin = 0, ymax = 33000, alpha=0.5) + 
  scale_fill_manual(values=c("기" = "#d6dce5", "승" = "#adb9ca", '전-1' = '#d6dce5')) +
  scale_linetype_manual(values=c("dashed", "solid"),labels = c('그외', '심부름&도시락'))+
  scale_size_manual(values = c(1, 1.1), guide = 'none') +
  guides(color = FALSE) +
  labs(x = '월', y = '평균 배달 매출', linetype = '매출 성장 추세', fill = '코로나 시기', color = '배달품목') +
  ggtitle('배달품목별 월별 매출 변화 추세') +
  theme(panel.background = element_rect(fill = "white" , color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5, size =20),
        text = element_text(face = "bold",hjust = 0.5,size =15),
        plot.margin = margin(30,40,30,40))


p2 <- delivery  %>%
  filter(DLVR_REQUST_STTUS_VALUE == 1) %>% 
  mutate('SELL_AMOUNT' = GOODS_AMOUNT - DLVR_AMOUNT - CALL_RLAY_FEE_AMOUNT) %>% 
  group_by(month, DLVR_STORE_INDUTY_NM) %>% 
  summarise(MEAN = mean(SELL_AMOUNT)) %>% 
  ungroup() %>% 
  filter(month != "06") %>%
  filter(DLVR_STORE_INDUTY_NM %in% c('심부름', '도시락')) %>% 
  ggplot(aes(x = month, y = MEAN)) +
  geom_line(aes(group = DLVR_STORE_INDUTY_NM, color= DLVR_STORE_INDUTY_NM)) +
  geom_point(aes(group = DLVR_STORE_INDUTY_NM, color= DLVR_STORE_INDUTY_NM) ,size = 1) + 
  geom_smooth(aes(group = DLVR_STORE_INDUTY_NM, color= DLVR_STORE_INDUTY_NM), 
              method = 'lm', se = FALSE, formula = y~x, linetype = 'longdash') +
  geom_rect(data = month_index,
            aes(NULL, NULL, fill = period, xmin = start, xmax = end), 
            ymin = 0, ymax = 30000, alpha=0.5) + 
  scale_fill_manual(values=c("기" = "#d6dce5", "승" = "#adb9ca", '전-1' = '#d6dce5')) +
  scale_color_manual(values=c( "#1E3269","#c00000")) +
  labs(x = '월', y = '평균 배달 매출', fill = '코로나 시기', color = '배달품목') +
  ggtitle('배달품목별 월별 매출 변화 추세') +
  theme(panel.background = element_rect(fill = "white" , color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5, size =20),
        text = element_text(face = "bold",hjust = 0.5,size =15),
        plot.margin = margin(30,40,30,40))



grid.arrange(p1, p2, ncol = 1)

delivery = delivery %>% filter(DLVR_REQUST_STTUS_VALUE == 1) %>% 
  mutate('SELL_AMOUNT' = GOODS_AMOUNT - DLVR_AMOUNT - CALL_RLAY_FEE_AMOUNT) %>% 
  mutate('NEW_cate' = ifelse(DLVR_STORE_INDUTY_NM %in% c('심부름', '도시락') , 1, 0))

p1 <- delivery %>% group_by(month, NEW_cate) %>% 
  summarise(MEAN = mean(SELL_AMOUNT)) %>% 
  ungroup() %>% 
  filter(month != "06") %>% 
  ggplot(aes(x = month, y = MEAN)) +
  geom_line(aes(group = as.factor(NEW_cate), color = as.factor(NEW_cate))) +
  geom_point(aes(group = as.factor(NEW_cate), color = as.factor(NEW_cate)), size = 1) +
  geom_smooth(aes(group = as.factor(NEW_cate), color = as.factor(NEW_cate)),
              method = 'lm', se = FALSE, formula = y~x, linetype = 'longdash') +
  geom_rect(data = month_index,
            aes(NULL, NULL, fill = period, xmin = start, xmax = end), 
            ymin = 0, ymax = 33000, alpha=0.5) + 
  scale_fill_manual(values=c("기" = "#d6dce5", "승" = "#adb9ca", '전-1' = '#d6dce5')) +
  scale_color_manual(labels = c('그외', '심부름&도시락'), values=c( "#1E3269","#c00000")) +
  labs(x = '월', color = '배달품목', y = '월별 평균 배달 매출', fill = '코로나 시기') +
  ggtitle('배달품목별 월별 매출 변화 추세') +
  theme(panel.background = element_rect(fill = "white" , color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5, size =20),
        text = element_text(face = "bold",hjust = 0.5,size =15),
        plot.margin = margin(30,40,30,40))


p2 <- delivery %>% group_by(month, NEW_cate) %>% 
  summarise(MEAN = mean(n())) %>%
  ungroup() %>%
  filter(month != "06") %>% 
  ggplot(aes(x = month, y = MEAN)) +
  geom_line(aes(group = as.factor(NEW_cate), color = as.factor(NEW_cate))) +
  geom_point(aes(group = as.factor(NEW_cate), color = as.factor(NEW_cate)), size = 1) +
  geom_smooth(aes(group = as.factor(NEW_cate), color = as.factor(NEW_cate)),
              method = 'lm', se = FALSE, formula = y~x, linetype = 'longdash') +
  geom_rect(data = month_index,
            aes(NULL, NULL, fill = period, xmin = start, xmax = end), 
            ymin = -1000000, ymax = 1200000, alpha=0.5) +
  scale_fill_manual(values=c("기" = "#d6dce5", "승" = "#adb9ca", '전-1' = '#d6dce5')) +
  scale_color_manual(labels = c('그외', '심부름&도시락'), values=c( "#1E3269","#c00000")) +
  labs(x = '월', color = '배달품목', y = '월별 배달 건수', fill = '코로나 시기') +
  ggtitle('배달품목별 월별 건수 변화 추세') +
  theme(panel.background = element_rect(fill = "white" , color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5, size =20),
        text = element_text(face = "bold",hjust = 0.5,size =15),
        plot.margin = margin(30,40,30,40))

grid.arrange(p1, p2, ncol = 1)

delivery %>% group_by(month, NEW_cate) %>% 
  summarise(MEAN = mean(n())) %>%
  ungroup() %>%
  filter(month != "06") %>%
  filter(NEW_cate == 1) %>% 
  ggplot(aes(x = month, y = MEAN)) +
  geom_line(group = 1, color = "#c00000") +
  geom_point(group = 1, color = "#c00000", size = 1) +
  geom_smooth(aes(group = 1), 
              color = "#c00000", method = 'lm', se = FALSE, formula = y ~ x + I(x^2), linetype = 'longdash') +
  geom_rect(data = month_index,
            aes(NULL, NULL, fill = period, xmin = start, xmax = end), 
            ymin = 0, ymax = 15000, alpha=0.5) +
  scale_fill_manual(values=c("기" = "#d6dce5", "승" = "#adb9ca", '전-1' = '#d6dce5')) +
  labs(x = '월', y = '월별 배달 건수', fill = '코로나 시기') +
  ggtitle('도시락&심부름 : 월별 건수 변화 및 추세') +
  theme(legend.position = 'bottom') +
  theme(panel.background = element_rect(fill = "white" , color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5, size =20),
        text = element_text(face = "bold",hjust = 0.5,size =15),
        plot.margin = margin(30,30,30,30))

display_png(file="delivery_2.JPG")  

delivery %>% group_by(month, NEW_cate, bad) %>% 
  summarise(MEAN = mean(SELL_AMOUNT)) %>% 
  ungroup() %>% 
  filter(month != '06') %>% 
  ggplot(aes(y = MEAN, x = as.factor(bad), color = as.factor(NEW_cate))) + 
  stat_boxplot(alpha = 0.4, geom = 'errorbar') +
  stat_summary(aes(fill = as.factor(NEW_cate)), fun = mean, size= 5, geom="point", shape=20,
               position = position_dodge(width = .75)) +
  geom_boxplot(alpha = 0.4) +
  scale_color_manual(labels = c('그외', '심부름,도시락'), values=c("#c00000", "#1E3269")) +
  scale_fill_discrete(guide = 'none') +
  scale_x_discrete(labels = c('완만지역', '심각지역')) +
  labs(x = '코로나 심각 수준', color = '배달품목', y = '배달 매출') +
  ggtitle('코로나 심각에 따른 심부름&도시락 vs 그외품목 매출 비교') +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 15, face = 'bold'),
        text = element_text(face = "bold",hjust = 0.5,size =15),
        plot.margin = margin(45,45,45,45))

#전처리
index <- fread('index.csv', data.table = FALSE, encoding = 'UTF-8')

index %<>% group_by(period,catm,age,gender) %>% mutate(cgi_mean = mean(cgi)) %>% select(period,catm,age,gender,cgi_mean) %>% ungroup() %>% unique()

index <- index[index$period ==  202003 | index$period == 202005,]

a <- c(rep(0,594))

month03 <- filter(index,index$period == 202003)

month05 <- filter(index,index$period == 202005)

change <- plyr::join(month03, month05, by = c("catm","age","gender"))

colnames(change) <- c("period","catm","age","gender","mean1","period2","mean2")
change %<>% mutate(change = (mean2-mean1)/mean1) %>% na.omit() %>% select(catm,age,gender,change,period2)


change%<>% spread(key='catm', value='change') %>% select(-period2)

change

#scaling 하기
change_factor = change %>% select(-age,-gender) %>% na.omit
change_factor = scale(change_factor) %>% na.omit %>% as.tibble()
#scree plot을 통해 factor 수 정하기
par(mar = c(12,5,12,5))
plot(prcomp(change_factor),type="l",sub = 'Scree Plot' , col = "#1E3269")

#시각화

change_fa <- factanal(change_factor,factors=2,rotation = "varimax",scores = "regression")
par(mar = c(6,5,6,5))
plot(change_fa$scores, col = "#1E3269",pch = 20, fg = "#423144",col.axis ='#423144')
text(change_fa$scores[,1], change_fa$scores[,2], labels = change$age, cex = 0.8, pos = 2, col = "#1E3269")
points(change_fa$loadings, pch= 20, col = "#c00000")
text(change_fa$loadings[,1], change_fa$loadings[,2],labels = rownames(change_fa$loadings), cex = 0.5, pos = 4, col = "#c00000")


index <- fread('index.csv', stringsAsFactors=FALSE,data.table=FALSE, encoding = "UTF-8")
index %<>% mutate(year = ifelse(period>202000,2020,2019))
index %<>% group_by(period,catm) %>% mutate(n=mean(cgi)) %>% filter(period < 201906 | period >= 202001 )
index %<>% select(period, catm, n, year) %>% unique()


index %<>% mutate(month = substr(as.character(period),5,6)) 
a <- index %>% filter(catm == "화장품")%>%
  ggplot(aes(x = month, y = n, group = as.character(year) ,color =as.character(year))) +geom_line(size = 0.4, linetype="longdash")+labs(color = "Year\n")+
  ggtitle("화장품 : 2019/2020 cgi 변화율 추이") +  scale_color_manual(values=c("#c00000", "#1E3269"))+ylab("cgi") +geom_point(size=2)+
  theme(legend.position="right", panel.background = element_rect(fill = "white" , color = "black"),plot.background = element_rect(fill = "white",
                                size = 0.5, linetype = "solid"),plot.title = element_text(face = "bold",hjust = 0,size =15))

b <- index %>% filter(catm == "건강관리용품")%>%
  ggplot(aes(x = month, y = n, group = as.character(year) ,color =as.character(year))) +geom_line(size = 0.4, linetype="longdash")+labs(color = "Year\n")+
   ggtitle("건강관리용품 : 2019/2020 cgi 변화율 추이") +geom_point(size=2)+
   scale_color_manual(values=c("#c00000", "#1E3269"))+ylab("cgi") +
  theme(legend.position="right", panel.background = element_rect(fill = "white" , color = "black"),plot.background = element_rect(fill = "white",
                                size = 0.5, linetype = "solid"),plot.title = element_text(face = "bold",hjust = 0,size =15))


c <- index %>% filter(catm == "애완동물용품")%>%
  ggplot(aes(x = month, y = n, group = as.character(year) ,color =as.character(year))) +geom_line(size = 0.4, linetype="longdash")+labs(color = "Year\n")+
  ggtitle("애완동물용품 : 2019/2020 cgi 변화율 추이") + geom_point(size=2)+
  scale_color_manual(values=c("#c00000", "#1E3269"))+ylab("cgi") +
  theme(legend.position="right", panel.background = element_rect(fill = "white" , color = "black"),plot.background = element_rect(fill = "white",
                                size = 0.5, linetype = "solid"),plot.title = element_text(face = "bold",hjust = 0,size =15))

grid.arrange(a,b,c)

cosmetic <- fread('Amore Pacific.csv', data.table = FALSE, encoding = 'UTF-8')
cosmetic <- cosmetic[c(3,26,27,28),c(1,2,3,4,6,7,8,9,10)]

cosmetic

카테고리 <- c("오프라인","오프라인","오프라인","오프라인","오프라인","오프라인","오프라인","오프라인",
      "온라인","온라인","온라인","온라인","온라인","온라인","온라인","온라인",
      "면세점","면세점","면세점","면세점","면세점","면세점","면세점","면세점")
비율<- c(13.2,46.7,40.9,40.1,38.2,29.7,20.4,27.4,
       12.9,13.2,11.5,13.4,15.5,14.2,19.8,30.7,
       40.9,40.1,47.5,46.5,46.2,56.1,59.8,41.8)
분기 <- c("18-1","18-2","18-3","19-1","19-2","19-3","19-4","20-1","18-1","18-2","18-3","19-1","19-2","19-3","19-4","20-1","18-1","18-2","18-3","19-1","19-2","19-3","19-4","20-1")

cosmetic<- data.frame(cbind(카테고리,비율,분기))

a<- ggplot(cosmetic, aes(x=분기, y = 비율, group = 카테고리,color = 카테고리))+
  geom_line(size = 0.5, linetype="longdash") +
  scale_color_manual(values=c('#FFBE0A','#1E3269','#CD1F48'))+ 
  ggtitle("아모레퍼시픽 분기 매출비율 추이") +
  ylab("%") + geom_point(size=2)+ scale_y_discrete(breaks = c(11.5,15.5,30.7,46.2,59.8))+
  theme(legend.position="top", panel.background = element_rect(fill = "white" , color = "black"),plot.background = element_rect(fill = "white",
                                size = 0.5, linetype = "solid"),plot.title = element_text(face = "bold",hjust = 0.5,size =15),
       text = element_text(face = "bold",hjust = 0.5,size =15)) +
  geom_vline(xintercept="19-4",linetype= "longdash", color='grey', size=0.4)
 

b<-  ggplot(cosmetic, aes(x=분기, y = 비율, group = 카테고리,color = 카테고리)) +geom_line(size = 0.5, linetype="longdash") +
  scale_color_manual(values=c('#FFBE0A','#1E3269','#CD1F48'))+
  ylab("%") + geom_point(size=2)+
  coord_fixed(ratio = 0.33) + scale_y_discrete(breaks = c(11.5,15.5,30.7,46.2,59.8))+
  theme(legend.position="", panel.background = element_rect(fill = "white" , color = "black"),plot.background = element_rect(fill = "white",
                                size = 0.5, linetype = "solid"),plot.title = element_text(face = "bold",hjust = 0,size =15),
       text = element_text(face = "bold",hjust = 0.5,size =15)) + facet_wrap(~카테고리, ncol=3)+
  theme(axis.text.x=element_blank())+
  geom_vline(xintercept="19-4",linetype= "longdash", color='grey', size=0.4)
 

grid.arrange(a,b, ncol=1)


card <- fread('card.csv',
              header = T, 
              stringsAsFactors = F,
              data.table = F,
              encoding = 'UTF-8')

## 한글 없애기 ##
data <- card %>% filter(! (selng_cascnt %in% grep('[ㄱ-힣]',unique(card$selng_cascnt), value = T)),
                        ! (salamt %in% grep('[ㄱ-힣]',unique(card$salamt), value = T))) %>% 
  mutate(selng_cascnt = as.numeric(selng_cascnt),
         salamt = as.numeric(salamt)) %>%
  select(- c(adstrd_code, mrhst_induty_cl_code))

rm(list = c('card'))

data$receipt_dttm=data$receipt_dttm %>% as.character() %>% as.Date('%Y%m%d')

## 음수 값 확인 - 양수만 넣기## 
data$selng_cascnt %>% summary()
data$salamt %>% summary()

data = data %>% filter(selng_cascnt > 0, salamt > 0) %>% 
  mutate(receipt_dttm = ymd(receipt_dttm),
         week = week(receipt_dttm))

data %>% glimpse()

#코로나 시기를 새로운 period변수로 나타내 줍니다.
index1 = which(data$receipt_dttm == '2020-02-22') %>% max() #기 
index2 = which(data$receipt_dttm == '2020-03-08') %>% max() #승
index3 = which(data$receipt_dttm == '2020-05-06') %>% max() #전-1
index4 = nrow(data) #전-2

data_period = data 
data_period$period = c(rep(1, index1),
                       rep(2, index2 - index1),
                       rep(3, index3 - index2),
                       rep(4, index4 - index3))

##이상치 및 결측치 처리##

data_period %>% is.na() %>% colSums()

mean_amount=data_period %>%
  group_by(mrhst_induty_cl_nm) %>% 
  summarise(N_amount=mean(selng_cascnt)) %>% 
  arrange(N_amount)

mean_amount %>%
  ggplot(aes(x=1, y=N_amount))+
  geom_violin( color = "#1E3269",size=0.3)+theme_bw() +theme(plot.margin = margin(60,60,60,60)) 

categories_new=mean_amount %>%
  filter(N_amount>=quantile(mean_amount$N_amount)[2]) %>% 
  arrange(desc(N_amount)) %>% select(mrhst_induty_cl_nm)%>% 
  ungroup()

categories_new <- as.data.frame(categories_new)


data_period <- data_period %>% 
  filter(mrhst_induty_cl_nm%in%
           as.matrix(categories_new,nrow = 1))

data_amount_period <- data_period %>% 
  group_by(period, mrhst_induty_cl_nm) %>% 
  summarise(mean_amount = mean(selng_cascnt)) %>% 
  ungroup() %>% 
  spread(period, value = mean_amount)

data_selling_period <- data_period %>% 
  group_by(period, mrhst_induty_cl_nm) %>% 
  summarise(mean_selling = mean(salamt)) %>% 
  ungroup() %>% 
  spread(period, value = mean_selling)

data_price_period <- data_period %>%
  group_by(period,mrhst_induty_cl_nm) %>% 
  summarize(once_price=sum(salamt)/sum(selng_cascnt)) %>% 
  ungroup() %>% 
  spread(period,value = once_price)

colnames(data_amount_period)[-1] = c('amount_1', 'amount_2', 'amount_3', 'amount_4')
colnames(data_selling_period)[-1] = c('selling_1', 'selling_2', 'selling_3', 'selling_4')
colnames(data_price_period)[-1] = c('price_1','price_2','price_3','price_4')

data_clust = data_period %>% group_by(mrhst_induty_cl_nm) %>% 
  summarise(MEAN_SELLING = mean(salamt),
            MEAN_AMOUNT = mean(selng_cascnt),
            once_price = sum(salamt)/sum(selng_cascnt)
            ) %>%
  ungroup %>% 
  left_join(data_amount_period) %>% 
  left_join(data_selling_period) %>% 
  left_join(data_price_period)

#클러스터링하려면 numeric 변수만 필요합니다. 각 카테고리의 이름을 제거해 줍니다.
clust1 = data_clust %>%
  select(-c(mrhst_induty_cl_nm))

clust_scaled = scale(clust1) %>% as_tibble()

set.seed("19990107")
kmeans1 <- kmeans(clust_scaled, nstart = 10, iter.max = 15, centers = 4)
a<-fviz_nbclust(x = clust_scaled, FUNcluster = kmeans, method='wss') + 
  geom_vline(xintercept = 4, linetype = 2)

b<-fviz_nbclust(x = clust_scaled, FUNcluster = kmeans, method = "silhouette") +
  geom_vline(xintercept = 4, linetype = 2)
grid.arrange(a,b)

data_clust$cluster = kmeans1$cluster

fviz_cluster(kmeans1, clust_scaled)+ theme_bw()+theme(
    legend.background = element_rect(color = 'black', 
                                            size = 0.5),plot.margin=margin(50,10,50,10))

final_test_k = data_period %>% 
  left_join(data_clust) %>% 
  group_by(period, cluster) %>% 
  summarise(mean_amount = mean(selng_cascnt),
            mean_selling = mean(salamt),
            once_price = sum(salamt)/sum(selng_cascnt)
            ) %>% ungroup() %>% 
  select(c(period, cluster, mean_amount, mean_selling,once_price))

p1 <- final_test_k %>%  ggplot(aes(x = as.factor(period), 
                                 y = mean_amount,
                                 group = as.factor(cluster), 
                                 color = as.factor(cluster))) + geom_line(size=0.3,linetype="longdash")+geom_point(size=1)+theme_bw()+theme(
    axis.title = element_text(face = "bold",size =11),
    title = element_text(face="bold",size=13),
    legend.position = "none",
    text = element_text(face = "bold")) +
    scale_color_manual(values=c('#CD1F48','#FFBE0A',"#006400",'#1E3269'))+labs(
        color = "clusters",y="평균 판매량",x="",title="시기별 평균 판매량")+ 
    scale_x_discrete(labels = c("기","승","전-1","전-2"))

p1_2 <-  final_test_k %>%  ggplot(aes(x = as.factor(period), 
                                 y = mean_amount,
                                 group = as.factor(cluster), 
                                 color = as.factor(cluster))) + geom_line(size=0.3)+theme_bw()+theme(
    axis.title = element_text(face = "bold",size =11),
    title = element_text(face="bold",size=13,hjust = 0.5),legend.background = element_rect(color = 'black', size = 0.5),
    legend.title = element_text(face="plain",size=10),
    legend.text = element_text(size=8)) +
    scale_color_manual(values=c('#CD1F48','#FFBE0A',"#006400",'#1E3269'))+labs(
        color = "clusters",y="평균 판매량",x="")+ 
    scale_x_discrete(labels = c("기","승","전-1","전-2"))+facet_wrap(~cluster,scales="free")+theme(axis.text.x=element_blank(),
                                                    axis.text.y=element_blank())+labs(y="",x="")

p2 <- final_test_k %>%  ggplot(aes(x = as.factor(period), 
                                 y = mean_selling,
                                 group = as.factor(cluster), 
                                 color = as.factor(cluster))) + geom_line(size=0.3,linetype="longdash")+geom_point(size=1)+theme_bw()+theme(
    axis.title = element_text(face = "bold",size =11),
    title = element_text(face="bold",size=13),legend.position = "none",text = element_text(face = "bold")) +
    scale_color_manual(values=c('#CD1F48','#FFBE0A',"#006400",'#1E3269'))+labs(
        color = "clusters",y="평균 매출",x="",title="시기별 평균 매출")+ 
    scale_x_discrete(labels = c("기","승","전-1","전-2"))

p2_2<-final_test_k %>%  ggplot(aes(x = as.factor(period), 
                                 y = mean_selling,
                                 group = as.factor(cluster), 
                                 color = as.factor(cluster))) + geom_line(size=0.3)+theme_bw()+theme(
    axis.title = element_text(face = "bold",size =11),
    title = element_text(face="bold",size=13,hjust = 0.5),
    legend.background = element_rect(color = 'black', size = 0.5),
    legend.title = element_text(face="plain",size=10),
    legend.text = element_text(size=8)) +
    scale_color_manual(values=c('#CD1F48','#FFBE0A',"#006400",'#1E3269'))+labs(
        color = "clusters",y="평균 매출",x="")+ 
    scale_x_discrete(labels = c("기","승","전-1","전-2"))+facet_wrap(~cluster,scales="free")+theme(axis.text.x=element_blank(),
                                                    axis.text.y=element_blank())+labs(y="",x="")
p3 <- final_test_k %>%  ggplot(aes(x = as.factor(period), 
                                 y = once_price,
                                 group = as.factor(cluster), 
                                 color = as.factor(cluster))) + geom_line(size=0.3,linetype="longdash")+geom_point(size=1)+theme_bw()+theme(
    axis.title = element_text(face = "bold",size =11),
    title = element_text(face="bold",size=13),legend.position = "none",text = element_text(face = "bold")) +
    scale_color_manual(values=c('#CD1F48','#FFBE0A',"#006400",'#1E3269'))+labs(
        color = "clusters",y="평균 가격",x="",title="시기별 평균 가격")+ 
    scale_x_discrete(labels = c("기","승","전-1","전-2"))


p3_2<-final_test_k %>%  ggplot(aes(x = as.factor(period), 
                                 y = mean_selling,
                                 group = as.factor(cluster), 
                                 color = as.factor(cluster))) + geom_line(size=0.3)+theme_bw()+theme(
    axis.title = element_text(face = "bold",size =11),
    title = element_text(face="bold",size=13,hjust = 0.5),
    legend.background = element_rect(color = 'black', size = 0.5),
    legend.title = element_text(face="plain",size=10),
    legend.text = element_text(size=8)
    ) +
    scale_color_manual(values=c('#CD1F48','#FFBE0A',"#006400",'#1E3269'))+labs(
        color = "clusters",y="평균 가격",x="")+ 
    scale_x_discrete(labels = c("기","승","전-1","전-2"))+facet_wrap(~cluster,scales="free")+theme(axis.text.x=element_blank(),
                                                    axis.text.y=element_blank())+labs(y="",x="")
gridExtra::grid.arrange(p1,p1_2,p2,p2_2,p3,p3_2, ncol = 2, nrow = 3)

data_period %>% left_join(data_clust) %>% filter(cluster == 1) %>% select(mrhst_induty_cl_nm) %>% unique() %>% matrix(nrow=1)

data_period %>% left_join(data_clust) %>% filter(cluster == 2) %>% select(mrhst_induty_cl_nm) %>% unique() %>% matrix(nrow=1)

data_period %>% left_join(data_clust) %>% filter(cluster == 3) %>% select(mrhst_induty_cl_nm) %>% unique() %>% matrix(nrow=1)

data_period %>% left_join(data_clust) %>% filter(cluster == 4) %>% select(mrhst_induty_cl_nm) %>% unique() %>% matrix(nrow=1)

##클러스터1##
display_png(file="C:/Users/qtuej/Desktop/피샛/공모전/KT_data_20200703/2차본/클러스터1.png")  

##클러스터2##
display_png(file="C:/Users/qtuej/Desktop/피샛/공모전/KT_data_20200703/2차본/클러스터2.png")

##클러스터3##
display_png(file="C:/Users/qtuej/Desktop/피샛/공모전/KT_data_20200703/2차본/클러스터3.png",width=700,height=350)
display_png(file="C:/Users/qtuej/Desktop/피샛/공모전/KT_data_20200703/2차본/클러스터3_2.png",width=700,height=350) 

##클러스터4##
display_png(file="C:/Users/qtuej/Desktop/피샛/공모전/KT_data_20200703/2차본/클러스터4.png",width=700,height=350)
display_png(file="C:/Users/qtuej/Desktop/피샛/공모전/KT_data_20200703/2차본/클러스터4_1.png",width=700,height=350)
display_png(file="C:/Users/qtuej/Desktop/피샛/공모전/KT_data_20200703/2차본/클러스터4_2.png",width=700,height=350) 

######################
## CARD DATA 전처리 ##
######################

rm(list = ls())
card <- fread('card.csv',
              header = TRUE, 
              stringsAsFactors = FALSE,
              data.table = FALSE,
              encoding = 'UTF-8'
             )

card <- card %>% 
        filter(! (selng_cascnt %in% grep('[ㄱ-힣]',unique(card$selng_cascnt), value = T)),
               ! (salamt %in% grep('[ㄱ-힣]',unique(card$salamt), value = T))) %>% 
        mutate(selng_cascnt = as.numeric(selng_cascnt),
               salamt = as.numeric(salamt),
               receipt_dttm = ymd(receipt_dttm)) %>% 
        select(-c(adstrd_code, mrhst_induty_cl_code))

card$selng_cascnt = ifelse(card$selng_cascnt < 0, 0, card$selng_cascnt)
card$salamt = ifelse(card$salamt < 0, 0, card$salamt)

#########################################
## CARD 품목별 NA(결제 안된 일수) 확인 ##
#########################################

card_amount = card %>%  
  group_by(receipt_dttm, mrhst_induty_cl_nm) %>% 
  summarize(mean_amount = mean(selng_cascnt)) %>% 
  ungroup() %>% 
  spread(key = mrhst_induty_cl_nm, value = mean_amount) 

data_missing = card_amount[,-1] %>% is.na() %>% colSums() %>% as_tibble()
data_missing = cbind(colnames(card_amount)[-1], data_missing)
colnames(data_missing) = c('name', 'num_missing')

data_missing %>% 
  ggplot(aes(x = reorder(name, -num_missing), y = num_missing)) +
  geom_col(aes(fill = num_missing), alpha = 0.5) +
  scale_fill_gradient('결제내역 없는 비중', low = "#1E3269", high = "red") +
      scale_x_discrete(breaks = NULL) +
      labs(x = '카드 결제 품목', y = '결제 없는 일수') +
      ggtitle('카드 결제 품목 별 전체 기간(155일) 중 결제 없는 비율') +
      theme_classic() +
      theme(title = element_text(size = 15, face = 'bold'))

## NA 비중 큰 품목 제거 및 시계열 데이터 화 ##
card_amount = card_amount[colSums(is.na(card_amount)) < 10]
card_amount[is.na(card_amount)] <- 0
card_day <- card_amount[,-1] %>% ts(freq = 365.25)

## Time Series Plot 확인 ##
par(mfrow = c(1,2))
tfplot(card_day[,1:3], graphs.per.page = 3, Title = "Time Series Plot",col = c("#1E3269"))
tfplot(card_day[,4:6], graphs.per.page = 3, Title = "Time Series Plot",col = c("#1E3269"))

## Eigen value를 통한 Factor 개수 결정 ## 
DX <- diff(card_day, lag = 1)
xx <- eigen(cor(diff(card_day, lag = 1)), symmetric = TRUE)[["values"]]
data.frame('eigen_value' = xx, 'index' = 1:153) %>% 
  ggplot(aes(x = index, y = xx)) + 
  geom_line(linetype = 'dashed', color = 'black', size = 0.5) +
  geom_point(col = '#CD1F48', size = 1.2) + 
  labs(x = 'Eigen Value Number', y = 'Eigen Value') +
  ggtitle('Scree Plot by Eigen Value') +
  theme_classic() +
  theme(title = element_text(size = 15, face = 'bold'),plot.margin = margin(50,50,50,50))

card_fa <- estTSF.ML(card_day, 9, rotation = "quartimin", normalize =  TRUE)

data.frame(factors(card_fa), 'time' = 1:155) %>% 
  gather(key = 'Factor', value = 'value', -time) %>% 
  ggplot(aes(x = time, y = value, color = Factor)) + 
  geom_line() +
  labs(x = 'Time', y = 'Value') +
  ggtitle('Factors Time Series plot') +
  theme_classic() +
  theme(title = element_text(size = 15, face = 'bold'),plot.margin =margin(30,30,30,30)) +
  scale_x_discrete(breaks = NULL) +
  scale_y_discrete(breaks = NULL)


tfplot(factors(card_fa),
       Title = "Factors from model", lty = c("solid"), col = c("#1E3269"),
       xlab = c(""), ylab = c("Factor1", "Factor2", "Factor3", "Factor4",
                              "Factor5", "Factor6", "Factor7", "Factor8", "Factor9"),
       par = list(mar = c(2.1, 4.1, 1.1, 0.5)), reset.screen = TRUE)

data.frame(factors(card_fa), 'time' = 1:155) %>% 
  gather(key = 'Factor', value = 'value', -time) %>% 
  filter(Factor %in% c('Factor.6', 'Factor.7')) %>% 
  ggplot(aes(x = time, y = value, color = Factor)) + 
  geom_line() +
  geom_smooth(method = 'loess', formula = y~x, se = FALSE, linetype = 'longdash') +
  labs(x = 'Time', y = 'Value') +
  ggtitle('Factor 6 & 7 Time Series plot') +
  theme_classic() +
  theme(title = element_text(size = 15, face = 'bold'),plot.margin = margin(50,50,50,50)) +
  scale_x_discrete(breaks = NULL) +
  scale_y_discrete(breaks = NULL)+scale_color_manual(values=c( "#1E3269","#c00000"),labels =c('Factor6', 'Factor7'))

factor_6 = card_fa$loadings[,6][which(card_fa$loadings[,6] > 100)] %>% data.frame()
factor_6 = data.frame('name' = row.names(factor_6), 'value' = factor_6[,1])
p1 <- factor_6 %>% 
  ggplot(aes(x = reorder(name, value), y = value)) +
  geom_col(aes(fill = value), alpha = 0.4,) +
  scale_fill_gradient('영향을 끼친 정도', low = "#1E3269", high = "red") +
  labs(x = '카드 결제 품목', y = '영향을 끼친 정도') +
  ggtitle('Factor6에 영향을 끼친 카드 품목별 정도') +
  theme_classic() +
  theme(title = element_text(size = 15, face = 'bold')) +
  coord_flip() 


factor_7 = card_fa$loadings[,7][which(card_fa$loadings[,7] > 100)] %>% data.frame()
factor_7 = data.frame('name' = row.names(factor_7), 'value' = factor_7[,1])
p2 <- factor_7 %>% 
  ggplot(aes(x = reorder(name, value), y = value)) +
  geom_col(aes(fill = value), alpha = 0.4,) +  
  scale_fill_gradient('영향을 끼친 정도', low = "#1E3269", high = "red") +
  labs(x = '카드 결제 품목', y = '영향을 끼친 정도') +
  ggtitle('Factor7에 영향을 끼친 카드 품목별 정도') +
  theme_classic() +
  theme(title = element_text(size = 15, face = 'bold')) +
  coord_flip() 

grid.arrange(p1, p2, ncol = 1)

display_png(file="insight.JPG")

display_png(file="last.JPG")
