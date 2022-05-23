library(MASS)
library(randomForest)
library(caret)
library(dplyr)
library(stringr)
#zaimportowanie zbioru danych
df <- read.table(file.choose(), header = TRUE)

# rozdzielenie zmiennej geo na dwie oddzielne zmienne

df[c('GEO_X', 'GEO_X2')] <- str_split_fixed(df$GEO, ',', 2)
# usuniecie zmiennej geo
df <- subset (df, select = -GEO)
# zmiana typow zmiennych na factor
df$FLAG<- as.factor(df$FLAG)
df$SECTOR <- as.factor(df$SECTOR)
df$ACC_BEFORE <- as.factor(df$ACC_BEFORE)
# oddzielenie data.frameów na dwa, jeden w ktorym sa firmy updale i drugi, w ktorym sa firmy dzialajace
df_FLAG_1<- data.frame(ID= df$ID.[df$FLAG==1],EMPLOYESS = df$EMPLOYEES[df$FLAG==1],
                       MARKETING_SPENDING = df$MARKETING_SPENDING[df$FLAG==1],
                       INCOME=df$INCOME[df$FLAG==1], ACC_BEFORE = df$ACC_BEFORE[df$FLAG==1],
                       FB_LIKES =df$FB_LIKES[df$FLAG==1],AREA=df$AREA[df$FLAG==1],PWC_EMPLOYESS=df$PWC_EMPLOYEES[df$FLAG==1],
                       CREDIT=df$CREDIT[df$FLAG==1],FLAG = df$FLAG[df$FLAG==1], PWC_PRESS_INDEX = df$PWC_PRESS_INDEX[df$FLAG==1],
                       SECTOR =df$SECTOR[df$FLAG==1], WORKING_TIME =2015-df$YEAR_FOUNDED[df$FLAG==1],
                       GEO_X = df$GEO_X[df$FLAG==1],GEO_X2 = df$GEO_X2[df$FLAG==1])
df_FLAG_0<- data.frame(ID= df$ID.[df$FLAG==0],EMPLOYESS = df$EMPLOYEES[df$FLAG==0],
                       MARKETING_SPENDING = df$MARKETING_SPENDING[df$FLAG==0],
                       INCOME=df$INCOME[df$FLAG==0], ACC_BEFORE = df$ACC_BEFORE[df$FLAG==0],
                       FB_LIKES =df$FB_LIKES[df$FLAG==0],AREA=df$AREA[df$FLAG==0],PWC_EMPLOYESS=df$PWC_EMPLOYEES[df$FLAG==0],
                       CREDIT=df$CREDIT[df$FLAG==0],FLAG = df$FLAG[df$FLAG==0], PWC_PRESS_INDEX = df$PWC_PRESS_INDEX[df$FLAG==0],
                       SECTOR =df$SECTOR[df$FLAG==0], WORKING_TIME =NaN,
                       GEO_X = df$GEO_X[df$FLAG==0],GEO_X2 = df$GEO_X2[df$FLAG==0])
# randomforest dla upadlych firm (zmienna working_time)
ind<-sample(2,nrow(df_FLAG_1), replace=TRUE, prob = c(0.7,0.3))
train3 <- df_FLAG_1[ind==1,]
test3 <- df_FLAG_1[ind==2,]
# budowa lasu losowego dla zbioru testowego i zmiennej working time
rf<-randomForest(WORKING_TIME~ EMPLOYESS+MARKETING_SPENDING+
                 INCOME+ACC_BEFORE+
                 FB_LIKES+AREA+PWC_EMPLOYESS+
                 CREDIT+FLAG+PWC_PRESS_INDEX+
                 SECTOR+
                 GEO_X+GEO_X2, data = train3)
p1<- predict(rf,test3)
predicted_working_time<- data.frame(p1,test3$WORKING_TIME)
roznica <- predicted_working_time$p1-predicted_working_time$test3.WORKING_TIME
predicted_working_time_difference<- cbind(predicted_working_time,roznica)
summary(roznica) # Mediana = 0.34 srednia = 0.01, DOKLADNOSC
sd(roznica) # 2.80
# mean error of prediction is (delta)t = 0.30+/-2.7 
# predykcja czasu dzialania firm, które nie upadły
p2 <- predict(rf,df_FLAG_0)
p2_ind <- data.frame(p2,INDEX=df_FLAG_0$ID)
p2ordered <- data.frame(p2_ind$p2[order(p2_ind$p2)],p2_ind$INDEX[order(p2_ind$p2)])
varImpPlot(rf)
p2_ind$p2 <- as.numeric(p2_ind$p2)
posortowane <- data.frame(p2_ind[order(p2)],p2_ind$V2)
# companies that will bankrupt soon are the first indexes in p2ordered variable
# first three: 	
#2878,2377,3550
plot(rf)
varImpPlot(rf)
importance(rf)

rf_t <- tuneRF(train3[,-15],train3[,15],
            stepFactor = 0.2)

plot(t)
# w kolumnie posortowane są wartosci zmiennej working time od najmniejszej wartosci
# 

#############################################################################################
# DLA LOSOWANEGO WORKING TIME
RANDOM_WORKING_TIME <- function(df_FLAG_1, df, YEAR_BANKRUPTCY, YEAR_FOUNDED, WORKING_TIME, .) {
  ind_flag_1<-sample(2,nrow(df_FLAG_1), replace=TRUE, prob = c(1/3,1/3,1/3))
  
  year_bankruptcy <-2015
  working_time <- year_bankruptcy-df_FLAG_1$YEAR_FOUNDED
  
  df_FLAG_1<- data.frame(ID= df$ID.[df$FLAG==1],EMPLOYESS = df$EMPLOYEES[df$FLAG==1],
                         MARKETING_SPENDING = df$MARKETING_SPENDING[df$FLAG==1],
                         INCOME=df$INCOME[df$FLAG==1], ACC_BEFORE = df$ACC_BEFORE[df$FLAG==1],
                         FB_LIKES =df$FB_LIKES[df$FLAG==1],AREA=df$AREA[df$FLAG==1],PWC_EMPLOYESS=df$PWC_EMPLOYEES[df$FLAG==1],
                         CREDIT=df$CREDIT[df$FLAG==1],FLAG = df$FLAG[df$FLAG==1], PWC_PRESS_INDEX = df$PWC_PRESS_INDEX[df$FLAG==1],
                         SECTOR =df$SECTOR[df$FLAG==1],YEAR_FOUNDED =df$YEAR_FOUNDED[df$FLAG==1], WORKING_TIME =working_time
                         ,GEO_X = df$GEO_X[df$FLAG==1],GEO_X2 = df$GEO_X2[df$FLAG==1])
  df_FLAG_1 <- subset (df_FLAG_1, select = -YEAR_BANKRUPTCY)
  # usuniete year_bankruptycy poniewaz mocno wplywa na zaklamanie wyniku working time
  df_FLAG_1 <- subset (df_FLAG_1, select = -YEAR_FOUNDED)
  
  
  ind3<-sample(2,nrow(df_FLAG_1), replace=TRUE, prob = c(0.7,0.3))
  train3 <- df_FLAG_1[ind3==1,]
  test3 <- df_FLAG_1[ind3==2,]
  
  rf3 <- randomForest(WORKING_TIME~.,data = train3)
  
  rf3
  varImpPlot(rf3)
  p3 <- data.frame(predict(rf3, test3))
  roznica_p3 <- p3-test3$WORKING_TIME
  df_p3_roznica_test <- data.frame(p3,test3$WORKING_TIME, p3-test3$WORKING_TIME)
  
  summary(df_p3_roznica_test$predict.rf3..test3..1)
  sd(df_p3_roznica_test$predict.rf3..test3..1)
}

# srednia roznica w przewidywaniach test 3_ working time to 0.32+/-2.83


##################################################################################################
#
#
#
#
#
#
#
################################################################

#################################################################################################################################################################
# DO ZROBIENIA
EXP_CEO_PREDICTION <- function(df, EXP_CEO, ., rozn, df_ceo_binded) {
  # CEO EXP
  
  
  df$EXP_CEO[is.na(df$EXP_CEO)] <- 0
  
  df_FLAG_ceo0<- data.frame(ID= df$ID.[df$EXP_CEO==0],EMPLOYESS = df$EMPLOYEES[df$EXP_CEO==0],
                         MARKETING_SPENDING = df$MARKETING_SPENDING[df$EXP_CEO==0],
                         INCOME=df$INCOME[df$EXP_CEO==0], ACC_BEFORE = df$ACC_BEFORE[df$EXP_CEO==0],
                         FB_LIKES =df$FB_LIKES[df$EXP_CEO==0],AREA=df$AREA[df$EXP_CEO==0],PWC_EMPLOYESS=df$PWC_EMPLOYEES[df$EXP_CEO==0],
                         CREDIT=df$CREDIT[df$EXP_CEO==0],FLAG = df$FLAG[df$EXP_CEO==0], PWC_PRESS_INDEX = df$PWC_PRESS_INDEX[df$EXP_CEO==0],
                         SECTOR =df$SECTOR[df$EXP_CEO==0],
                         GEO_X = df$GEO_X[df$EXP_CEO==0],GEO_X2 = df$GEO_X2[df$EXP_CEO==0])
  
  df_FLAG_ceo1<- data.frame(ID= df$ID.[df$EXP_CEO!=0],EMPLOYESS = df$EMPLOYEES[df$EXP_CEO!=0],
                            MARKETING_SPENDING = df$MARKETING_SPENDING[df$EXP_CEO!=0],
                            INCOME=df$INCOME[df$EXP_CEO!=0], ACC_BEFORE = df$ACC_BEFORE[df$EXP_CEO!=0],
                            FB_LIKES =df$FB_LIKES[df$EXP_CEO!=0],AREA=df$AREA[df$EXP_CEO!=0],PWC_EMPLOYESS=df$PWC_EMPLOYEES[df$EXP_CEO!=0],
                            CREDIT=df$CREDIT[df$EXP_CEO!=0],FLAG = df$FLAG[df$EXP_CEO!=0], PWC_PRESS_INDEX = df$PWC_PRESS_INDEX[df$EXP_CEO!=0],
                            SECTOR =df$SECTOR[df$EXP_CEO!=0],
                            GEO_X = df$GEO_X[df$EXP_CEO!=0],GEO_X2 = df$GEO_X2[df$EXP_CEO!=0], EXP_CEO= df$EXP_CEO[df$EXP_CEO!=0])
  
  
  ind<- sample(2,nrow(df_FLAG_ceo1),replace = TRUE, c(0.7,0.3))
  
  trainceo <- data.frame(df_FLAG_ceo1[ind==1,])
  testceo <- data.frame(df_FLAG_ceo1[ind==2,])
  
  rfceo <- randomForest(EXP_CEO~., data = trainceo)
  
  pceo <- predict(rfceo,testceo)
  summary(pceo)
  rozn$pceo...testceo.EXP_CEO>
  summary(rozn)
  sd(rozn)
  # przewidzenie EXP +/- 1.71
  roznicaceo <- data.frame(pceo,testceo$EXP_CEO, rozn)
  predicted_ceo <- predict(rfceo,df_FLAG_ceo0)
  df_FLAG_ceo_predicted<- data.frame(ID= df$ID.[df$EXP_CEO==0],EMPLOYESS = df$EMPLOYEES[df$EXP_CEO==0],
                                     MARKETING_SPENDING = df$MARKETING_SPENDING[df$EXP_CEO==0],
                                     INCOME=df$INCOME[df$EXP_CEO==0], ACC_BEFORE = df$ACC_BEFORE[df$EXP_CEO==0],
                                     FB_LIKES =df$FB_LIKES[df$EXP_CEO==0],AREA=df$AREA[df$EXP_CEO==0],PWC_EMPLOYESS=df$PWC_EMPLOYEES[df$EXP_CEO==0],
                                     CREDIT=df$CREDIT[df$EXP_CEO==0],FLAG = df$FLAG[df$EXP_CEO==0], PWC_PRESS_INDEX = df$PWC_PRESS_INDEX[df$EXP_CEO==0],
                                     SECTOR =df$SECTOR[df$EXP_CEO==0],
                                     GEO_X = df$GEO_X[df$EXP_CEO==0],GEO_X2 = df$GEO_X2[df$EXP_CEO==0], EXP_CEO = predicted_ceo)
  
  
   df1<- bind_rows(df_FLAG_ceo_predicted,df_FLAG_ceo1)
   
   
   
   
    # 
    #################################################################################################################################################################
    # CEO EXP
    
    
    df$EXP_CEO[is.na(df$EXP_CEO)] <- 0
    
    df_FLAG_ceo0<- data.frame(ID= df$ID.[df$EXP_CEO==0],EMPLOYESS = df$EMPLOYEES[df$EXP_CEO==0],
                              MARKETING_SPENDING = df$MARKETING_SPENDING[df$EXP_CEO==0],
                              INCOME=df$INCOME[df$EXP_CEO==0], ACC_BEFORE = df$ACC_BEFORE[df$EXP_CEO==0],
                              FB_LIKES =df$FB_LIKES[df$EXP_CEO==0],AREA=df$AREA[df$EXP_CEO==0],PWC_EMPLOYESS=df$PWC_EMPLOYEES[df$EXP_CEO==0],
                              CREDIT=df$CREDIT[df$EXP_CEO==0],FLAG = df$FLAG[df$EXP_CEO==0], PWC_PRESS_INDEX = df$PWC_PRESS_INDEX[df$EXP_CEO==0],
                              SECTOR =df$SECTOR[df$EXP_CEO==0],
                              GEO_X = df$GEO_X[df$EXP_CEO==0],GEO_X2 = df$GEO_X2[df$EXP_CEO==0])
    
    df_FLAG_ceo1<- data.frame(ID= df$ID.[df$EXP_CEO!=0],EMPLOYESS = df$EMPLOYEES[df$EXP_CEO!=0],
                              MARKETING_SPENDING = df$MARKETING_SPENDING[df$EXP_CEO!=0],
                              INCOME=df$INCOME[df$EXP_CEO!=0], ACC_BEFORE = df$ACC_BEFORE[df$EXP_CEO!=0],
                              FB_LIKES =df$FB_LIKES[df$EXP_CEO!=0],AREA=df$AREA[df$EXP_CEO!=0],PWC_EMPLOYESS=df$PWC_EMPLOYEES[df$EXP_CEO!=0],
                              CREDIT=df$CREDIT[df$EXP_CEO!=0],FLAG = df$FLAG[df$EXP_CEO!=0], PWC_PRESS_INDEX = df$PWC_PRESS_INDEX[df$EXP_CEO!=0],
                              SECTOR =df$SECTOR[df$EXP_CEO!=0],
                              GEO_X = df$GEO_X[df$EXP_CEO!=0],GEO_X2 = df$GEO_X2[df$EXP_CEO!=0], EXP_CEO= df$EXP_CEO[df$EXP_CEO!=0])
    
    
    ind<- sample(2,nrow(df_FLAG_ceo1),replace = TRUE, c(0.7,0.3))
    
    trainceo <- data.frame(df_FLAG_ceo1[ind==1,])
    testceo <- data.frame(df_FLAG_ceo1[ind==2,])
    
    rfceo <- randomForest(EXP_CEO~., data = trainceo)
    
    pceo <- predict(rfceo,testceo)
    summary(pceo)
    rozn$pceo...testceo.EXP_CEO>
      summary(rozn)
    sd(rozn)
    # przewidzenie EXP +/- 1.71
    roznicaceo <- data.frame(pceo,testceo$EXP_CEO, rozn)
    predicted_ceo <- predict(rfceo,df_FLAG_ceo0)
    df_FLAG_ceo_predicted<- data.frame(ID= df$ID.[df$EXP_CEO==0],EMPLOYESS = df$EMPLOYEES[df$EXP_CEO==0],
                                       MARKETING_SPENDING = df$MARKETING_SPENDING[df$EXP_CEO==0],
                                       INCOME=df$INCOME[df$EXP_CEO==0], ACC_BEFORE = df$ACC_BEFORE[df$EXP_CEO==0],
                                       FB_LIKES =df$FB_LIKES[df$EXP_CEO==0],AREA=df$AREA[df$EXP_CEO==0],PWC_EMPLOYESS=df$PWC_EMPLOYEES[df$EXP_CEO==0],
                                       CREDIT=df$CREDIT[df$EXP_CEO==0],FLAG = df$FLAG[df$EXP_CEO==0], PWC_PRESS_INDEX = df$PWC_PRESS_INDEX[df$EXP_CEO==0],
                                       SECTOR =df$SECTOR[df$EXP_CEO==0],
                                       GEO_X = df$GEO_X[df$EXP_CEO==0],GEO_X2 = df$GEO_X2[df$EXP_CEO==0], EXP_CEO = predicted_ceo)
    
    
    df1<- bind_rows(df_FLAG_ceo_predicted,df_FLAG_ceo1)
    
    
    
    # Predykcja ze zmienna CEO_EXP
    df_FLAG_1ceo<- data.frame(ID= df1$ID.[df1$FLAG==1],
                           EMPLOYESS = df1$EMPLOYEES[df1$FLAG==1],
                           MARKETING_SPENDING = df1$MARKETING_SPENDING[df1$FLAG==1],
                           INCOME=df1$INCOME[df1$FLAG==1],
                           ACC_BEFORE = df1$ACC_BEFORE[df1$FLAG==1],
                           FB_LIKES =df1$FB_LIKES[df1$FLAG==1],
                           AREA=df1$AREA[df1$FLAG==1],
                           PWC_EMPLOYESS=df1$PWC_EMPLOYEES[df1$FLAG==1],
                           CREDIT=df1$CREDIT[df1$FLAG==1],
                           FLAG = df1$FLAG[df1$FLAG==1],
                           PWC_PRESS_INDEX = df1$PWC_PRESS_INDEX[df1$FLAG==1],
                           SECTOR =df1$SECTOR[df1$FLAG==1],
                           WORKING_TIME =NA,
                           GEO_X = df1$GEO_X[df1$FLAG==1],
                           GEO_X2 = df1$GEO_X2[df1$FLAG==1],
                           EXP_CEO = df1$EXP_CEO[df1$FLAG==1])
    
    df_FLAG_0ceo<- data.frame(ID= df1$ID.[df1$FLAG==0],EMPLOYESS = df1$EMPLOYEES[df1$FLAG==0],
                           MARKETING_SPENDING = df1$MARKETING_SPENDING[df1$FLAG==0],
                           INCOME=df1$INCOME[df1$FLAG==0], ACC_BEFORE = df1$ACC_BEFORE[df1$FLAG==0],
                           FB_LIKES =df1$FB_LIKES[df1$FLAG==0],AREA=df1$AREA[df1$FLAG==0],PWC_EMPLOYESS=df1$PWC_EMPLOYEES[df1$FLAG==0],
                           CREDIT=df1$CREDIT[df1$FLAG==0],FLAG = df1$FLAG[df1$FLAG==0], PWC_PRESS_INDEX = df1$PWC_PRESS_INDEX[df1$FLAG==0],
                           SECTOR =df1$SECTOR[df1$FLAG==0], WORKING_TIME =NaN,
                           GEO_X = df1$GEO_X[df1$FLAG==0],GEO_X2 = df1$GEO_X2[df1$FLAG==0],EXP_CEO = df1$EXP_CEO[df1$FLAG==0])
    
    ind<- sample(2,nrow(df_ceo_binded),replace = TRUE, c(0.7,0.3))
    
    trainceo <- data.frame(df_FLAG_ceo1[ind==1,])
    testceo <- data.frame(df_FLAG_ceo1[ind==2,])
    
    df_koncowe <-cbind(df_ceo_binded,df$EXP_CEO) 
}

############################## K_NEAREST dla najwazniejszych parametow impvarplot

