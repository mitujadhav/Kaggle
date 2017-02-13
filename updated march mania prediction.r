
df<-read.csv("D:/My Stuff/Kaggle/march ml mania 2017/RegularSeasonDetailedResults.csv")
table(df$Season)
names(df)
df<-df[,c(1:8)]

a<-as.data.frame(unique(df$Wteam))
b<-as.data.frame(unique(df$Lteam))
colnames(a)[1]<-"team"
colnames(b)[1]<-"team"
c<-rbind(a,b)
d<-unique(c)
d

for(j in seq(length(unique(df$Season))))
{
  df1<-df[which(df$Season==unique(df$Season)[j]),]
  print(paste0("-----",unique(df$Season)[j],"-----"))
  for(i in seq(1:355))
  {
    df2<-(df1[which(df1$Wteam==d$team[i] | df1$Lteam==d$team[i]),])
    numerator<-(0.6*dim(df2[which(df2$Wteam==d$team[i] & df2$Wloc=="H"),])[1])+(1.4*dim(df2[which(df1$Wteam==d$team[i] & df2$Wloc=="A"),])[1])
    denominator<-(0.6*(dim(df2[which(df2$Wteam==d$team[i] & df2$Wloc=="H"),])[1]+dim(df2[which(df2$Lteam==d$team[i] & df2$Wloc=="A"),])[1]))+(1.4*(dim(df2[which(df2$Wteam==d$team[i] & df2$Wloc=="A"),])[1]+dim(df2[which(df2$Lteam==d$team[i] & df2$Wloc=="H"),])[1]))
    WP<-numerator/denominator
    print(WP)
  }
}

df1<-df[which(df$Season==unique(df$Season)[1]),]
df1<-df1[which(df1$Wteam=="1105" | df1$Lteam=="1105"),]
numerator<-(0.6*dim(df1[which(df1$Wteam=="1105" & df1$Wloc=="H"),])[1])+(1.4*dim(df1[which(df1$Wteam=="1105" & df1$Wloc=="A"),])[1])
denominator<-(0.6*(dim(df1[which(df1$Wteam=="1105" & df1$Wloc=="H"),])[1]+dim(df1[which(df1$Lteam=="1105" & df1$Wloc=="A"),])[1]))+(1.4*(dim(df1[which(df1$Wteam=="1105" & df1$Wloc=="A"),])[1]+dim(df1[which(df1$Lteam=="1105" & df1$Wloc=="H"),])[1]))
WP<-numerator/denominator
print(WP)

#############################################
### for OWP (Opponent Winning Percentage) ###
num=NULL
owp <- data.frame(owp = numeric(length(355)))
for(k in seq(1:length(unique(df$Season))))
{
  df1<-df[which(df$Season==unique(df$Season)[k]),]
  print(paste0("*****",unique(df$Season)[k],"*****"))
  for(i in seq(1:355))
  {
    df2<-(df1[which(df1$Wteam==d$team[i] | df1$Lteam==d$team[i]),])
    x<-unique(df2$Wteam)
    y<-unique(df2$Lteam)
    x<-as.data.frame(x)
    y<-as.data.frame(y)
    colnames(x)[1]<-"team"
    colnames(y)[1]<-"team"
    z<-rbind(x,y)
    z<-unique(z)
    dim(z)
    z<-z[which(z$team!=d$team[i]),]
    print(paste0("------",d$team[i],"------"))
    
    numerator<-(0.6*dim(df2[which(df2$Wteam==d$team[i] & df2$Wloc=="H"),])[1])+(1.4*dim(df2[which(df2$Wteam==d$team[i] & df2$Wloc=="A"),])[1])
    denominator<-(0.6*(dim(df2[which(df2$Wteam==d$team[i] & df2$Wloc=="H"),])[1]+dim(df2[which(df2$Lteam==d$team[i] & df2$Wloc=="A"),])[1]))+(1.4*(dim(df2[which(df2$Wteam==d$team[i] & df2$Wloc=="A"),])[1]+dim(df2[which(df2$Lteam==d$team[i] & df2$Wloc=="H"),])[1]))
    WP<-numerator/denominator
    print(WP)
    num<-0
    for(j in seq(1:length(z)))
    {
      dim(df1[which(df1$Wteam==z[j] | df1$Lteam==z[j]),])
      numerator1<-(dim(df1[which(df1$Wteam==z[j] & df1$Lteam!=d$team[i]),])[1])
      opp<-df1[which(df1$Wteam==z[j] | df1$Lteam==z[j]),]
      denominator2<-dim(opp[which(opp$Wteam!=d$team[i] & opp$Lteam!=d$team[i]),])[1]
      print(numerator1/denominator2)
      num <-num + (numerator1/denominator2)
    }
    print(paste0("Addition : ",num,""))
  }
}


##################################
# Demo #


num=NULL

  df1<-df[which(df$Season==unique(df$Season)[1]),]
  print(paste0("*****",unique(df$Season)[1],"*****"))
 
     df2<-(df1[which(df1$Wteam==d$team[1] | df1$Lteam==d$team[1]),])
    x<-unique(df2$Wteam)
    y<-unique(df2$Lteam)
    x<-as.data.frame(x)
    y<-as.data.frame(y)
    colnames(x)[1]<-"team"
    colnames(y)[1]<-"team"
    z<-rbind(x,y)
    z<-unique(z)
    dim(z)
    z<-z[which(z$team!=d$team[1]),]
    print(paste0("------",d$team[1],"------"))
    
    numerator<-(0.6*dim(df2[which(df2$Wteam==d$team[1] & df2$Wloc=="H"),])[1])+(1.4*dim(df2[which(df2$Wteam==d$team[1] & df2$Wloc=="A"),])[1])
    denominator<-(0.6*(dim(df2[which(df2$Wteam==d$team[1] & df2$Wloc=="H"),])[1]+dim(df2[which(df2$Lteam==d$team[1] & df2$Wloc=="A"),])[1]))+(1.4*(dim(df2[which(df2$Wteam==d$team[1] & df2$Wloc=="A"),])[1]+dim(df2[which(df2$Lteam==d$team[1] & df2$Wloc=="H"),])[1]))
    WP<-numerator/denominator
    print(WP)
    num<-0
    for(j in seq(1:length(z)))
    {
      dim(df1[which(df1$Wteam==z[j] | df1$Lteam==z[j]),])
      numerator1<-(dim(df1[which(df1$Wteam==z[j] & df1$Lteam!=d$team[1]),])[1])
      opp<-df1[which(df1$Wteam==z[j] | df1$Lteam==z[j]),]
      denominator2<-dim(opp[which(opp$Wteam!=d$team[1] & opp$Lteam!=d$team[1]),])[1]
      print(numerator1/denominator2)
      num <-num + (numerator1/denominator2)
    }
    print(paste0("Addition : ",num,""))
    num/dim(df2)[1]
    
    
    
#     [1] 0.7586207
#     [1] 0.3333333
#     [1] 0.6428571
#     [1] 0.4814815
#     [1] 0.9032258
#     [1] 0.3076923
#     [1] 0.7666667
#     [1] 0.6923077
#     [1] 0.7142857
#     [1] 0.6896552
#     [1] 0.8275862
#     [1] 0.4814815
#     [1] 0.5185185
#     [1] 0.5666667
#     [1] 0.2222222
#     [1] 0.5
#     [1] 0.4230769
#     [1] 0.5714286
#     [1] 0.68
#     [1] 0.862069
#     [1] 0.6296296
#     [1] 0.4444444
#    
#     > print(paste0("Addition : ",num,""))
#     [1] "Addition : 13.0172496370272"
#     > 0.7586207+0.3333333+0.6428571+0.4814815+0.9032258+0.3076923+0.7666667+0.6923077+0.7142857+0.6896552+ 0.8275862+0.4814815+0.5185185+0.5666667+0.2222222+0.5+0.4230769+0.5714286+0.68+0.862069+0.6296296+0.4444444
#     [1] 13.01725

#########################################


df1[which(df1$Wteam=="1104" & df1$Lteam=="1104"),]

dim(df2)


