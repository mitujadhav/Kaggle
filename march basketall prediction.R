df<-read.csv("D:/Data Analytics/Kaggle/march football prediction/RegularSeasonDetailedResults.csv")
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
    df1<-(df2003[which(df2003$Wteam==d$team[i] | df2003$Lteam==d$team[i]),])
    numerator<-(0.6*dim(df1[which(df1$Wteam==d$team[i] & df1$Wloc=="H"),])[1])+(1.4*dim(df1[which(df1$Wteam==d$team[i] & df1$Wloc=="A"),])[1])
    denominator<-(0.6*(dim(df1[which(df1$Wteam==d$team[i] & df1$Wloc=="H"),])[1]+dim(df1[which(df1$Lteam==d$team[i] & df1$Wloc=="A"),])[1]))+(1.4*(dim(df1[which(df1$Wteam==d$team[i] & df1$Wloc=="A"),])[1]+dim(df1[which(df1$Lteam==d$team[i] & df1$Wloc=="H"),])[1]))
    WP<-numerator/denominator
    print(WP)
  }
}

#############################################
### for OWP (Opponent Winning Percentage) ###

df1<-df[which(df$Season==unique(df$Season)[1]),]
print(paste0("-----",unique(df$Season)[1],"-----"))


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
  
  for(j in seq(1:length(z)))
  {
    dim(df1[which(df1$Wteam==z[j] | df1$Lteam==z[j]),])
    numerator1<-(dim(df1[which(df1$Wteam==z[j] & df1$Lteam!=d$team[i]),])[1])
    opp<-df1[which(df1$Wteam==z[j] | df1$Lteam==z[j]),]
    denominator2<-dim(opp[which(opp$Wteam!=d$team[i] & opp$Lteam!=d$team[i]),])[1]
    print(numerator1/denominator2)
  }
}



