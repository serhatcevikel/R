# http://stackoverflow.com/questions/41569576/r-creating-new-data-from-column-with-string-variable-name


a<-c("Bill", "Jo", "Sue")
b<-c(3,4,10)
d<-c("Red", "Blue", "Yellow")
df<-data.frame(b,d,row.names=a)
colnames(df)<-c("age","favorite_color")
df
str<-"car"
df[,str]<-character(nrow(df))
df
