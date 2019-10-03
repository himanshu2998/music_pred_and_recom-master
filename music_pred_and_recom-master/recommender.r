# install.packages("recommenderlab")
# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("reshape2")
recom=function()
{
library(ggplot2)
library(data.table)
library(reshape2)

# making a ratings csv file

# library(dplyr) #required for select
# data=read.csv("D:\\r programming\\programs\\music.csv")
# head(data)
# temp=select(data,filename,label)
# head(temp)
# rating=sample(1:5,1000,replace=TRUE)
# userId=sample(1:100,1000,replace = TRUE)
# rating
# temp=cbind(temp,rating)
# head(temp)
# temp=cbind(temp,userId)
# temp
# setwd("D:\\r programming\\programs")
# write.csv(temp,file="ratings.csv")

#making a user+genre file

# data=read.csv("D:/r programming/programs/ratings.csv")
# df=data.frame()
# `%not_in%` = purrr::negate(`%in%`)
# for(i in 1:max(data$userId))
# {
#   user=c()
#   genre=c()
#   user=c(i)
#   for(j in 1:nrow(data))
#   {
#     if(i==data$userId[j])
#     {
#       if(data$label[j] %not_in% genre)
#       {
#         genre=c(genre,as.character(data$label[j]))
#       }
#     }
#   }
#   genre=paste(genre,collapse = "|")
#   df=rbind(df,data.frame(user,genre))
# }
# df
# write.csv(df,"D:/r programming/programs/user.csv")

users=dbGetQuery(con,"select * from users")
ratings=dbGetQuery(con,"select * from ratings")

## Data pre-processing
genres=as.data.frame(users$genre, stringsAsFactors=FALSE)
genres2=as.data.frame(tstrsplit(genres[,1], '[|]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE)
colnames(genres2)=c(1:9)
genres2
genre_list=c("jazz", "classical", "pop", "rock", 
                "reggae", "metal","blues", "disco", "country",
                "hiphop")# we have 10 genres in total

genre_matrix=matrix(0,nrow(users)+1,10) #empty matrix, no of users+1, 10=no of genres
genre_matrix[1,]= genre_list #set first row to genre list
colnames(genre_matrix)=genre_list #set column names to genre list


#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col]=1
  }
}
genre_matrix

#convert into dataframe and remove first row which was genre list
genre_matrix2=as.data.frame(genre_matrix[-1,]) 
genre_matrix2
  return (genre_matrix2)
}


# digit=0:9
# #genrating names
# myfun1=function(n=100)
# {
#   a=do.call(paste0,replicate(5,sample(LETTERS,n,TRUE),FALSE))
# }
# name=myfun1()
# 
# #genrating passwords
# myfun2=function(n=100)
# {
#   a=do.call(paste0,replicate(5,sample(digit,n,TRUE),FALSE))
# }
# pwd=myfun2()
# pwd
# 
# #generating users dataframe 
# df=data.frame(uid=users$X,name=name,pwd=pwd,genre=users$genre)
# df
# 
# #appending to users table
# con=dbConnect(RMySQL::MySQL(), host = "localhost",dbname="pathak",user = "pathak", password = "arpitdadlove")
# dbWriteTable(con,"users",df,append=TRUE,row.names=FALSE)

#jaccard similarity function
Jaccard = function (x, y) {
  M.11 = sum(x == 1 & y == 1)
  M.10 = sum(x == 1 & y == 0)
  M.01 = sum(x == 0 & y == 1)
  return (M.11 / (M.11 + M.10 + M.01))
}

# x=Jaccard(genre_matrix2[102,],genre_matrix2[65,])
# x

maxfunc=function(id,genre_matrix2)
  {
  df=dbGetQuery(con,"Select * from users")
  max=0
  for(i in id:id)
{
  for(j in 1:nrow(df))
  {
    x1=genre_matrix2[i,]
    x2=genre_matrix2[j,]
    r=Jaccard(x1,x2)
    if(r>max&&r!=1)
      {
        max=r
        id=j
      }
  }
  }
  return=c(max,id)
}
# r=maxfunc(102)
# r
#generating ratings table
# df_rating=data.frame(uid=ratings$userId,song=ratings$filename,rate=ratings$rating)
# df_rating
# dbWriteTable(con,"ratings",df_rating,append=TRUE,row.names=FALSE)
# 
# #generating music table
# music=read.csv("D:\\r programming\\programs\\music.csv")
# dbWriteTable(con,"music",music,append=TRUE,row.names=FALSE)
# 
# #generting friends table
# df=data.frame(user=name,friend="")
# dbWriteTable(con,"friends",df,append=TRUE,row.names=FALSE)
# 
# #fetching top rated songs of similar users
# x=subset(df_rating,uid==id)
# x=subset(x,rate==max(rate))
# print(x$song)

