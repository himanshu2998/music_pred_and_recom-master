library(warbleR)
convert_input<-function(path){
  split<-strsplit(path,"/|\\\\")
  file_path=split[[1]][1]
  for(val in 2:(length(split[[1]])-1))
    file_path<-file.path(file_path,split[[1]][val])
  mp32wav(path = file_path, to = file_path, dest.path = file_path)
  return(TRUE)
}
#convert_input(file.path("C:","Users","AYUSHI","Desktop","R_minor","1.mp3"))