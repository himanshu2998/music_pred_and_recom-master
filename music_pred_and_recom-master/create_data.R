create_data<-function(script){
  #script <- dirname(sys.frame(1)$ofile)
  new_script<-file.path(script,"dataset")
  unlink(c(new_script),recursive=TRUE,force=TRUE)
  dir.create(new_script)
  script<-file.path(script,"music_dataset")
  folder<-""
  old_file<-""
  new_folder<-""
  new_file<-""
  comm<-""
  change<-file.path("C:","Program Files (x86)","sox-14-4-2")
  change<-paste("cd",change,sep=" ")
  for(genre in dir(script)){
    folder<-file.path(script,genre)
    new_folder<-file.path(new_script,genre)
    dir.create(new_folder)
    i<-0
    for(exact in dir(folder)){
      old_file<-file.path(folder,exact)
      new_file<-file.path(new_folder,paste(i,".wav",sep=""))
      #print(new_file)
      comm<-paste("sox",old_file,new_file,sep=" ")
      comm<-paste(change,comm,sep=" & ")
      shell(comm)
      i<-i+1
    }
  }
  return(TRUE)
}
#create_data()