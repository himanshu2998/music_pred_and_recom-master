#install.packages("tcltk")
#install.packages("RMySQL")
# Import the tcltk package
setwd("D:/r programming/programs")

run=parse("test_ui.R")
library(tcltk)
library(RMySQL)

#script="D:/r programming/programs"
#sql connection
con=dbConnect(RMySQL::MySQL(), host = "localhost",dbname="dbname",user = "user", password = "pwd")


#exception handling
Try=function(expr) {
  res=try(expr, silent = TRUE)
  if (inherits(res, "try-error")) {
    tkmessageBox(title = "An error has occured!",
                 message = as.character(res), icon = "error", type = "ok")
  }
  res
}

fontHeading=tkfont.create(family = "Arial", size = 20,weight = "bold")
fontTextLabel=tkfont.create(family = "Times New Roman", size = 12)

funcBtnLogin=function()
{
        login=tktoplevel(bg="lavender")
        tktitle(login)="login"
        
        tkgrid(ttklabel(login,text="Enter name:",font=fontTextLabel),row=0,column=0,columnspan=2,padx=10,pady=10,sticky="W")
        varName=tclVar("enter name")
        tkgrid(ttkentry(login,textvariable=varName),row=0,column=2,columnspan=2,padx=10,pady=10)
        
        tkgrid(ttklabel(login,text="Enter password:",font=fontTextLabel),row=1,column=0,columnspan=2,padx=10,pady=10)
        varPwd=tclVar("enter password")
        tkgrid(ttkentry(login,textvariable=varPwd,show="*"),row=1,column=2,columnspan=2,padx=10,pady=10)
        
        tkgrid(ttklabel(login,text="or",font=fontHeading),row=3,column=1,columnspan=2,padx=10,pady=10)
        
funcBtnAdmin=function()
        {
          #source("test_ui.R")
          if(tclvalue(varName)=="admin")
          {
            if(tclvalue(varPwd)=="admin123@")
            {
              
              eval(run)
              #calling test_ui.R file
            }
            else
            {
              tkmessageBox(message="Sorry!! Wrong Password:(")
            }
          }
          else
          {
            tkmessageBox(message="Sorry!! Not an admin:(")
          }
        }
        
        tkgrid(ttkbutton(login,text="Login as Admin",command=funcBtnAdmin),row=4,column=1,columnspan=2,padx=10,pady=10)
        
  funcBtnLP=function()
  {
     res=dbGetQuery(con, "SELECT * FROM users ")
    if(tclvalue(varName) %in% res$name)
    {
                x=subset(res,name==tclvalue(varName))
                if(x$pwd==tclvalue(varPwd))
        {
                 tkmessageBox(message="Logged in successfully!!",icon="info")
                 tkdestroy(login)
                 
                 #user window
                 userWin=tktoplevel(bg="lavender")
                 tktitle(userWin)="user"
                 
                 userName=paste("Welcome ",tclvalue(varName),"!!")
                 tkgrid(ttklabel(userWin,text=userName,font=fontHeading),row=0,column=1,columnspan=2,padx=20,pady=20)
                 
                 tkgrid(ttklabel(userWin,text="What you feel today ?",font=fontTextLabel),row=1,column=1,columnspan=2,padx=20,pady=20)
                 
         funcLoginExplore=function()
         {
           source("recommender.r")
           x=recom()
           x
           allUsers=dbGetQuery(con,"select * from users")
           sub=subset(allUsers,name==tclvalue(varName))
           df_rating=dbGetQuery(con,"select * from ratings")
           # source("D:\\r programming\\programs\\recommender.r")
           ret=maxfunc(sub$uid,x)
           xE=subset(df_rating,uid==ret[2])
           xE=subset(xE,rate==5|rate==4|rate==3)
           #
           
           explore=tktoplevel(bg="lavender")
           tktitle(explore)="Explore"
           tkgrid(ttklabel(explore,text="Recommended songs are:-",font=fontHeading),row=0,column=0,columnspan=3)
           
           scrU=ttkscrollbar(explore,orient="vertical",command=function(...) tkyview(lU,...))
           lU=tklistbox(explore,height=3,selectmode="extended",yscrollcommand=function(...) tkset(scrU,...))
           for(i in xE$song)
           {
             tkinsert(lU,"end",i)
           }
           tkgrid(lU,row=1,column=0,columnspan=3,sticky="nwes")
           tkgrid(scrU,row=1,column=3,columnspan=1,sticky="ns")
           
           
           # tkmessageBox(message=paste("",xE$song,"\n"),type="ok",title="Recommended songs are:-")

         }
         tkgrid(ttkbutton(userWin,text="Explore",command=funcLoginExplore),row=2,column=0,columnspan=2,padx=20,pady=20)
         
         funcLoginConnect=function()
         {
           connect=tktoplevel(bg="lavender")
           tktitle(connect)="connect"
           funcFriendPlaylist=function()
           {
             res=dbGetQuery(con,"select * from friends")
             res
             playlist=subset(res,user==tclvalue(varName))
             # playlist=subset(res,user=="ABCDE")
             if(any(playlist$friend!=""))
             {
               all=dbGetQuery(con,"select * from users")
               fin=""
               sub2=NULL
               for(i in playlist$friend)
               {
                 i=tstrsplit(i,'[|]',type.convert = TRUE)
                 for(j in i)
                 {
                   sub1=subset(all,name==j)
                   sub1
                   #fetching friends' songs
                   res=dbGetQuery(con,"select * from ratings")
                   sub2=subset(res,uid==sub1$uid)
                   
                   for(k in sub2$song)
                     fin=paste(fin,k,sep = ",")
                 }
                 
               }
               fin=strsplit(fin,',') #splitting fin with ','
               d=data.frame(x=c(fin)) #converting to dataframe
               d=d[-1,] #dropping first row
               
               listen=tktoplevel(bg="lavender")
               tktitle(listen)="Explore"
               tkgrid(ttklabel(listen,text="Your friends are listening to:-",font=fontHeading),row=0,column=0,columnspan=3)
               
               scrL=ttkscrollbar(listen,orient="vertical",command=function(...) tkyview(lL,...))
               lL=tklistbox(listen,height=6,selectmode="extended",yscrollcommand=function(...) tkset(scrL,...))
               for(i in d)
               {
                    tkinsert(lL,"end",i)
               }
               tkgrid(lL,row=1,column=0,columnspan=3,sticky="nwes")
               tkgrid(scrL,row=1,column=3,columnspan=1,sticky="ns")
               
               
               # tkmessageBox(message=paste("Your friends are listening to:-",fin,"\n"))
             }
             else
             {
               tkmessageBox(message="Sorry! You have no friends!!")
             }
          } 
           tkgrid(ttkbutton(connect,text="Friends' Playlist",command=funcFriendPlaylist,width=20),row=0,column=0,columnspan=2,padx=20,pady=20)
           funcNewFriends=function()
           {
             resNew=dbGetQuery(con,"select * from friends")
             sub1=subset(resNew,user==tclvalue(varName))
             
             resNew=dbGetQuery(con,"select * from users")
             sub2=subset(resNew,name!=sub1$user | name!=sub1$friend)
             sub2
             s=sample(sub2$name,6)
            
             friends=tktoplevel(bg="lavender")
             tktitle(friends)="friends"
             tkgrid(ttklabel(friends,text="Make some new friends:-",font=fontTextLabel),row=0,column=0,columnspan=3,padx=20,pady=20)
             scr=ttkscrollbar(friends,orient="vertical",command=function(...) tkyview(l,...))
             l=tklistbox(friends,height=3,selectmode="extended",yscrollcommand=function(...) tkset(scr,...))
             for(i in s)
                {
                  tkinsert(l,"end",i)
                }
             tkgrid(l,row=1,column=0,columnspan=3,sticky="nwes")
             tkgrid(scr,row=1,column=3,columnspan=1,sticky="ns")
             funcBtnSelect=function()
             {
               x=(tkcurselection(l))
               # print(x)
               x=as.character(x)
               c=NULL
               for(j in x)
               {
                 j=as.integer(j)
                 c=c(c,s[j+1])
               }
               c=paste(as.character(c),collapse = "|")
               c
               df=data.frame(user=tclvalue(varName),friend=c)
               dbWriteTable(con,"friends", df, append=TRUE, row.names=FALSE)
               tkmessageBox(message="Friends added!!")
             }
             tkgrid(ttkbutton(friends,text="Select",command=funcBtnSelect),column=0,columnspan=2,padx=20,pady=20)
             
           }
           tkgrid(ttkbutton(connect,text="New Friends",command=funcNewFriends,width=20),row=1,column=0,columnspan=2,padx=20,pady=20)
       } 
         tkgrid(ttkbutton(userWin,text="Connect",command=funcLoginConnect),row=2,column=2,columnspan=2,padx=20,pady=20)
      }
      else
        tkmessageBox(message="Wrong password!!",icon="error")
    }
    else
    {
      tkmessageBox(message="User name doesn't exist!!",icon="error")
    }
  }
  tkgrid(ttkbutton(login,text="Login",command=funcBtnLP),row=2,column=1,columnspan=2,padx=20,pady=20)
  
  tkconfigure(login, cursor = "spider")
}


funcBtnSignup=function()
{
  signup=tktoplevel(bg="lavender")
  tktitle(signup)="signup"
  
  tkgrid(ttklabel(signup,text="Enter name:",font=fontTextLabel),row=0,column=0,padx=10,pady=10,sticky="W")
  varNameS=tclVar("enter name")
  tkgrid(ttkentry(signup,textvariable=varNameS),row=0,column=1,padx=10,pady=10)
  
  tkgrid(ttklabel(signup,text="Enter password:",font=fontTextLabel),row=1,column=0,padx=10,pady=10)
  varPwdS=tclVar("enter password")
  tkgrid(ttkentry(signup,textvariable=varPwdS,show="*"),row=1,column=1,padx=10,pady=10)
  
  funcBtnSP=function()
  {
    res=dbSendQuery(con, "SELECT * FROM users ")
    res=dbFetch(res)
    if(tclvalue(varNameS) %in% res$name)
    {
      tkmessageBox(message="User name already exists!!",icon="error")
    }
    else
    {
      tkmessageBox(message="Registered successfully!!")
    
    signWin=tktoplevel(bg="lavender")
    tktitle(signWin)="Welcome"
    
    text=paste("Welcome",tclvalue(varNameS),"!!")
    tkgrid(ttklabel(signWin,text=text,font=fontHeading),row=0,column=0,columnspan=2,padx=10,pady=20)
    
    tkgrid(ttklabel(signWin,text="What you feel today?",font=fontTextLabel),row=1,column=0,columnspan=2,padx=10,pady=10)
    
    funcBtnExploreS=function()
    {
      exploreS=tktoplevel(bg="lavender")
      tktitle(exploreS)="Explore"
      
      tkgrid(ttklabel(exploreS,text="Here are some songs:-",font=fontHeading),row=0,column=0,columnspan=3,padx=20,pady=20)
      scrS=ttkscrollbar(exploreS,orient="vertical",command=function(...) tkyview(lS,...))
      lS=tklistbox(exploreS,height=3,selectmode="single",yscrollcommand=function(...) tkset(scrS,...))
      songs=dbGetQuery(con,"Select * from music")
      songs=songs$filename
      songs=sample(songs,10,replace = FALSE)
      for(i in songs)
      {
        tkinsert(lS,"end",i)
      }
      tkgrid(lS,scrS,sticky="E")
      tkgrid.configure(scrS,rowspan=2,sticky="E",pady=20)
      
      funcBtnSelect=function()
      {
        cur=tkcurselection(lS)
        query=dbGetQuery(con,"Select * from music")
        query=subset(query,filename==songs[as.integer(cur)+1])
        query=query$label[1]
        x=dbGetQuery(con,"select * from users")
        x=tail(x,1)
        query
        df=data.frame(uid=x$uid+1,name=tclvalue(varNameS),pwd=tclvalue(varPwdS),genre=query)
        dbWriteTable(con,"users",df,append=TRUE, row.names=FALSE)
        f=""
        df=data.frame(user=tclvalue(varNameS),friend=f)
        dbWriteTable(con,"friends",df,append=TRUE,row.names=FALSE)
        df=data.frame(uid=x$uid+1,song=songs[as.integer(cur)+1],rate=sample(1:5,1))
        dbWriteTable(con,"ratings",df,append=TRUE,row.names=FALSE)
        tkmessageBox(message="Thanks for listening! :)")
      }
      
      tkgrid(ttkbutton(exploreS,text="Select",command=funcBtnSelect),row=3,column=0,columnspan=3,padx=10,pady=10)
      
      
    }
    
    tkgrid(ttkbutton(signWin,text="Explore",command=funcBtnExploreS),row=2,column=0,columnspan=2,padx=10,pady=10)
    
    
    }
    
  }
  
  tkgrid(ttkbutton(signup,text="SignUp",command=funcBtnSP),row=2,column=0,columnspan=2,padx=20,pady=20)
  
  tkconfigure(signup, cursor = "spider")
  
}
win=tktoplevel(bg="lavender")
tktitle(win)="window"
tkgrid(tklabel(win,text="WELCOME TO OUR MUSIC APP!!",font=fontHeading),row=0,columnspan=4,sticky="N",pady=20,padx=20)

#login page
tkgrid(ttkbutton(win, text = "login", command=funcBtnLogin),row=1,column=0,columnspan=2,padx=20,pady=20)

tkgrid(ttkbutton(win, text = "signup", command=funcBtnSignup),row=1,column=2,columnspan=2,padx=20,pady=20)


tkconfigure(win, cursor = "spider") #changing the cursor

#lapply(dbListConnections(MySQL()), dbDisconnect) #terminating all connections of database

#Try(tkmessageBox(message="hello")) #exception handling

#tkdestroy(win) closes window

# res=dbSendQuery(con, "SELECT * FROM users ")
# dbFetch(res)

# Clear the result
#dbClearResult(res)

# Disconnect from the database
#dbDisconnect(con)

