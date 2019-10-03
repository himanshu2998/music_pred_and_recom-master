script <- dirname(sys.frame(1)$ofile)
setwd(script)

source("input_conversion.R")
source("create_data.R")
library(tcltk)

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

funcBtnCreate=function()
{
  if(create_data(script))
    tkmessageBox(message = paste("Dataset has been converted Successfully!!!"))
  else
    tkmessageBox(message = paste("Some error occured!!!"))
    #create_data=tktoplevel(bg="lavender")
  #tktitle(create_data)="create_data"
  #funcBtnLP=function()
  {
  }
  #tkgrid(ttkbutton(login,text="Create datatset",command=funcBtnLP),row=2,column=0,columnspan=2,padx=20,pady=20)
  
  #tkconfigure(login, cursor = "spider")
}

funcBtnInputConvert=function()
{
  filename = tclvalue(tkgetOpenFile()) # Very simple, isn't it?
  if (!nchar(filename)) {
    tkmessageBox(message = "No file was selected!")
  } else {
    convert_input(filename)
    tkmessageBox(message = paste("The file ", filename," was successfully converted!!!"))
  }
  
}


funcBtnInput=function()
{
  filename = tclvalue(tkgetOpenFile()) # Very simple, isn't it?
  if (!nchar(filename)) {
    tkmessageBox(message = "No file was selected!")
  } else {
    convert_input(filename)
    tkmessageBox(message = paste("The file ", filename," was successfully converted!!!"))
  }
  
}

funcBtnStats=function()
{
  
}

win=tktoplevel(bg="lavender")
tktitle(win)="admin"
#tkgrid(tklabel(win,text="WELCOME TO OUR MUSIC APP!!",font=fontHeading),row=0,columnspan=4,sticky="N",pady=20,padx=20)

#login page
tkgrid(ttkbutton(win, text = "Create dataset", command=funcBtnCreate),row=0,column=0,rowspan=2,padx=20,pady=20)
tkgrid(ttkbutton(win, text = "Input music coversion(mp3 to wav)", command=funcBtnInputConvert),row=2,column=0,rowspan=2,padx=20,pady=20)
tkgrid(ttkbutton(win, text = "Input music", command=funcBtnInput),row=4,column=0,rowspan=2,padx=20,pady=20)

tkgrid(ttkbutton(win, text = "Statistical Analysis", command=funcBtnStats),row=6,column=0,rowspan=2,padx=20,pady=20)


tkconfigure(win, cursor = "spider") #changing the cursor

