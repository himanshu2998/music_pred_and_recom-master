# install.packages("GGally")
#install.packages("rgl")
library(GGally)
library(rgl)
library(neuralnet)
# setwd("D:\\r programming\\programs")

#preprocessing
samplesize = 0.8 * nrow(data)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

data=read.csv("D:\\sm\\3rd Year\\sem2\\minor\\music.csv")
str(data)

data$filename=as.numeric(data$filename)
data$filename
data$label=as.numeric(data$label)
data$label

#checking for nulll values
null=function(x)
{
  ret=is.na(x)
  return(ret)
}

isnull=as.data.frame(lapply(data, null))
options(max.print = 100)
print(TRUE %in% isnull)

#scaling/normalizing data
d=read.csv("input.csv")
normalize=function(x) {
   return ((x - min(x)) / (max(x) - min(x)))
 }

maxmindf=as.data.frame(lapply(data, normalize))

# Create training and test set
datatrain = maxmindf[index,]

datatest = maxmindf[-index,]


#visualizing data 

#various combined plots(scatter,corr,graph) between features
ggpairs(data, columns = 1:5, title = "",  
        axisLabels = "show", columnLabels = colnames(data[, c(1:5)]))

#plotting scatter graph between features and analyzing label values(0-10 scale) among them
ggplot(data, aes(chroma_stft, spectral_centroid)) +
  geom_jitter(aes(color = label), size = 1.5)

#plotting correlation plot between all features
# 1. Compute correlation
cormat=round(cor(data),2)
# 2. Reorder the correlation matrix by Hierarchical clustering
hc=hclust(as.dist(1-cormat)/2)
cormat.ord=cormat[hc$order, hc$order]
# 3. Get the upper triangle
cormat.ord[lower.tri(cormat.ord)]=NA
# 4. Melt the correlation matrix
require(reshape2)
melted_cormat=melt(cormat.ord, na.rm = TRUE)
# Create the heatmap
ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") + # Change gradient color
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


#3-d representation of data(rgl package)
plot=with(datatrain, plot3d(datatrain$chroma_stft, datatrain$spectral_centroid,datatrain$zero_crossing_rate, 
                             type="s",col=as.numeric(data$label)))

data_matrix=model.matrix(~., data=scaled)
data_matrix

datatrain
datatest

#creating column names
col_list=paste(c(colnames(datatrain[,-c(1,28)])),collapse="+") #-c(1,28) to drop filename and label columns
col_list=paste(c("label ~",col_list),collapse="")
f=formula(col_list)


library(neuralnet)
set.seed(7896129)

#our main model
nmodel=neuralnet(f,data=datatrain,hidden=c(11,7),
                    threshold = 0.05,
                    learningrate.limit = NULL,
                    learningrate.factor =
                      list(minus = 0.5, plus = 1.2),
                    rep = 100,
                    stepmax = 1e+06,
                    algorithm = "sag")
 

plot(nmodel,rep = 1)

# save the model to disk
saveRDS(nmodel, "./final_model.rds")



##### checking various models with different parameters
set.seed(7896129)
nn5 <- neuralnet(f,data=data_matrix,hidden=5)
plot(nn5)

#This results in an error!!
nn_backprop <- neuralnet(f, data=data_matrix,
                         algorithm = "backprop")
#Error: 'learningrate' must be a numeric value, if the #backpropagation algorithm is used

#This works (but likely won't converge)!
nn_backprop <- neuralnet(f, data=data_matrix,
                         algorithm = "backprop",
                         learningrate = 0.0001)
plot(nn_backprop)
#RPROP Multiple layers
set.seed(1973549813)

nn_rprop_multi <- neuralnet(f, data=datatrain,
                            algorithm = "rprop+",
                            hidden=c(10,3),
                            threshold=0.1,
                            stepmax = 1e+06)
plot(nn_rprop_rep5)

#Creating a neural network stopped at threshold = 0.5
nn_rprop_rep5 <- neuralnet(f, data=datatrain,
                           algorithm = "rprop+",
                           hidden=c(15),
                           threshold=0.5,
                           rep=1,
                           stepmax = 1e+06)
nn_rprop_rep5
plot(nn_rprop_rep5)

# load the final model
nmodel_load=readRDS("./final_model.rds")

plot(nmodel_load,rep=1)

#computing output
output=compute(nmodel_load, datatest,rep=1) 
output$net.result
#ploting real vs predicted regression line
data$label=as.numeric(data$label)
output = (output$net.result * (max(data$label) - min(data$label))) + min(data$label)
test=data[-index,]
plot(datatest$label, output, col='blue', pch=16, ylab = "p redicted label NN", xlab = "real label")

abline(0,11)

#rmse error

RMSE.NN = (sum((datatest$label - output)^2) / nrow(datatest)) ^ 0.5
RMSE.NN


datatest$label
#summary of output
summary(output)
length(datatest$label)

#creating actual vs predicted matrix
results=data.frame(actual = datatest$label, prediction = output$net.result)
results

#finding accuracy of model
predicted=results$prediction * abs(diff(range(data$label))) + min(data$label)
actual=results$actual * abs(diff(range(data$label))) + min(data$label)
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy

datatest
output$net.result

# # Random sampling
# samplesize = 0.60 * nrow(data)
# samplesize
# set.seed(80)
# index = sample( seq_len ( nrow ( data ) ), size = samplesize )
# 
# # Create training and test set
# datatrain = data[ index, ]
# datatest = data[ -index, ]
# 
# ## Scale data for neural network
# 
# max = apply(data , 2 , max)
# max = as.numeric(max)
# min = apply(data, 2 , min)
# min = as.numeric(min)
# scaled = as.data.frame(lapply(data, as.numeric), center = min, scale = max-min)
# 
# ## Fit neural network 
# 
# # install library
# install.packages("neuralnet")
# 
# # load library
# library(neuralnet)
# 
# # creating training and test set
# trainNN = scaled[index , ]
# testNN = scaled[-index , ]
# 
# # fit neural network
# set.seed(2)
# n=names(trainNN)
# f <- as.formula(paste("label ~", paste(n[!n %in% "label"], collapse = " + ")))
# #For some reason the formula y~. is not accepted in the neuralnet() function.
# #You need to first write the formula and then pass it as an argument in the fitting function.
# NN = neuralnet(f, trainNN, hidden = 6 , linear.output = T )
# 
# # plot neural network
# plot(NN)
# 
# 
# ## Prediction using neural network
# 
# predict_testNN = compute(NN, testNN[,c(1:28)])
# predict_testNN = (predict_testNN$net.result * (max(as.numeric(data$label))) - min(as.numeric(data$label)) + min(as.numeric(data$label)))
# plot(datatest$label, predict_testNN, col='blue', pch=16, ylab = "predicted label NN", xlab = "real label")
# 
# abline(0,1)
# 
# # Calculate Root Mean Square Error (RMSE)
# RMSE.NN = (sum((as.numeric(datatest$label) - predict_testNN)^2) / nrow(datatest)) ^ 0.5
# 
# ## Cross validation of neural network model
# 
# # install relevant libraries
# install.packages("boot")
# install.packages("plyr")
# 
# # Load libraries
# library(boot)
# library(plyr)
# 
# # Initialize variables
# set.seed(50)
# k = 100
# RMSE.NN = NULL
# 
# List = list( )
# 
# # Fit neural network model within nested for loop
# for(j in 10:65){
#   for (i in 1:k) {
#     index = sample(1:nrow(data),j )
#     
#     trainNN = scaled[index,]
#     testNN = scaled[-index,]
#     datatest = data[-index,]
#     
#     
#     n=names(trainNN)
#     f <- as.formula(paste("label ~", paste(n[!n %in% "label"], collapse = " + ")))
#     NN = neuralnet(f, trainNN, hidden = 6, linear.output= T)
#     predict_testNN = compute(NN,testNN[,c(1:28)])
#     predict_testNN = (predict_testNN$net.result * (max(as.numeric(data$label))) - min(as.numeric(data$label)) + min(as.numeric(data$label)))
#     
#     RMSE.NN [i]<- (sum((as.numeric(datatest$lebel) - predict_testNN)^2)/nrow(datatest))^0.5
#   }
#   List[[j]] = RMSE.NN
# }
# 
# Matrix.RMSE = do.call(cbind, List)
# 
# 
# ## Prepare boxplot
# boxplot(Matrix.RMSE[,56], ylab = "RMSE", main = "RMSE BoxPlot (length of traning set = 65)")
# 
# 
# ## Variation of median RMSE 
# install.packages("matrixStats")
# library(matrixStats)
# 
# med = colMedians(Matrix.RMSE)
# 
# X = seq(10,65)
# 
# plot (med~X, type = "l", xlab = "length of training set", ylab = "median RMSE", main = "Variation of RMSE with length of training set")
