import librosa as li
import numpy as np
import os
import pandas as pd
def pr():
	a="hello"
	return a
def create_train(pathr):
    path=os.path.join(pathr,"dataset")
    path_file=""
    path_folder=""
    whole=[]
    fea=["filename","chroma_stft","rmse","spectral_centroid","spectral_bandwidth","rolloff","zero_crossing_rate","mfcc1","mfcc2","mfcc3","mfcc4","mfcc5","mfcc6","mfcc7","mfcc8","mfcc9","mfcc10","mfcc11","mfcc12","mfcc13","mfcc14","mfcc15","mfcc16","mfcc17","mfcc18","mfcc19","mfcc20","label"]
    #whole.append(fea)
    for label in os.listdir(path):
        path_folder=os.path.join(path,label)
        for file in os.listdir(path_folder):
            features=[]
            path_file=os.path.join(path_folder,file)
            y,sr =li.load(path_file,duration=30)
            chroma_stft = li.feature.chroma_stft(y=y, sr=sr)
            chroma_stft=np.mean(chroma_stft)
            rmse=li.feature.rms(y=y)
            rmse=np.mean(rmse)
            spec_cent = li.feature.spectral_centroid(y=y, sr=sr)
            spec_cent=np.mean(spec_cent)
            spec_bw = li.feature.spectral_bandwidth(y=y, sr=sr)
            spec_bw=np.mean(spec_bw)
            rolloff = li.feature.spectral_rolloff(y=y, sr=sr)
            rolloff=np.mean(rolloff)
            zcr = li.feature.zero_crossing_rate(y)
            zcr=np.mean(zcr)
            mfcc = li.feature.mfcc(y=y, sr=sr)
            features.append(file)
            features.append(chroma_stft)
            features.append(rmse)
            features.append(spec_cent)
            features.append(spec_bw)
            features.append(rolloff)
            features.append(zcr)
            for i in range(20):
                features.append(np.mean(mfcc[i,:]))
            features.append(label)
            whole.append(features)
    df=pd.DataFrame(data=whole,columns=fea)
    path_csv=os.path.join(pathr,"music.csv")
    if (os.path.isfile(path_csv)):
        os.remove(path_csv)
    df.to_csv(path_csv,index=False)

def create_input_feature(path,create_path):
    whole=[]
    fea=["chroma_stft","rmse","spectral_centroid","spectral_bandwidth","rolloff","zero_crossing_rate","mfcc1","mfcc2","mfcc3","mfcc4","mfcc5","mfcc6","mfcc7","mfcc8","mfcc9","mfcc10","mfcc11","mfcc12","mfcc13","mfcc14","mfcc15","mfcc16","mfcc17","mfcc18","mfcc19","mfcc20"]
    features=[]
    y,sr =li.load(path,mono=True)
    chroma_stft = li.feature.chroma_stft(y=y, sr=sr)
    chroma_stft=np.mean(chroma_stft)
    rmse=li.feature.rms(y=y)
    rmse=np.mean(rmse)
    spec_cent = li.feature.spectral_centroid(y=y, sr=sr)
    spec_cent=np.mean(spec_cent)
    spec_bw = li.feature.spectral_bandwidth(y=y, sr=sr)
    spec_bw=np.mean(spec_bw)
    rolloff = li.feature.spectral_rolloff(y=y, sr=sr)
    rolloff=np.mean(rolloff)
    zcr = li.feature.zero_crossing_rate(y)
    zcr=np.mean(zcr)
    mfcc = li.feature.mfcc(y=y, sr=sr)
    features.append(chroma_stft)
    features.append(rmse)
    features.append(spec_cent)
    features.append(spec_bw)
    features.append(rolloff)
    features.append(zcr)
    for i in range(20):
        features.append(np.mean(mfcc[i,:]))
    whole.append(features)
    df=pd.DataFrame(data=whole,columns=fea)
    create_path=os.path.join(create_path,"input.csv")
    if (os.path.isfile(create_path)):
        os.remove(create_path)
    df.to_csv(create_path,index=False)


#create_train(r"C:/Users/AYUSHI/Desktop/R_minor")
#create_input_feature(r"C:/Users/AYUSHI/Desktop/R_minor/1.wav",r"C:/Users/AYUSHI/Desktop/R_minor")