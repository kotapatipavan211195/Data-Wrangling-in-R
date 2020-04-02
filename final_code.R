####Installing Packages
install.packages("readxl")
install.packages("dplyr")
install.packages("reshape2")
install.packages("utils")

####Loading Packages
library(readxl)
library(dplyr)
library(reshape2)
library(utils)

#Loading Raw Data from excel file
path_in = "./Technical Test - Data Wrangling.xlsx"
df_patient_clinicaldata<-read_xlsx(file.path(path_in), sheet = 3)
df_tissue_sample<-read_xlsx(file.path(path_in), sheet = 4)
df_serum_protein<-read_xlsx(file.path(path_in), sheet = 5)
df_rna_seq<-read_xlsx(file.path(path_in), sheet = 6)

#Renaming columns
names(df_serum_protein)[1]<- "Patient  Number"

#Joining patient_clinical data & tissue_sample data
data1<-left_join(df_patient_clinicaldata, df_tissue_sample, by="Patient  Number")
#Joining patient_clinical data & serum_protein data
data2<-left_join(df_patient_clinicaldata, df_serum_protein, by= "Patient  Number")

#serum dataset
#Creating Data Frame for Serum IL6 Values
data_serum_IL6<-data2[,c(1:6)]
data_serum_IL6$Result_Units<-"g/L"
data_serum_IL6$Gene_Symbol<-"IL6"
data_serum_IL6$Status<-NA
names(data_serum_IL6)[6]<-"Result"

#Creating Data Frame for Serum IL6R Values
data_serum_IL6R<-data2[,c(1:5,7)]
data_serum_IL6R$Result<-as.numeric(as.character(data_serum_IL6R$`Serum IL-6 Receptor (mg/L)`))/1000
data_serum_IL6R$Result_Units<-"g/L"
data_serum_IL6R$Gene_Symbol<-"IL6R"
data_serum_IL6R$Status<-"NA"
data_serum_IL6R<-data_serum_IL6R[,c(1:5,7,8,9,10)]

#Merged Data Frame with Serum IL6 & IL6R Values
serum_data<-rbind(data_serum_IL6, data_serum_IL6R)
serum_data$Unique_Patient_ID<-paste(serum_data$Study_ID, serum_data$`Patient  Number`, sep = "_")
serum_data$Material_type<-"SERUM"
serum_data$Sample_General_Pathology<-"NA"
serum_data<-serum_data[,c(1,2,10,3,4,5,12,11,8,6,7,9)]
names(serum_data)[6]<-"Sample_ID"

#Flagging values to Status Column (Serum)
serum_data$Status<-NA
i<-0
for (res in serum_data$Result) {
  i<-i+1
  if(is.na(res) | is.na(suppressWarnings(as.numeric(res)))){
    print(paste(i, 'is ', res))
    serum_data$Status[i]<-"Not Done"
  }
}

#Extracting and creating required fields for RNA dataset
data1$Unique_Patient_ID<-paste(data1$Study_ID, data1$`Patient  Number`, sep = "_")
data1$Gene_Symbol<-""
data1$Result_Units<-"RPKM"
data1$Status<-NA
data1$Result<-""
data1$Gene_Symbol<-""
data1<-data1[,-c(8,9)]
data1<-data1[,c(1,2,8,3:7,12,11,9,10)]

#Extracting Values from  RNA-seq data for respective Sample Id and Gene Id
df_rna_seq<-setNames(melt(df_rna_seq), c('rows', 'vars', 'values'))
names(df_rna_seq)[1]<-"Gene_Symbol"
names(df_rna_seq)[2]<-"Sample"
df_rna_seq$Sample<-as.character(df_rna_seq$Sample)
names(df_rna_seq)[3]<-"Result"
data3<- left_join(data1, df_rna_seq, by="Sample")
data3<-data3[,c(1:8,13,14,12,10)]

#Renaming Columns
names(data3)[6]<-"Sample_ID"
names(data3)[7]<-"Sample_General_Pathology"
names(data3)[8]<-"Material_type"
names(data3)[9]<-"Gene_Symbol"
names(data3)[10]<-"Result"

#Flagging values to Status Column (RNA)
i<-0
for (res in data3$Result) {
  i<-i+1
  if(is.na(res) | is.na(suppressWarnings(as.numeric(res)))){
    print(paste(i, 'is ', res))
    data3$Status[i]<-"Not Done"
  }
}

#Final Dataset Creation
#Concatenating RNA & SERUM data frames
final_data<-rbind(data3, serum_data)
final_data<-data.frame(final_data)

#Transforming values for Sample_General_Pathology
final_data$Sample_General_Pathology2<-"NA"
final_data$Sample_General_Pathology2[final_data$Sample_General_Pathology=="Normal"]<-"NORMAL"
final_data$Sample_General_Pathology2[final_data$Sample_General_Pathology=="Liver Tumor"]<-"PRIMARY"
final_data$Sample_General_Pathology2[final_data$Sample_General_Pathology=="Metastic Lung"]<-"METASTATIC"

#Transforming values for Sex
final_data$Sex2<-"NA"
final_data$Sex2[final_data$Sex=="M"]<-"MALE"
final_data$Sex2[final_data$Sex=="F"]<-"FEMALE"

#Rounding off Age
final_data$Age2<-"NA"
final_data$Age2<-round(final_data$Age)

#Rearranging, selecting and renaming required columns
final_data<-final_data[,c(1:3,14,15,6,13,8:12)]
names(final_data)[4]<-"Sex"
names(final_data)[5]<-"Age"
names(final_data)[7]<-"Sample_General_Pathology"

#Writing final Data set to CSV format
write.csv(final_data, "Final_Data.csv", row.names = FALSE)
