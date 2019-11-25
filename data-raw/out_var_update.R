
# This simple script update the output data available in STICS from the "output.csv" file.
# Simply locate the "output.csv", read it, and pass the object to `usethis::use_data()`.
out_data= read.csv2("D:/OneDrive - cirad.fr/Travail_Postdoc/STICS/JavaSTICS-v85/config/outputs.csv",header = FALSE)
colnames(out_data)= c('variable','details','unit','file',"type","V1","V2")
usethis::use_data(out_data, overwrite = TRUE)
