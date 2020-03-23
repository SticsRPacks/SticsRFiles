
# This simple script update the output data available in STICS from the "output.csv" file.
# Simply locate the "output.csv", read it, and write it in the right place ("inst/extdata/csv"):
javaSTICS= "D:/OneDrive - cirad.fr/Travail_Postdoc/STICS/JavaSTICS-v85"
out_data= read.csv2(file.path(javaSTICS,"config/outputs.csv"), header = FALSE)[,c(1:3,5)]
colnames(out_data)= c('variable','details','unit',"type")
# usethis::use_data(out_data, overwrite = TRUE)
write.csv2(out_data, file = "inst/extdata/csv/V8.5/outputs.csv", row.names = FALSE)


javaSTICS= "D:/OneDrive - cirad.fr/Travail_Postdoc/SticsRPacks/SticsRTests/inst/stics/JavaSTICS-1.41-stics-9.0"
out_data= read.csv2(file.path(javaSTICS,"config/outputs.csv"), header = FALSE)[,c(1:3,5)]
colnames(out_data)= c('variable','details','unit',"type")
write.csv2(out_data, file = "inst/extdata/csv/V9.0/outputs.csv", row.names = FALSE)


javaSTICS= "D:/OneDrive - cirad.fr/Travail_Postdoc/STICS/JavaSTICS-1.41-stics-9.1"
out_data= read.csv2(file.path(javaSTICS,"config/outputs.csv"), header = FALSE)[,c(1:3,5)]
colnames(out_data)= c('variable','details','unit',"type")
write.csv2(out_data, file = "inst/extdata/csv/V9.1/outputs.csv", row.names = FALSE)
