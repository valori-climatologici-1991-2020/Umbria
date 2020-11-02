#2 novembre 2020
#Questo programma prende la lista dei file csv ricavato dal sito degli annali dell'Umbria e mediante
#wget scarica ifile .csv

#Siccome l'indirizzo di ciascun file contiene caratteri speciali (parentesi) necessario mettere
#l'indirizzo fra parentesi.

#Sia wget che curl hanno un problema di SSL certificate: la soluzione ' utilizzare wget con l'opzione
# --no-check-certificate

rm(list=objects())
library("tidyverse")

read_delim("tabellaDownloads.csv",delim=",",col_names = TRUE)->daScaricare

purrr::walk(daScaricare$nomiFile,.f=function(indirizzo){
  system(glue::glue('wget --no-check-certificate "{indirizzo}"'))
  Sys.sleep(5)
})