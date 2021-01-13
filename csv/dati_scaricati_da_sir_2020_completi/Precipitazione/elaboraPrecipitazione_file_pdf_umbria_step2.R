#13 gennaio 2021:
#Lettura dei file fixed_*.txt per generare il file unico con i dati di precipitazione dell'Umbria relativi a ANNO.

#I dati fixed hanno:
#-una colonna dd che corrisponde ai giorni (ma i giorni sono in questa fase sbagliati, in quanto la colonna giorno non viene letta bene da read_table)
#-i dati di precipitazione corretti e separati da ";" (quindi possono essere letti senza ambiguita' da read_delim)
#- un campo staz con il nome della stazione
#
rm(list=objects())
library("stringr")
library("readr")
library("dplyr")
library("purrr")
library("tidyr")
library("magrittr")
library("guido")

#Questo programma e' pensato per elaborare i pdf dell'Umbria che coprono un solo anno, quindi annoI e annoF devono essere uguali
#Il programma fa anche un controllo per verificare l'anno che compare nel file
ANNO<-annoI<-annoF<-2020


PARAMETRO<-c("Prec")

creaCalendario(annoI = annoI,annoF=annoF)->calendario


try(file.remove("logPrec.txt"))

#leggo anagrafica
read_delim("reg.umbria.info.csv",delim=";",col_names = TRUE)->ana

purrr::imap(list.files(pattern="^fixed_temp.+txt$"),.f=function(nomeFile,.y){

  
  read_delim(nomeFile,delim=";",col_names = TRUE,col_types = cols(dd=col_character(),staz=col_character(),.default = col_double()))->dati

  dati$dd<-1:31
  names(dati)[2:13]<-month.abb 
  
  str_trim(dati$staz[1],side="both")->nomeStazione
  
  dati %>%
    dplyr::select(-staz) %>%
    gather(key ="mm",value="val",-dd) %>%
    mutate(yy=ANNO,mm2=match(mm,month.abb)) %>%
    mutate(mm=mm2) %>%
    dplyr::select(-mm2) %>%
    select(yy,mm,dd,everything())->dati
  
  #qui non posso usare grep: ad esempio, Monteleone di Spoleto troverebbe anche la stazione di Spoleto. Devo avere un0uguaglianza perfetta tra i due nomi
  #o comunque nn affidarmi a un semplice grep
  if(nomeStazione=="Passignano sul Trasimeno") nomeStazione<-"Passignano alta"
  
  
  which(tolower(ana$SiteName_CentroFunzionale) %in% tolower(nomeStazione) )->riga 
  if(length(riga)==0) browser()
  if(length(riga)>1) stop("Impossibile")
    
  names(dati)[4]<-ana$SiteID[riga]
  
  dati[!duplicated(dati[,c("yy","mm","dd")]),] 
  
  
  
})->listaOut

purrr::compact(listaOut)->listaOut 

if(!length(listaOut)) stop(glue::glue("Nessun dato disponibile per {PARAMETRO}"))

reduce(.x = listaOut,left_join,by=c("yy"="yy","mm"="mm","dd"="dd"),.init=calendario)->daScrivere

write_delim(daScrivere,glue::glue("{PARAMETRO}_{annoI}_{annoF}.csv"),delim=",",col_names=TRUE)
