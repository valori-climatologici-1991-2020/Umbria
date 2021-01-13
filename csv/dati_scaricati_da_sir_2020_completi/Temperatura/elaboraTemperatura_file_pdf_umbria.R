#Revisione 13 gennaio 2021
#
#Questo programma elabora i pdf disponibili alla pagina: https://www.regione.umbria.it/ambiente/servizio-idrografico
#Il programma sfrutta i valori minimi medi e massimi riportati a fine tabella per controllare la correttezza dell'elaborazione
#
#I dati elaborati sono file .txt ottenuti processando i pdf mediante uno script in bash
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

PARAMETRO<-c("Tmax","Tmin")[2]

ifelse(PARAMETRO=="Tmax","Valori massimi","Valori minimi")->stringaTemp

creaCalendario(annoI = annoI,annoF=annoF)->calendario

try(file.remove("log.txt"))

#leggo anagrafica
read_delim("reg.umbria.info.csv",delim=";",col_names = TRUE)->ana

#mesi
stringr::str_pad(1:12,pad="0",side="left",width=2)->mesi

purrr::imap(list.files(pattern="^clean_temp.+txt$"),.f=function(nomeFile,.y){

  readLines(nomeFile,n=2)->intestazione
  
  stringr::str_trim(intestazione[1],side="both")->nomeStazione
  print(sprintf("Elaboro stazione %s",nomeStazione))

  stringr::str_trim(intestazione[2],side="both")->qualeParametro  
  if(qualeParametro!=stringaTemp) return()
  
  #if(nomiStazioni[.y]!="Castelluccio di Norcia") return()
  read_table(nomeFile,col_names = TRUE,skip=2,na=c("","*"),col_types = cols(X1=col_character(),.default = col_double()))->dati_full
  names(dati_full)[1]<-"dd"
  
  if(ncol(dati_full)!=13){ 
    sink("log.txt",append=TRUE)
    cat(sprintf("File %s numero di colonne %s\n", nomeFile,ncol(dati_full)))
    sink()
    dati_full %<>% select(-contains("X"))
  }  
  

  #stopifnot(ncol(dati_full)==13)  
  stopifnot(grep("MIN",dati_full[[1]])==33)  
  stopifnot(grep("MAX",dati_full[[1]])==34)
  stopifnot(grep("MED",dati_full[[1]])==35)  
  
  dati_full %>% slice(2:32)->dati
  
  print(nomeFile)
  
  #media
  apply(dati %>% select(-dd),2,FUN=function(x){ 
    
    if(all(is.na(x))) return(NA)
    as.integer(floor(mean(x,na.rm=TRUE)))
    
  })->meanCalcolata

  apply(dati %>% select(-dd),2,FUN=function(x){ 
    
    if(all(is.na(x))) return(NA)
    as.integer(floor(min(x,na.rm=TRUE)))
    
  })->minCalcolata
  
  apply(dati %>% select(-dd),2,FUN=function(x){ 
    
    if(all(is.na(x))) return(NA)
    as.integer(floor(max(x,na.rm=TRUE)))
    
  })->maxCalcolata
  
  
  as.integer(floor(dati_full[33,2:13]))->minLetta
  as.integer(floor(dati_full[34,2:13]))->maxLetta
  as.integer(floor(dati_full[35,2:13]))->meanLetta
  
  if(!all(is.na(minCalcolata))) if(! all(minCalcolata==minLetta,na.rm=TRUE)) stop("Errore temperatura minima")
  if(!all(is.na(maxCalcolata))) if(! all(maxCalcolata==maxLetta,na.rm=TRUE)) stop("Errore temperatura massima")
  if(!all(is.na(meanCalcolata))) if(! all((meanCalcolata-meanLetta >= -1) & (meanCalcolata-meanLetta <= 1),na.rm=TRUE)) browser() #stop("Errore temperatura media")  
  
  stopifnot(all(as.integer(dati_full[1,2:13])==ANNO ))
  
  #dati è una tabella in cui la prima colonna sono i giorni, poi segue la colonna di gennaio, poi febbraio etc etc
  names(dati)<-c(as.character(c("dd",mesi)))

  dati %>%
    gather(key ="mm",value="val",-dd) %>%
      mutate(yy=as.integer(ANNO),mm=as.integer(mm),dd=as.integer(dd)) %>%
        select(yy,mm,dd,everything())->dati
  
  
  #qui non posso usare grep: ad esempio, Monteleone di Spoleto troverebbe anche la stazione di Spoleto. Devo avere un0uguaglianza perfetta tra i due nomi
  #o comunque nn affidarmi a un semplice grep
  if(nomeStazione=="Passignano sul Trasimeno") nomeStazione<-"Passignano alta"
  
  which(tolower(ana$SiteName_CentroFunzionale) %in% tolower(nomeStazione) )->riga 
  if(length(riga)==0) browser()

  names(dati)[4]<-ana$SiteID[riga]
  
  dati[!duplicated(dati[,c("yy","mm","dd")]),] 
  
})->listaOut

purrr::compact(listaOut)->listaOut 

if(!length(listaOut)) stop(glue::glue("Nessun dato disponibile per {PARAMETRO}"))

reduce(.x = listaOut,left_join,by=c("yy"="yy","mm"="mm","dd"="dd"),.init=calendario)->daScrivere

write_delim(daScrivere,glue::glue("{PARAMETRO}_{annoI}_{annoF}.csv"),delim=",",col_names=TRUE)
