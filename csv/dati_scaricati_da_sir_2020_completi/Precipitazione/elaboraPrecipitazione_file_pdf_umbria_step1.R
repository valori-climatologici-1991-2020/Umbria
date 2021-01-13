#13 gennaio 2021:
#Lo script bash che elabora il pdf della precipitazione si limita a trasformare il pdf in testo mantenendo il 
#piu' possibile la struttura tabellare. Lo script bash della temperatura invece trasforma anche il testo mediante espressioni regolari.
#
#Questo primo programma e' un programma interattivo: prende i file temp*.txt li legge, confronta i massimi e i totali mensili calcolati
#con quelli disponibili a fine tabella. Se i confronti falliscono il prigramma si ferma mediante browser().
#
#Generalmente il confronto fallisce facendo i confronti fra i totali calcolati e quelli letti: ATTENZIONE!! iL PROGRAMMA
#LEGGE BENE I DATI!! I CONTROLLI FRA I TOTALI FALLISCONO PERCHE' read_table SBAGLIA A LEGGERE LA RIGA DEI TOTALI E TRONCA I TOTALI CON
#3 CIFRE INTERE! IL CONTROLLO QUINDI CONSISTE NEL VERIFICARE I TOTALI CALCOLATI (CHE RIPORTANO 3 CIFRE) CONFRONTARLI CON QUELLI 
#IN FONDO AL PDF (PROPRIO NEL PDF!!). PER TUTTE LE STAZIONI 2020 QUESTI TOTALI COINCIDONO. I TOTALI A 3 CIFRE LETTI DALLA TABELLA (MEDIANTE R) 
#HANNO SEMPRE UNO O DUE CIFRE!!
#
#sOLAMENTE IN TRE STAZIONI IL PROGRAMMA HA NOTATO CHE SIA I TOTALI CHE I MASSIMI NON COINCIDONO (QUESTO ERRORE E' STATO SEMPRE LIMITATO A UN SOLO MESE PER STAZIONE)
#L'ERRORE SUI MASSIMI E SUI TOTALI INDICA CHE QUALCHE DATO NON E' STATO LETTO BENE DA read_table E VA CORRETTO. Come??? vedi dopo
#
#Sia che il programma legga bene tutti i dati o che generi qualche errore il programma si blocca mediante browser() a cui segue un data_edit
#Quindi: in base al messaggio generato dal programma l'utente si rende conto se i dati vanno sistemati o meno e in quale colonna. Dando "continue"
#viene aperta un'interfaccia shiny per editare i dati.
#
#I dati una volta corretti ( o lasciati così come sono se non hanno bisogno di correzioni) vengono salvati in un file con lo stesso 
#nome del file di input preceduto dal prefisso "fixed_"

#I dati fixed hanno:
#-una colonna dd che corrisponde ai giorni (ma i giorni sono in questa fase sbagliati, in quanto la colonna giorno non viene letta bene da read_table)
#-i dati di precipitazione corretti e separati da ";" (quindi possono essere letti senza ambiguita' da read_delim)
#- un campo staz con il nome della stazione
#
#I file fixed devono essere letti dal programma elaboraPrecipitazione_file_pdf_umbria_step2.R per generare il file Prec definitivo.
rm(list=objects())
library("stringr")
library("readr")
library("dplyr")
library("purrr")
library("tidyr")
library("magrittr")
library("DataEditR")

ANNO<-annoI<-annoF<-2020
parametro<-c("Prec")

purrr::imap(list.files(pattern="^temp.+txt$"),.f=function(nomeFile,.y){

  readLines(nomeFile,n=2)->intestazione
  stringr::str_trim(intestazione[1],side="both")->nomeStazione
  str_remove(nomeStazione,"Pluviometr.+$")->nomeStazione
  print(sprintf("Elaboro stazione %s",nomeStazione))
 

  #if(nomiStazioni[.y]!="Castelluccio di Norcia") return()
  read_table(nomeFile,skip=2,guess_max=2,skip_empty_rows=TRUE,col_types = cols(X1=col_character(),.default = col_double()))->dati_full
  names(dati_full)[1]<-"dd"
  
  if(ncol(dati_full)!=13){ 
    sink("logPrec.txt",append=TRUE)
    cat(sprintf("File %s numero di colonne %s\n", nomeFile,ncol(dati_full)))
    sink()
    dati_full %<>% select(-contains("X"))
  }  
  

  dati_full[33,1]<-"Max"  
  dati_full[34,1]<-"Tot"

  dati_full %>% slice(2:32)->dati
  print(nomeFile)
  

  apply(dati %>% select(-dd),2,FUN=function(x){ 
    
    if(all(is.na(x))) return(NA)
    round(sum(x,na.rm=TRUE),1)
    
  })->totCalcolata
  
  apply(dati %>% select(-dd),2,FUN=function(x){ 
    
    if(all(is.na(x))) return(NA)
    round(max(x,na.rm=TRUE),1)
    
  })->maxCalcolata
  
  
  round(dati_full[33,2:13],1)->maxLetta
  round(dati_full[34,2:13],1)->totLetta  
  
  dati$staz<-nomeStazione

  if(!all(is.na(maxCalcolata))) if(! all(maxCalcolata==maxLetta,na.rm=TRUE)){ 
    
    print("Valori Massimi ")
    print(maxCalcolata)
    print(maxLetta)  
  }
  
  if(!all(is.na(totCalcolata))) if(! all(totCalcolata==totLetta,na.rm=TRUE)){ 
    print("Valori Totali ")
    print(totCalcolata)
    print(totLetta)
  }
  
  browser()
  DataEditR::data_edit(x=dati,viewer = FALSE,save_as =glue::glue("fixed_{nomeFile}"),write_fun =readr::write_delim,write_args = list(delim=";",col_names=TRUE))

})