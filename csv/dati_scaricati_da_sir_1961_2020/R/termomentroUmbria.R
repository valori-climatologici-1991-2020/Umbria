rm(list=objects())
library("tidyverse")

PARAM<-c("Tmax","Tmin")[2]

list.files(pattern="^.+termometro.+csv$")->ffile
stopifnot(length(ffile)>0)

read_delim("reg.umbria.info.csv",delim=";",col_names = TRUE)->ana

purrr:::map_dfr(ffile,.f=function(nomeFile){
  
  anno<-str_remove_all(str_extract(nomeFile,"-(1|2)[0-9]+-"),pattern = "-")


  
  
  str_remove(nomeFile,"-termometro.+csv$")->nomeStazione
  str_remove(nomeStazione,"-pluviometro.+csv$")->nomeStazione
  str_remove(nomeStazione,"-portata.+csv$")->nomeStazione
  
  #Ptrignano e' Petrignano d'assisi e non Petrignano del lago
  if(grepl("12850",nomeFile)) nomeStazione<-"Petrignano_d'assisi"
  
  
  Hmisc::capitalize(tolower(str_trim(nomeStazione,side="both")))->nomeStazione
  
  read_delim(nomeFile,
             delim=";",
             col_names = TRUE,
             n_max=31,
             skip = 1,
             col_types=cols(.default=col_double()))->dati
  
  ifelse(PARAM=="Tmax",2,3)->primaColonna
  seq(primaColonna,by=3,length.out = 12)->colonne
  dati[,c(1,colonne)]->dati

  names(dati)<-c("dd","Gen","Feb","Mar","Apr","Mag","Giu","Lug","Ago","Set","Ott","Nov","Dic")
  dati$stazione<-nomeStazione
  as.integer(anno)->anno
  if(is.na(anno)) browser()
  dati$yy<-anno
  
  which(is.na(dati$dd))->righe
  if(length(righe)) browser()
  if(!all(dati$dd %in% 1:31)) browser()
  
  dati

  
})->finale

finale %>%
  gather(key="mese",value="val",-yy,-dd,-stazione) %>%
  mutate(mese=str_remove(mese,pattern="\\.")) %>%
  mutate(mm=case_when(mese=="Gen"~1,
                      mese=="Feb"~2,
                      mese=="Mar"~3,
                      mese=="Apr"~4,
                      mese=="Mag"~5,
                      mese=="Giu"~6,
                      mese=="Lug"~7,
                      mese=="Ago"~8,
                      mese=="Set"~9,
                      mese=="Ott"~10,
                      mese=="Nov"~11,
                      mese=="Dic"~12)) %>%
  dplyr::select(-mese)->finale

finale %>%
  mutate(stazione=ifelse(stazione=="Calvi_dell'umbria","Calvi_dell_umbria",stazione)) %>%
  mutate(stazione=ifelse(stazione=="Carestello_meteo","Carestello",stazione)) %>%
  mutate(stazione=ifelse(stazione=="La_bruna","Bruna",stazione)) %>%
  mutate(stazione=ifelse(stazione=="Perugia_isa","Perugia",stazione)) %>%
  mutate(stazione=ifelse(stazione=="Perugia_sede","Perugia_fontivegge",stazione)) %>%
  mutate(stazione=ifelse(stazione=="Ponte_s_maria","Ponte_s.maria",stazione)) %>%
  mutate(stazione=ifelse(stazione=="S_benedetto_vecchio","S.benedetto_vecchio",stazione)) %>%
  mutate(stazione=ifelse(stazione=="S_biagio_della_valle","S.biagio_della_valle",stazione)) %>%
  mutate(stazione=ifelse(stazione=="S_gemini","S.gemini",stazione)) %>%
  mutate(stazione=ifelse(stazione=="S_savino","S.savino",stazione)) %>%
  mutate(stazione=ifelse(stazione=="S_silvestro","S.silvestro",stazione)) %>%
  mutate(stazione=ifelse(stazione=="Torre_dell_olmo","Torre_dell'olmo",stazione))->finale


left_join(finale,ana,by=c("stazione"="SiteName_CentroFunzionale"))->finale2
which(is.na(finale2$SiteID))->riga

if(length(riga)) browser()

finale2 %>%
  dplyr::select(yy,mm,dd,val,SiteID) %>%
  spread(key=SiteID,value=val)->daScrivere

min(daScrivere$yy)->annoI
max(daScrivere$yy)->annoF

annoI<-1916 #per conformita con la precipitazione

calendario<-tibble(yymmdd=seq.Date(from=as.Date(glue::glue("{annoI}-01-01")),
         to=as.Date(glue::glue("{annoF}-12-31")),
         by="day")) %>%
  separate(yymmdd,into=c("yy","mm","dd"),sep="-") %>%
  mutate(yy=as.integer(yy),
         mm=as.integer(mm),
         dd=as.integer(dd))

left_join(calendario,daScrivere)->daScrivere


write_delim(daScrivere,glue::glue("{PARAM}_centro_funzionale.csv"),delim=";",col_names = TRUE)