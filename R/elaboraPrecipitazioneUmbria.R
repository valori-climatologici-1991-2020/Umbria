rm(list=objects())
library("tidyverse")
library("janitor")

sensore<-c("pluviometro","termometro")[1]
list.files(pattern=glue::glue("^.+{sensore}.+"))->ffile
stopifnot(length(ffile)!=0)


purrr::map(ffile,.f=~{
  
  str_extract(str_extract(.,"-(2|1)[0-9]{3}-"),"[0-9]+")->yy
  str_extract(str_extract(.,"\\(.+\\)"),"[0-9]+")->idSensore
  read_delim(.,delim=";",col_names = TRUE,skip = 0,col_types = cols(.default = col_double()))->x;
  x$id<-idSensore
  x$yy<-yy
  
  x %>%
    dplyr::select(id,yy,everything())
  
})->listaFile

correggiNomi<-function(x){
  
  names(x)<-c("id","yy","dd",paste(rep(month.abb,each=1),c("Prec"),sep="."))

  x
  
}#fine funzione

purrr::map(listaFile,.f=correggiNomi)->listaFile2

elaboraFile<-function(x,parametro){
  
  
  x %>%
    dplyr::select(id,yy,dd,matches(parametro)) %>%
    gather(key="mm",value="value",-id,-yy,-dd) %>%
    mutate(mm=str_remove(mm,paste0("\\.",parametro))) %>%
    mutate(mm=match(mm,month.abb)) %>%
    filter(!is.na(dd))->xx
   
  xx


  
}


purrr::walk(c("Prec"),.f=function(PARAM){

    purrr::map_dfr(listaFile2,.f=elaboraFile,parametro=PARAM)->dfOut

    dfOut %>%
      spread(value=value,key=id) %>%
      dplyr::select(yy,mm,dd,everything()) %>%
      arrange(yy,mm,dd)->finale
    
    write_delim(finale,glue::glue("{PARAM}_2novembre2020.csv"),delim=";",col_names=TRUE)
    
    pdf(glue::glue("{PARAM}_vis_miss.pdf"),width=12,height = 8)
    print(vis_miss(finale %>% dplyr::select(-yy,-mm,-dd),warn_large_data = FALSE))
    dev.off()
    
    pdf(glue::glue("{PARAM}_vis_miss_1997_2020.pdf"),width=12,height = 8)
    print(vis_miss(finale%>% filter(yy>=1997) %>% dplyr::select(-yy,-mm,-dd) ,warn_large_data = FALSE))
    dev.off()

})
