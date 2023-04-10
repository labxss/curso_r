rm(list = ls()) # elimina variaveis da memoria
options(timeout=1000) # aumenta limite de tempo para download
memory.limit(size=29000) # regula a memoria RAM

if(!require(RCurl)) {install.packages("RCurl"); require(RCurl)}# funcao getURL
if(!require(downloader)) {install.packages("downloader"); require(downloader)}# funcao download
# if(!require(read.dbc)) {install.packages("read.dbc"); require(read.dbc)}# le arquivo DBC da estrategia tabnet/tabwin de disseminacao
if(!require(read.dbc)) {devtools::install_github("danicat/read.dbc"); require(read.dbc)}
if(!require(stringr)) {install.packages("stringr"); require(stringr)} # lpad str_pad
if(!require(gsubfn)) {install.packages("gsubfn"); require(gsubfn)}


url="ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"

dirdbc="/home/ferre/Downloads/dbc/"

# --------------------------------------
# Parametros
# --------------------------------------

# https://www.gov.br/conitec/pt-br/midias/protocolos/20210428_pcdt-espondilite-ancilosante-1.pdf
cid10=c("M45", "M468") # Espondilite Ancilosante
sigtap=c(
  "0601010019", # ADALIMUMABE (A) 40 MG INJETAVEL- SERINGA PREENCHIDA (POR TRATAMENTO MENSAL)	Revogado desde 06/2010
  "0604380011", # ADALIMUMABE 40 MG INJETAVEL (POR SERINGA PREENCHIDA)	
  "0604380062", # ADALIMUMABE 40 MG INJETÁVEL (POR SERINGA PREENCHIDA)	
  "0604380097", # ADALIMUMABE 40 MG INJETÁVEL (FRASCO AMPOLA)	
  "0604380127", # ADALIMUMABE 40 MG INJETÁVEL ( POR SERINGA PREENCHIDA)( BIOSSIMILAR A)	
  "0604380135", # ADALIMUMABE 40 MG INJETÁVEL (POR SERINGA PREENCHIDA) (BIOSSIMILAR B)
  "0601010027", # ETANERCEPTE (A)25 MG INJETAVEL -FRASCO-AMPOLA (POR TRATAMENTO MENSAL)	Revogado desde 06/2010
  "0601010051", # ETANERCEPTE 50MG INJETAVEL- FRASCO AMPOLA (POR TRATAMENTO MENSAL)	Revogado desde 06/2010
  "0604380020", # ETANERCEPTE 25 MG INJETÁVEL (POR FRASCO-AMPOLA OU SERINGA PREENCHIDA)	
  "0604380038", # ETANERCEPTE 50MG INJETAVEL (POR FRASCO-AMPOLA OU SERINGA PREENCHIDA)(ORIGINADOR)	
  "0604380100", # ETANERCEPTE 50 MG INJETÁVEL (POR FRASCO-AMPOLA OU SERINGA PREENCHIDA)(BIOSSIMILAR A)
  "0601010035", # INFLIXIMABE (A)10 MG/ML 10 ML INJETAVEL (FRASCO-AMPOLA- POR TRATAMENTO MENSAL)	Revogado desde 06/2010
  "0601010043", # INFLIXIMABE 10 MG/ML INJETAVEL (POR FRASCO-AMPOLA 10 ML)	Revogado desde 06/2010
  "0604380046", # INFLIXIMABE 10 MG/ML INJETAVEL (POR FRASCO-AMPOLA COM 10 ML)	
  "0604380054", # INFLIXIMABE 10 MG/ML INJETAVEL (POR FRASCO-AMPOLA COM 10 ML)	
  "0604380119", # INFLIXIMABE 10 MG /ML INJETÁVEL (POR FRASCO-AMPOLA COM 10 ML) (BIOSSIMILAR A)
  "0604380089", # GOLIMUMABE 50 MG INJETÁVEL (POR SERINGA PREENCHIDA
  "0604380070", # CERTOLIZUMABE PEGOL 200 MG/ML INJETÁVEL (POR SERINGA PREENCHIDA)
  "0604690029" # SECUQUINUMABE 150 MG/ML SOLUÇÃO INJETÁVEL (POR SERINGA PREENCHIDA)
) # biologicos


ufs=c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO', 
      'AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'RN', 'SE', 
      'ES', 'MG', 'RJ', 'SP', 'PR', 'RS', 'SC', 'DF', 'GO', 'MS', 'MT')

# lista arquivos dbc do diretorio FTP
aux <-
  getURL(
    url,
    verbose = TRUE,
    ftp.use.epsv = FALSE,
    dirlistonly = TRUE,
    crlf = TRUE
  )
listadbc=strsplit(aux, "\r*\n")[[1]]

ano=18:22
mes=str_pad(1:12, 2, pad="0")


paam_estrutura=data.frame(
  PA_AUTORIZ = numeric(), 
  PA_CMP = numeric(),
  PA_MVM = numeric(),
  PA_CIDPRI = character(),
  PA_CIDSEC = character(),
  PA_PROC_ID = character(),
  PA_QTDAPR = numeric(),
  PA_SEXO = character(),
  PA_IDADE = numeric(),
  PA_MUNPCN = numeric(),
  uf_processamento  = character(),
  AP_CNSPCN = character()
)

pa_estrutura=data.frame(
  PA_AUTORIZ = numeric(), 
  PA_CMP = numeric(),
  PA_MVM = numeric(),    
  PA_CIDPRI = character(),
  PA_CIDSEC = character(),
  PA_PROC_ID = character(),
  PA_QTDAPR = numeric(),
  PA_SEXO = character(),
  PA_IDADE = numeric(),
  PA_MUNPCN = numeric(),
  uf_processamento  = character()
)

am_estrutura=data.frame(
  AP_AUTORIZ = numeric(), 
  AP_PRIPAL = character(),
  AP_CIDPRI = character(),
  AP_CNSPCN = character()
) 

# ------------------------------------
# processamento
# ------------------------------------

for (k in 1:length(ufs)) {

  uf=ufs[k]
  
  aux=as.matrix(expand.grid(uf,ano,mes))
  ufaamm=sort(paste0(
    aux[,1],
    aux[,2],
    aux[,3]
  ))
  
  paam=paam_estrutura
  pa=pa_estrutura
  am=am_estrutura
  
  for (i in 1:length(ufaamm)) {
    print(ufaamm[i])
    
    listadbc_pa=subset(
      listadbc,
      grepl(paste0("^PA",ufaamm[i]), listadbc)
    )
    
    for (j in 1:length(listadbc_pa)) {
      # download.file(paste0(url,listadbc_pa[j]), destfile = "arquivo.dbc") 
      arquivo=paste0(dirdbc,listadbc_pa[j])
      aux=subset(
        read.dbc(arquivo)[,c(
          "PA_AUTORIZ","PA_CMP","PA_MVM" ,"PA_CIDPRI", "PA_CIDSEC", 
          "PA_PROC_ID", "PA_QTDAPR","PA_SEXO", "PA_IDADE", "PA_MUNPCN"
        )],
        PA_CIDPRI %in% cid10 & 
          PA_PROC_ID %in% sigtap
      )
      
      if (nrow(aux)==0) {
        aux=pa
      } else {
        aux$uf_processamento = uf
        pa=rbind(pa,aux)  
      }
      
    }
    
    listadbc_am=subset(
      listadbc,
      grepl(paste0("^AM",ufaamm[i]), listadbc)
    )
    
    if (length(listadbc_am) > 0) {
      
      for (j in 1:length(listadbc_am)) {
        # download.file(paste0(url,listadbc_am[j]), destfile = "arquivo.dbc") 
        arquivo=paste0(dirdbc,listadbc_am[j])
        if (file.exists(arquivo)) {
          aux=subset(
            read.dbc(arquivo)[,c(
              "AP_AUTORIZ", "AP_PRIPAL",
              "AP_CIDPRI",  "AP_CNSPCN"
            )],
            AP_CIDPRI %in% cid10 & AP_PRIPAL %in% sigtap
          )
          am=rbind(am,aux)        
        }
      }
      
      am2=unique(am[,c("AP_AUTORIZ","AP_CNSPCN")])
      
      am2$AP_CNSPCN=
        gsubfn(
          ".", 
          list(
            "{" = "0", "}" = "9", "~" = "5", 
            "\177" = "7", "Ç" = "6", "ä" = "8", 
            "ü" = "4", "é" = "1", "|" = "2", "â" = "3"
          ), 
          iconv(am2$AP_CNSPCN, "CP861", "UTF-8")
        )
      
      aux = merge(
        pa, am2, 
        by.x=c("PA_AUTORIZ"), 
        by.y=c("AP_AUTORIZ"), 
        all.x = TRUE
      )
      
    } 
    else {
      aux=pa
      aux$AP_CNSPCN = NA
    } # length(listadbc_am) > 0
    
    paam=rbind(paam,aux)
  } # ufaamm[i]
  
  write.csv(paam,paste0("sia_pa_",uf,".csv"))
  rm(am)
  rm(pa)
  rm(paam)
  rm(aux)
}
