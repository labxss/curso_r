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

















col=c("pa_mvm", "pa_cmp", "pa_gestao", "pa_ufmun", "pa_coduni", "pa_munpcn", 
      "pa_proc_id", "pa_cidpri", "pa_cidsec", "pa_idade", "pa_sexo", 
      "pa_racacor", "pa_etnia", "pa_qtdapr", "pa_docorig", "pa_autoriz", 
      "pa_condic", "pa_cnsmed", "pa_regct", "pa_incout", "pa_incurg", 
      "pa_tpups", "pa_tippre", "pa_mn_ind", "pa_cnpjcpf", "pa_cnpjmnt", 
      "pa_cnpj_cc", "pa_fin", "pa_nivcpl", "pa_cbocod", "pa_motsai", 
      "pa_obito", "pa_encerr", "pa_perman", "pa_alta", "pa_transf", 
      "pa_cidcas", "pa_catend", "pa_qtdpro", "pa_valpro", "pa_valapr", 
      "pa_ufdif", "pa_mndif", "pa_dif_val", "nu_vpa_tot", "nu_pa_tot", 
      "pa_indica", "pa_codoco", "pa_flqt", "pa_fler", "pa_vl_cf", 
      "pa_vl_cl", "pa_vl_inc", "pa_srv_c", "pa_ine", "pa_nat_jur")


# para cada competencia e UF
for (ano in 22:10) {
  for (mes in 1:12) {
    for (uf in c(ufs)) {
      unlink("arquivo.dbc", recursive = FALSE, force = FALSE)
      dbc=listadbc[grepl(paste0("AM",uf,ano,str_pad(mes, 2, pad="0")),listadbc,fixed=T)]
      if (length(dbc)>0) {
        unlink("arquivo.dbc", recursive = FALSE, force = FALSE)
        try(download.file(paste0(url,dbc), destfile = "arquivo.dbc"))
        if (file.exists("arquivo.dbc")) {
          ds=read.dbc("arquivo.dbc")
          ds <- ds[,toupper(c("ap_autoriz","ap_cnspcn","am_peso","am_altura"))]
          colnames(ds) <- tolower(colnames(ds))
          ds$ap_cnspcn=
            gsubfn(
              ".", 
              list(
                "{" = "0", "}" = "9", "~" = "5", 
                "\177" = "7", "Ç" = "6", "ä" = "8", 
                "ü" = "4", "é" = "1", "|" = "2", "â" = "3"
              ), 
              iconv(ds$ap_cnspcn, "CP861", "UTF-8")
            )
          pg_persiste(ds[order(ds$ap_autoriz),], "public", "tm_sia_am", TRUE, FALSE)
          pg_roda_query("CREATE INDEX tm_sia_am_ap_autoriz_idx ON public.tm_sia_am (ap_autoriz);")
          rm(ds)
          unlink("arquivo.dbc", recursive = FALSE, force = FALSE)
        }  
      }
      
      
      for (dbc in listadbc[grepl(paste0("PA",uf,ano,str_pad(mes, 2, pad="0")),listadbc,fixed=T)]) {
        unlink("arquivo.dbc", recursive = FALSE, force = FALSE)
        try(download.file(paste0(url,dbc), destfile = "arquivo.dbc"))
        if (file.exists("arquivo.dbc")) {
          ds=read.dbc("arquivo.dbc")
          ds <- subset(ds, substr(PA_PROC_ID,1,2)=="06")
          colnames(ds) <- tolower(colnames(ds))
          pg_persiste(ds[order(ds$pa_autoriz),], "public", "tm_sia_pa", TRUE, FALSE)
          pg_roda_query("CREATE INDEX tm_sia_pa_ap_autoriz_idx ON public.tm_sia_pa (pa_autoriz);")
          campos=paste(colnames(ds), collapse = ", ")
          rm(ds)
          unlink("arquivo.dbc", recursive = FALSE, force = FALSE)
          
          pg_roda_query(
            "alter table public.tm_sia_pa 
        add ap_cnspcn varchar(100) default null,
        add am_peso varchar(3) default null,
        add am_altura varchar(3) default null;
     update public.tm_sia_pa siapa
        set ap_cnspcn = siaam.ap_cnspcn,
            am_peso = siaam.am_peso,
            am_altura = siaam.am_altura
       from public.tm_sia_am siaam
      where siapa.pa_autoriz = siaam.ap_autoriz;
"
          )
          # elimina processamento anterior
          pg_roda_query(paste0("
                        DELETE FROM public.tf_sia_pa
                        where left(pa_gestao::text,2)::int = ", ufdpara[ufs],"
                          and pa_mvm = ", paste0('20',ano,str_pad(mes, 2, pad="0")),"
                        "))
          # PERSISTE
          pg_roda_query(query_persiste)
          pg_roda_query("drop table if exists 
                public.tm_sia_am, public.tm_sia_pa")
          
        }
        
      }
      
      
    }}}




















library(stringr) # lpad
library(gsubfn)


library("RPostgreSQL") # pacote para a conexao com SGBD

# CONEXAO COM O SGBD
pg_conecta = function(
    dbname,
    host,
    port,
    user,
    password
){
  con = DBI::dbConnect(
    dbDriver(drvName = "PostgreSQL"),
    dbname="bd_teste", # nome do banco de dados
    host="177.85.160.74", # servidor, podendo ser o IP, ex., 123.456.78.9
    port=5432, # porta, pot padrao usa-se 5432 para o PostgreSQL
    user = 'teste2', # usuario que criou ao configurar o PostgreSQL
    password = "uma_senha_bem_boa123"
  )
  return(con)
}

# adiciona coisa no banco de dados
pg_persiste = function(dataframe, no_banco, no_tabela, overwrite, append){
  
  con = pg_conecta(dbname, host, port, user, password)
  x=dbWriteTable(
    con, 
    c(no_banco,no_tabela), 
    dataframe, 
    overwrite = overwrite,
    append = append
  )
  
  dbDisconnect(con)
  rm(con)
  
  return(x)
}

pg_roda_query = function(query){
  con = pg_conecta(dbname, host, port, user, password)
  x=dbGetQuery(
    con,
    query
  )
  
  dbDisconnect(con)
  rm(con)
  
  return(x)
}


# elimina processamento anterior
pg_roda_query(
  "drop table if exists 
    public.tm_sia_am, 
    public.tm_sia_pa"
)

ufdpara=c("RO" = "11", "AC" = "12", "AM" = "13", "RR" = "14", "PA" = "15", 
          "AP" = "16", "TO" = "17", "MA" = "21", "PI" = "22", "CE" = "23", 
          "PB" = "25", "PE" = "26", "AL" = "27", "SE" = "28", "BA" = "29", 
          "MG" = "31", "ES" = "32", "RJ" = "33", "SP" = "35", 
          "PR" = "41", "SC" = "42", "RS" = "43", 
          "MS" = "50", "MT" = "51", "GO" = "52", "DF" = "53", "RN" = "24")

ufs=c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO', 
      'AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'RN', 'SE', 
      'ES', 'MG', 'RJ', 'SP', 'PR', 'RS', 'SC', 'DF', 'GO', 'MS', 'MT')

ufs='RJ'

pg_roda_query("
CREATE TABLE IF NOT EXISTS public.tf_sia_pa (
   co_seq_sia_pa SERIAL primary key,
   pa_coduni int8,
   pa_gestao int4,
   ap_cnspcn int8,
   pa_condic varchar(2),
   pa_ufmun int4,
   pa_tpups int2,
   pa_mn_ind char(1),
   pa_cnpjcpf int8,
   pa_cnpjmnt int8,
   pa_cnpj_cc int8,
   pa_mvm int4,
   pa_cmp int4,
   pa_proc_id int8,
   pa_tpfin int2,
   pa_subfin int2,
   pa_nivcpl int2,
   pa_docorig varchar(1),
   pa_autoriz int8,
   pa_cnsmed int8,
   pa_cbocod text,
   pa_motsai int2,
   pa_obito int2,
   pa_encerr int2,
   pa_perman int2,
   pa_alta int2,
   pa_transf int2,
   pa_cidpri varchar(4),
   pa_cidsec varchar(4),
   pa_cidcas varchar(4),
   pa_catend int2,
   pa_idade int2,
   idademin int2,
   idademax int2,
   pa_flidade int2,
   pa_sexo char(1),
   pa_racacor int2,
   pa_munpcn int4,
   pa_qtdpro int4,
   pa_qtdapr int4,
   pa_valpro float8,
   pa_valapr float8,
   pa_ufdif int2,
   pa_mndif int2,
   pa_dif_val float8,
   nu_vpa_tot float8,
   nu_pa_tot float8,
   pa_indica int2,
   pa_codoco int2,
   pa_flqt char(1),
   pa_fler text,
   pa_etnia text,
   pa_srv_c text,
   pa_nat_jur text,
   am_peso varchar(3) default null,
   am_altura varchar(3) default null,
   ds_registro timestamp default current_timestamp
);
CREATE INDEX tf_sia_pa_autoriz_idx ON public.tf_sia_pa USING btree (pa_autoriz);
")

lista_persistido=pg_roda_query("select   
case
when left(pa_gestao::text,2)::int = 11 then 'RO'
when left(pa_gestao::text,2)::int = 12 then 'AC'
when left(pa_gestao::text,2)::int = 13 then 'AM'
when left(pa_gestao::text,2)::int = 14 then 'RR'
when left(pa_gestao::text,2)::int = 15 then 'PA'
when left(pa_gestao::text,2)::int = 16 then 'AP'
when left(pa_gestao::text,2)::int = 17 then 'TO'
when left(pa_gestao::text,2)::int = 21 then 'MA'
when left(pa_gestao::text,2)::int = 22 then 'PI'
when left(pa_gestao::text,2)::int = 23 then 'CE'
when left(pa_gestao::text,2)::int = 25 then 'PB'
when left(pa_gestao::text,2)::int = 26 then 'PE'
when left(pa_gestao::text,2)::int = 27 then 'AL'
when left(pa_gestao::text,2)::int = 28 then 'SE'
when left(pa_gestao::text,2)::int = 29 then 'BA'
when left(pa_gestao::text,2)::int = 31 then 'MG'
when left(pa_gestao::text,2)::int = 32 then 'ES'
when left(pa_gestao::text,2)::int = 33 then 'RJ'
when left(pa_gestao::text,2)::int = 35 then 'SP'
when left(pa_gestao::text,2)::int = 41 then 'PR'
when left(pa_gestao::text,2)::int = 42 then 'SC'
when left(pa_gestao::text,2)::int = 43 then 'RS'
when left(pa_gestao::text,2)::int = 50 then 'MS'
when left(pa_gestao::text,2)::int = 51 then 'MT'
when left(pa_gestao::text,2)::int = 52 then 'GO'
when left(pa_gestao::text,2)::int = 53 then 'DF'
when left(pa_gestao::text,2)::int = 24 then 'RN'
end  || right(pa_mvm::text,4) as origem
  from public.tf_sia_pa
  where ap_cnspcn > 0
  group by 1
  having sum(pa_qtdapr) > 100
  order by 1")


query_persiste="
insert into public.tf_sia_pa (
   pa_coduni, pa_gestao, ap_cnspcn, pa_condic, pa_ufmun, pa_tpups, pa_mn_ind,
   pa_cnpjcpf, pa_cnpjmnt, pa_cnpj_cc, pa_mvm, pa_cmp, pa_proc_id, pa_tpfin, 
   pa_subfin, pa_nivcpl, pa_docorig, pa_autoriz, pa_cnsmed, pa_cbocod, 
   pa_motsai, pa_obito, pa_encerr, pa_perman, pa_alta, pa_transf, pa_cidpri, 
   pa_cidsec, pa_cidcas, pa_catend, pa_idade, idademin, idademax, pa_flidade, 
   pa_sexo, pa_racacor, pa_munpcn, pa_qtdpro, pa_qtdapr, pa_valpro, pa_valapr,
   pa_ufdif, pa_mndif, pa_dif_val, nu_vpa_tot, nu_pa_tot, pa_indica, pa_codoco, 
   pa_flqt, pa_fler, pa_etnia, pa_srv_c, pa_nat_jur, am_peso, am_altura
)
select 
	pa_coduni::int8 pa_coduni,
	pa_gestao::int4 pa_gestao,
	ap_cnspcn::int8,
	pa_condic,
	pa_ufmun::int4 pa_ufmun,
	pa_tpups::int2 pa_tpups,
	pa_mn_ind,
	pa_cnpjcpf::int8 pa_cnpjcpf,
	pa_cnpjmnt::int8 pa_cnpjmnt,
	pa_cnpj_cc::int8 pa_cnpj_cc,
	pa_mvm::int4 pa_mvm,
	pa_cmp::int4 pa_cmp,
	pa_proc_id::int8 pa_proc_id,
	pa_tpfin::int2 pa_tpfin,
	pa_subfin::int2 pa_subfin,
	pa_nivcpl::int2 pa_nivcpl,
	SUBSTR(pa_docorig,1) pa_docorig,
	pa_autoriz::int8 pa_autoriz,
	pa_cnsmed::int8 pa_cnsmed,
	pa_cbocod,
	pa_motsai::int2 pa_motsai,
	pa_obito::int2 pa_obito,
	pa_encerr::int2,
	pa_perman::int2,
	pa_alta::int2,
	pa_transf::int2,
	SUBSTR(pa_cidpri,4),
	SUBSTR(pa_cidsec,4),
	SUBSTR(pa_cidcas,4),
	pa_catend::int2,
	pa_idade::int2,
	idademin::int2,
	idademax::int2,
	pa_flidade::int2,
	SUBSTR(pa_sexo,1),
	pa_racacor::int2,
	pa_munpcn::int4,
	pa_qtdpro::int4,
	pa_qtdapr::int4,
	pa_valpro::float8,
	pa_valapr::float8,
	pa_ufdif::int2,
	pa_mndif::int2,
	pa_dif_val::float8,
	nu_vpa_tot::float8,
	nu_pa_tot::float8,
	pa_indica::int2,
	pa_codoco::int2,
	SUBSTR(pa_flqt,1) pa_flqt,
	pa_fler,
	pa_etnia,
	pa_srv_c,
	pa_nat_jur,
	SUBSTR(am_peso,3),
	SUBSTR(am_altura,3)
from public.tm_sia_pa
order by pa_munpcn, ap_cnspcn, pa_proc_id;"

library("RCurl") # funcao getURL





download.file(
  "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Doc/Informe_Tecnico_SIASUS_2019_07.pdf", 
  destfile = "Informe_Tecnico_SIASUS_2019_07.pdf"
) 



download.file(
  paste0(url,'PAAC2212.dbc'), 
  destfile = "arquivo.dbc"
)

paac2212=read.dbc("arquivo.dbc")