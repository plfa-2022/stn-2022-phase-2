library(pacman)
p_load(data.table)
p_load(lubridate)
p_load(stringi)
p_load(ggplot2)
p_load(plotly)
p_load(caret)
p_load(ggrepel)
p_load(DataExplorer)
p_load(GGally)
p_load(stringr)
p_load(rpart)
p_load(rpart.plot)
p_load(rsample)
p_load(ranger)
p_load(xgboost)
p_load(vtreat)


# install.packages('Rcpp')




#diretorio de trabalho
caminho_arquivos = 'C:\\Users\\pedro_jw08iyg\\Downloads\\STONE_CHALLENGE\\dados'


#carregamento dos arquivos
portfolio_clientes = fread(paste0(caminho_arquivos,'\\portfolio_clientes.csv'), encoding = "UTF-8")
portfolio_comunicados = fread(paste0(caminho_arquivos,'\\portfolio_comunicados.csv'), encoding = "UTF-8")
portfolio_geral = fread(paste0(caminho_arquivos,'\\portfolio_geral.csv'), encoding = "UTF-8")
portfolio_tpv = fread(paste0(caminho_arquivos,'\\portfolio_tpv.csv'), encoding = "UTF-8")






#limpeza das variaveis categoricas


get_char_cols = function(DT){
  num_cols = DT[, sapply(.SD, is.character)]
  return(names(DT)[num_cols])
}



clean_char = function(DT){
  char_cols = get_char_cols(DT)
  DT[, (char_cols):=lapply(.SD, stri_trans_general,id = "Latin-ASCII"), .SDcols = char_cols]
  
}


clean_char(portfolio_clientes)


#ordenando os datasets
portfolio_geral = portfolio_geral[order(nr_documento,contrato_id,dt_ref_portfolio)]
portfolio_comunicados = portfolio_comunicados[order(contrato_id,data_acao)]
portfolio_tpv = portfolio_tpv[order(nr_documento,dt_transacao)]
portfolio_clientes = portfolio_clientes[order(nr_documento)]




portfolio_geral[,safra:=lubridate::ym(safra)]


portfolio_tpv[,dt_transacao:=lubridate::ymd(dt_transacao)]






####ANALISE EXPLORATORIA PARA VARIAVEIS NUMERICAS #####

get_num_cols = function(DT){
  num_cols = DT[, sapply(.SD, is.numeric)]
  return(names(DT)[num_cols])
}


portfolio_geral[,var_created_contratos_por_cliente:=uniqueN(contrato_id), by = nr_documento]


portfolio_geral[,var_created_situacao_inadimplencia:=fcase(
  
  dspp == 0 & dsp == 0, "ok",
  dspp > 0 & dsp  > 0, "inadimplente",
  dspp > 0 & dsp  == 0, "inadimplente",
  default = "erro"
  
)]



contratos_com_problema_dsp = unique(portfolio_geral[var_created_situacao_inadimplencia=='erro',contrato_id])

portfolio_geral[,delta_dsp:=(dsp-shift(dsp,type = 'lag')), by = contrato_id]

#junta os outros contratos com problema de dsp
contratos_com_problema_dsp = c(contratos_com_problema_dsp,unique(portfolio_geral[dsp!=0 & delta_dsp!=1,contrato_id] ))


#validacao dspp
#temos muitas inconsistencias com a dspp, se excluir vamos perder muitos contratos
portfolio_geral[,delta_dspp:=(dspp-shift(dspp,type = 'lag')), by = contrato_id]
table(portfolio_geral[dspp!=0,delta_dspp])
length(unique(portfolio_geral[dspp!=0 & delta_dspp!=1,contrato_id] ))




#transformando para o formato wide
comunicados_wide = dcast(portfolio_comunicados,formula = contrato_id + dt_ref_portfolio + data_acao + acao ~ tipo_acao, value.var = 'status')

table(comunicados_wide[,.(EMAIL,HSM)])

#criando variavel de resposta para email
comunicados_wide[,EMAIL_EFETIVO:=fcase(
  
  EMAIL=='LIDO', 'OK',
  EMAIL=='RESPONDIDO','OK',
  EMAIL=='NAO ENTREGUE','FALHA',
  default = 'NAO_CONCLUSIVO'
  
  
)]


#criando variavel de resposta para hsm
comunicados_wide[,HSM_EFETIVO:=fcase(
  
  HSM=='LIDO', 'OK',
  HSM=='RESPONDIDO','OK',
  HSM=='NAO ENTREGUE','FALHA',
  default = 'NAO_CONCLUSIVO'
  
  
)]

table(comunicados_wide[,.(EMAIL_EFETIVO,HSM_EFETIVO)])


#considerar somente os comunicados que foram realmente efetivos
# comunicados_wide = comunicados_wide[!(EMAIL=='NAO ENTREGUE' & HSM == 'NAO ENTREGUE')]
comunicados_wide = comunicados_wide[EMAIL_EFETIVO=='OK' | HSM_EFETIVO == 'OK']
#ARRUMAR FILTRO

#limpeza de variaveis redundantes 
comunicados_wide[,EMAIL:=NULL]
comunicados_wide[,HSM:=NULL]


#vetor contento os contratos questionados
contratos_questionados = unique(comunicados_wide[,contrato_id])

#filtrando apenas clientes que tiveram alguma inadimplencia e que foram contactados
portifolio_analisado = copy(portfolio_geral[contrato_id %in% contratos_questionados])

#removendo os contratos que apresensataram pelos menos um erro no dsp/dspp
portifolio_analisado = portifolio_analisado[!(contrato_id %in% contratos_com_problema_dsp )]


#juntando a tabela de portifolio com comunicados 
target_intermed = merge(portifolio_analisado[,.(contrato_id,dt_ref_portfolio,dsp,dspp)],comunicados_wide[,.(contrato_id,dt_ref_portfolio,data_acao,acao)], by = c('contrato_id','dt_ref_portfolio'), all.x = T )
target_intermed[, contatos_por_dia:=.N, by=.(contrato_id,dt_ref_portfolio)]


#codificacao do tipo de contato conforme a reta
target_intermed[,codificao_acao_comunicado:=fcase(
  
  dsp==5  & acao == 'campanhaobservacao',1,
  dsp==10 & acao ==  'campanhaparcelamento',2,
  dsp==15 & acao == 'campanhaboletoquitado',3,
  dsp==30 & acao == 'campanhaprenegativacao',4,
  dsp==60 & acao ==  'campanhanegativacao',5,
  dsp==90 & acao == 'campanhaboletoquitado',6,
  dspp==15 & acao == 'campanhaobservacao',1,
  dspp==30 & acao ==  'campanhaparcelamento',2,
  dspp==45 & acao == 'campanhaboletoquitado',3,
  default = NA
  
)]



target_intermed[,campanha_mais_drastica:=max(codificao_acao_comunicado), by=.(contrato_id,dt_ref_portfolio)]

teste = target_intermed[contatos_por_dia>1]

#caso tenha 2 contatos no mesmo dia, vamos considerar apenas a acao mais drastica

target_intermed = target_intermed[contatos_por_dia==1 | (codificao_acao_comunicado==campanha_mais_drastica)]
target_intermed[, conta_chave:=.N, by=.(contrato_id,dt_ref_portfolio)]
table(target_intermed[, conta_chave])
target_intermed[, conta_chave:=NULL]
target_intermed = target_intermed[!is.na(campanha_mais_drastica)]
target_intermed = unique(target_intermed[,.(contrato_id,data_acao,contatos_por_dia,campanha_mais_drastica)])


#left join com a tabela de comunicados pela data de comunicacao
target = merge(portifolio_analisado,target_intermed, by.x = c('contrato_id','dt_ref_portfolio'),by.y = c('contrato_id','data_acao') , all.x = T, suffixes = c('_geral','_comunicados') )

#left join com a target com a portfolio_tpv
target = merge(target, portfolio_tpv, by.x = c('nr_documento','dt_ref_portfolio'), by.y = c('nr_documento','dt_transacao'),all.x = T, suffixes = c("_target","_tpv") )

#ordenando a target
target = target[order(contrato_id,dt_ref_portfolio)]

#criacao de variavel binaria com base se houve ou nao pagamento no dia
# target[,pagamento_binario:=ifelse(dsp==0,1,0)]

#determinacao os ciclos continuos (pagamento/divida)
target[,ciclo_ativo:=rleid(var_created_situacao_inadimplencia), by = contrato_id]

#incluindo o dia de pagamento (ok) no inicio do ciclo de divida
# target[,ciclo_ativo:=ifelse(pagamento_binario==1 & shift(pagamento_binario, type = 'lead')==0, shift(ciclo_ativo, type = 'lead'),ciclo_ativo)]
target[,ciclo_ativo:=ifelse(var_created_situacao_inadimplencia=='ok' & shift(var_created_situacao_inadimplencia, type = 'lead')!='ok', shift(ciclo_ativo, type = 'lead'),ciclo_ativo)]

#incluindo o dia de pagamento parcial no inicio do cilo da divida
target[,ciclo_ativo:=ifelse(var_created_situacao_inadimplencia=='inadimplente_ativo' & shift(var_created_situacao_inadimplencia, type = 'lead')=='inadimplente_inativo', shift(ciclo_ativo, type = 'lead'),ciclo_ativo)]

#criando uma sequencia para cada ciclo
target[,dia_ciclo:=sequence(.N),by = .(ciclo_ativo,contrato_id)]

#criando subciclo com dsp
target[,dsp_binaria:=ifelse(dsp==0,1,0)]
target[,subciclo_dsp:=rleid(dsp_binaria), by = .(contrato_id,ciclo_ativo)]
target[,num_pagamentos_parciais_diario:=cumsum(dsp_binaria), by = .(ciclo_ativo,contrato_id) ]

target[,duracao_ciclo:=.N, by = .(ciclo_ativo,contrato_id) ]

# agregando por ciclos -----

preditivas_agregadas = copy(target)
str(target)

vars_chave = c('contrato_id','ciclo_ativo')


#agregando valores
preditivas_agregadas[,var_agg_dt_ref_portfolio_min:=min(dt_ref_portfolio), by = .(ciclo_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_dt_ref_portfolio_max:=max(dt_ref_portfolio), by = .(ciclo_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_vlr_pgto_realizado_sum:=sum(vlr_pgto_realizado, na.rm = T),by = .(ciclo_ativo,contrato_id)]
preditivas_agregadas[,var_agg_dsp_max:=max(dsp), by = .(ciclo_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_flag_transacao_med:=mean(flag_transacao), by = .(ciclo_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_qtd_transacoes_sum:=sum(qtd_transacoes, na.rm = T),by = .(ciclo_ativo,contrato_id)]
preditivas_agregadas[,var_agg_vlr_tpv_sum:=sum(vlr_tpv, na.rm = T),by = .(ciclo_ativo,contrato_id)]
preditivas_agregadas[,var_agg_duracao_ciclo:=.N, by = .(ciclo_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_num_pagamentos_parciais:=sum(dsp_binaria), by = .(ciclo_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_campanha_mais_drastica_ciclo:=max(campanha_mais_drastica, na.rm = T), by = .(ciclo_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_campanha_mais_drastica_ciclo:=ifelse(is.infinite(var_agg_campanha_mais_drastica_ciclo),0,var_agg_campanha_mais_drastica_ciclo)]

variveis_agregadas = stringr::str_subset(colnames(preditivas_agregadas),'var_agg_')

#Aplicando o filtro de colunas e executando o distinct

filtro_colunas = c(vars_chave,variveis_agregadas)
preditivas_agregadas = unique(preditivas_agregadas[ ,colnames(preditivas_agregadas) %in% filtro_colunas, with=FALSE])
setcolorder(preditivas_agregadas,filtro_colunas)


#adicionando o estado de inadimplencia inicial e final
estado_inicial = copy(target[,.SD[1],by = .(ciclo_ativo,contrato_id),.SDcols=c('var_created_situacao_inadimplencia','vlr_saldo_devedor')])
estado_final = copy(target[,.SD[.N],by = .(ciclo_ativo,contrato_id),.SDcols=c('var_created_situacao_inadimplencia','vlr_saldo_devedor')])


setnames(estado_inicial, old = c('var_created_situacao_inadimplencia','vlr_saldo_devedor'), new = c('situacao_inicial','var_agg_vlr_saldo_devedor_inicial'))
setnames(estado_final, old = c('var_created_situacao_inadimplencia','vlr_saldo_devedor'), new = c('situacao_final','var_agg_vlr_saldo_devedor_final'))


#informacoes complementares dos contratos
#usa do inicio de cada ciclo
status_inicial = copy(target[,.SD[1],by = .(ciclo_ativo,contrato_id),.SDcols=c('status_contrato','prazo','juros_mes','perc_retencao')])


#agregando os dados
preditivas_agregadas = merge(preditivas_agregadas,estado_inicial, by=c('ciclo_ativo','contrato_id'))
preditivas_agregadas = merge(preditivas_agregadas,estado_final, by=c('ciclo_ativo','contrato_id'))
preditivas_agregadas = merge(preditivas_agregadas,status_inicial, by=c('ciclo_ativo','contrato_id'))

#agregando um ciclo mediano por contrato 
#arrumar: verificar se faz uma mediana corrente
#levar em conta o status do contrato e tipo do ciclo (inicio e fim)
preditivas_agregadas[,var_agg_duracao_ciclo_mediana:=median(var_agg_duracao_ciclo, na.rm = T), by = .(contrato_id,status_contrato,situacao_inicial,situacao_final) ]
preditivas_agregadas[,var_agg_tipo_ciclo_numero_obs:=.N, by = .(contrato_id,status_contrato,situacao_inicial,situacao_final) ]
preditivas_agregadas[,var_agg_duracao_ciclo_normalizado:=var_agg_duracao_ciclo/var_agg_duracao_ciclo_mediana]

preditivas_agregadas = preditivas_agregadas[order(contrato_id,ciclo_ativo)]

#limpando a tabela clientes -----
clientes_limpa = unique(portfolio_clientes[,.(nr_documento,tipo_empresa,estado,subsegmento)])
clientes_limpa = clientes_limpa[,.SD[.N],.SDcols=c('tipo_empresa','estado','subsegmento'),by=nr_documento]

chave_cliente_contrato = unique(portfolio_geral[,.(nr_documento,contrato_id)])

clientes_limpa = merge(clientes_limpa,chave_cliente_contrato, by = 'nr_documento')


preditivas_lag = copy(preditivas_agregadas)

variaveis_preditivas_historicas = c('var_agg_qtd_transacoes_sum','var_agg_vlr_pgto_realizado_sum','var_agg_vlr_tpv_sum','var_agg_duracao_ciclo','var_agg_num_pagamentos_parciais','var_agg_campanha_mais_drastica_ciclo','var_agg_vlr_saldo_devedor_inicial','var_agg_vlr_saldo_devedor_final','prazo','juros_mes','perc_retencao','var_agg_duracao_ciclo_normalizado')
#removi var_agg_qtd_transacoes_sum

variaveis_chave_lag = c('ciclo_ativo','contrato_id')

summary(preditivas_lag)

#filtros

#considera somente ciclos de inadimplencia
preditivas_lag = preditivas_lag[situacao_final!='ok']
#numero de obs mínima de 10
preditivas_lag = preditivas_lag[var_agg_tipo_ciclo_numero_obs>10]
#filtro de somente contratos ativos
preditivas_lag = preditivas_lag[status_contrato=='Active']

#determinacao do range de interesse
preditivas_lag = preditivas_lag[var_agg_duracao_ciclo>=5]
preditivas_lag = preditivas_lag[var_agg_duracao_ciclo<=120]


#correcao de inconsistencias
colunas_preditivas_numericas=get_num_cols(preditivas_lag)

for (coluna in colunas_preditivas_numericas){
  
  preditivas_lag[,(coluna):=ifelse(get(coluna)>0,get(coluna),0)]
  
  
}

summary(preditivas_lag)

#filtrar numero minimo de obs para cada ciclo


for (lag_var in 1:3) {
  
  print(lag_var)
  
  
  for (coluna_preditiva_historica in variaveis_preditivas_historicas){
    
    
    preditivas_lag[,(paste0('lag_',lag_var,'_',coluna_preditiva_historica)):=shift(get(coluna_preditiva_historica),n = lag_var,type = 'lag'),by=.(contrato_id)]
    
    
  }
  
  
  #fazendo variaveis por variacao percentual
  # preditivas_lag[,(paste0('lag_',lag_var,'_','var_agg_vlr_saldo_devedor_inicial')):=log( (var_agg_vlr_saldo_devedor_inicial+1)/(shift(var_agg_vlr_saldo_devedor_inicial,n = lag_var,type = 'lag')+1) ),by=.(contrato_id)]
  preditivas_lag[,(paste0('lag_',lag_var,'_','var_agg_vlr_saldo_devedor_inicial')):=( (var_agg_vlr_saldo_devedor_inicial)-(shift(var_agg_vlr_saldo_devedor_inicial,n = lag_var,type = 'lag')) ),by=.(contrato_id)]
  
  
  #considerar apenas o anterior, pois caso contrario vai usar dados futuros
  # preditivas_lag[,(paste0('lag_',lag_var,'_','spread_vlr_saldo_devedor')):=log( (shift(var_agg_vlr_saldo_devedor_final,n = lag_var,type = 'lag')+1)/(shift(var_agg_vlr_saldo_devedor_inicial,n = lag_var,type = 'lag')+1) ),by=.(contrato_id)]
  preditivas_lag[,(paste0('lag_',lag_var,'_','spread_vlr_saldo_devedor')):=( (shift(var_agg_vlr_saldo_devedor_final,n = lag_var,type = 'lag'))-(shift(var_agg_vlr_saldo_devedor_inicial,n = lag_var,type = 'lag')) ),by=.(contrato_id)]
  
  
}


#retirando as linhas com NA (sem info historica minima)
preditivas_lag = na.omit(preditivas_lag)

#removendo variaveis futuras
variaveis_lag = stringr::str_subset(colnames(preditivas_lag),'^lag_')

filtro_colunas_lag = c(variaveis_chave_lag,variaveis_lag)


preditivas_lag = preditivas_lag[ ,colnames(preditivas_lag) %in% filtro_colunas_lag, with=FALSE]
setcolorder(preditivas_lag,filtro_colunas_lag)



#extraindo todos os comunciados ----
#extrai alguma informacoes uteis pontuais
estado_presente = unique(copy(target[!is.na(campanha_mais_drastica),.(contrato_id,ciclo_ativo,dt_ref_portfolio,campanha_mais_drastica,dia_contato=dia_ciclo,num_pagamentos_parciais_diario,vlr_saldo_devedor,juros_mes,perc_retencao,status_contrato, duracao_ciclo)]))
estado_presente[,ultimo_ciclo:=max(ciclo_ativo), by = .(contrato_id)]
estado_presente = estado_presente[status_contrato=='Active']

#ultimo comunicado do ciclo
estado_presente[,ultimo_comunicado_flag:=ifelse(dia_contato==max(dia_contato),T,F), by = .(contrato_id,ciclo_ativo)]

#remove o ulimo ciclo
estado_presente = estado_presente[ciclo_ativo!=ultimo_ciclo]

#caculo do tempo de resposta
estado_presente[,tempo_resposta_cliente:=duracao_ciclo-dia_contato]
#definindo finalmente a nossa variavel target

estado_presente[,comunicacao_efetiva:=fcase(
  campanha_mais_drastica==1 & tempo_resposta_cliente < 5, T,
  campanha_mais_drastica==2 & tempo_resposta_cliente < 5, T,
  campanha_mais_drastica==3 & tempo_resposta_cliente < 10, T,
  campanha_mais_drastica==4 & tempo_resposta_cliente < 10, T,
  campanha_mais_drastica==5 & tempo_resposta_cliente < 10, T,
  campanha_mais_drastica==6 & tempo_resposta_cliente < 10, T,
  default = F)
]

#verifica var target
table(estado_presente[,comunicacao_efetiva])
#incluindo variaveis sazonais
estado_presente[,mes_comunicado:=as.factor(month(dt_ref_portfolio))]
# estado_presente[,dia_da_semana_comunicado:=as.factor(wday(dt_ref_portfolio))]
estado_presente[,dia_do_mes_comunicado:=day(dt_ref_portfolio)]


estado_presente[,tempo_resposta_cliente:=NULL]
estado_presente[,ultimo_comunicado_flag:=NULL]
estado_presente[,duracao_ciclo:=NULL]
estado_presente[,dt_ref_portfolio:=NULL]



#criando uma segunda versao da target
#usar somente as preditivas lag
target_final = merge(x = estado_presente, y = preditivas_lag, by=c('contrato_id','ciclo_ativo') )










#join com a target final
target_final = merge(target_final,clientes_limpa,by='contrato_id')



#exclusao de variaveis
target_final[,contrato_id:=NULL]
target_final[,ciclo_ativo:=NULL]
target_final[,nr_documento:=NULL]
target_final[,ultimo_ciclo:=NULL]
target_final[,status_contrato:=NULL]



str(target_final)

#conversao variaveis

target_final[,tipo_empresa:=as.factor(tipo_empresa)]
target_final[,estado:=as.factor(estado)]
target_final[,subsegmento:=as.factor(subsegmento)]
target_final[,campanha_mais_drastica:=as.factor(campanha_mais_drastica)]



#removendo variaveis redundantes / futuro
# target_final[,tempo_resposta_cliente:=NULL]
#  target_final[,dia_contato:=NULL]
# target_final[,ultimo_comunicado_flag:=NULL]
# target_final[,duracao_ciclo:=NULL]
# target_final[,var_agg_duracao_ciclo:=NULL]
# target_final[,situacao_final:=NULL]
# target_final[,dt_ref_portfolio:=NULL]


# target_final[,var_agg_duracao_ciclo:=NULL]

table(target_final[,comunicacao_efetiva])

arvore_previsao_target_final = rpart::rpart(data = target_final,
                                                     method = "class", 
                                                     formula = comunicacao_efetiva ~ .
)



rpart.plot(arvore_previsao_target_final, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)


resultado = as.data.table(predict(arvore_previsao_target_final, target_final))
setnames(resultado, old = c('FALSE','TRUE'),new = c('false_perc','true_perc'))
resultado = data.table(resultado,target=target_final[,comunicacao_efetiva])
resultado[,pred:=ifelse(true_perc>0.5,T,F)]
mean(resultado[,pred]==resultado[,target])




# separacao treino e teste
target_final_split = rsample::initial_split(target_final,prop = 0.70)

training_target_final = training(target_final_split)

testing_target_final = testing(target_final_split)



arvore_previsao_target_final_treino = rpart::rpart(data = testing_target_final,
                                            method = "class", 
                                            formula = comunicacao_efetiva ~ .
)


resultado_test = as.data.table(predict(arvore_previsao_target_final_treino, testing_target_final))
setnames(resultado_test, old = c('FALSE','TRUE'),new = c('false_perc','true_perc'))
resultado_test = data.table(resultado_test,target=testing_target_final[,comunicacao_efetiva])
resultado_test[,pred:=ifelse(true_perc>0.5,T,F)]
mean(resultado_test[,pred]==resultado_test[,target])




random_forest_final = ranger::ranger(formula  = comunicacao_efetiva ~ .,
                         target_final,
                         num.trees = 100
                         )




rm(resultado_test,resultado)
resultado = data.table(pred=predict(random_forest_final, target_final)[[1]],
                       target_final[,.(comunicacao_efetiva)])
resultado[,comunicacao_efetiva:=as.numeric(comunicacao_efetiva)]
mean(resultado[,pred]==resultado[,comunicacao_efetiva])


#random forest 

resultados = c()

for (i in 1:20) {

target_final_split = rsample::initial_split(target_final,prop = 0.80)

training_target_final = training(target_final_split)

testing_target_final = testing(target_final_split)


random_forest_treino = ranger::ranger(formula  = comunicacao_efetiva ~ .,
                                     training_target_final,
                                     num.trees = 100
)



rm(resultado)
resultado = data.table(pred=predict(random_forest_treino, testing_target_final)[[1]],
                       testing_target_final[,.(comunicacao_efetiva)])

resultado[,comunicacao_efetiva:=as.numeric(comunicacao_efetiva)]
result_int = mean(resultado[,pred]==resultado[,comunicacao_efetiva])

resultados =  c(resultados,result_int)
rm(result_int)
}


hist(resultados)
median(resultados)
# 
# variaveis_explicativas = colnames(target_final)
# variael_target = 'comunicacao_efetiva'
# 
# variaveis_explicativas  = variaveis_explicativas[!(variaveis_explicativas %in% variael_target)]
# 
# target_final_tratada = vtreat::designTreatmentsC(target_final,varlist = variaveis_explicativas)
# 

