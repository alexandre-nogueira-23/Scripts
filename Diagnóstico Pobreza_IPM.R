###########################################################################
###########################################################################
###                                                                     ###
###                              Título:                                ###
###                   Indice de Pobreza Multidimensional                ###
###                                                                     ###
###########################################################################
###########################################################################

library("pacman")

#novo

p_load(tidyverse, data.table, here,
       lubridate, sf, fs, geobr, googledrive,
       openxlsx, abjutils, sidrar,
       deflateBR, brazilmaps)    # Install & load packages

options(scipen=999)

drive_auth("gestao@ods-minas.page")

setwd("C:\\Users\\alexandre pichilinga\\Documents\\00_trabalho_observatório\\diagnostico_pobreza")

#   (1) Criando pastas do projeto:
dir_create(c("data",
             "documents",
             "scripts",
             "outputs"))

#   (2) Verificando pastas:
dir_tree()

#--------------------------------------------------------------------------------#
#                              Abrindo bancos                                    #
#--------------------------------------------------------------------------------#

# (1) - Download CAD

# Cadpes Março 2022:
# "https://drive.google.com/file/d/1SIO4W558ky6PFe90P7qtHaxdVPaqcZVN/view?usp=sharing"

# Cadpes Dezembro 2019:
# "https://drive.google.com/file/d/1jywR_eNKN2cjRdeu5M0as151ufzDMTIz/view?usp=sharing"


cadpes <- drive_download("https://drive.google.com/file/d/1jywR_eNKN2cjRdeu5M0as151ufzDMTIz/view?usp=sharing",
                         tempfile(fileext = ".csv"),
                         overwrite = TRUE)

cadpes <- fread(cadpes$local_path, sep = ';',
                select = c(
                  p.cod_familiar_fam_origem, # no cad dez 2019: p.cod_familiar_fam
                  ind_trabalho_infantil_pessoa = p.ind_trabalho_infantil_pessoa,
                  dta_nasc_pessoa = p.dta_nasc_pessoa,
                  cod_parentesco_rf_pessoa = p.cod_parentesco_rf_pessoa,
                  cod_deficiencia_memb = p.cod_deficiencia_memb,
                  cod_sabe_ler_escrever_memb = p.cod_sabe_ler_escrever_memb,
                  cod_sexo_pessoa = p.cod_sexo_pessoa,
                  ind_frequenta_escola_memb = p.ind_frequenta_escola_memb,
                  cod_curso_frequenta_memb = p.cod_curso_frequenta_memb,
                  cod_ano_serie_frequenta_memb = p.cod_ano_serie_frequenta_memb,
                  cod_curso_frequentou_pessoa_memb = p.cod_curso_frequentou_pessoa_memb,
                  cod_concluiu_frequentou_memb = p.cod_concluiu_frequentou_memb,
                  cod_trabalhou_memb = p.cod_trabalhou_memb,
                  cod_afastado_trab_memb = p.cod_afastado_trab_memb,
                  cod_principal_trab_memb = p.cod_principal_trab_memb,
                  ind_dormir_rua_memb = p.ind_dormir_rua_memb,
                  val_remuner_emprego_memb = p.val_remuner_emprego_memb,
                  val_renda_bruta_12_meses_memb = p.val_renda_bruta_12_meses_memb))

#   (2) - Download CAD domicílio Março 2022:

# Caddom Março 2022:
# "https://drive.google.com/file/d/10IGTC1YDNTDp7yK1W5-ShGVkQg7IXFmI/view?usp=sharing"

# Caddom Dezembro 2019:
# "https://drive.google.com/file/d/14aot5mHKdB0eAEfupHr82mCE0lykOZJI/view?usp=sharing"


caddom <- drive_download("https://drive.google.com/file/d/14aot5mHKdB0eAEfupHr82mCE0lykOZJI/view?usp=sharing",
                         tempfile(fileext = ".csv"),
                         overwrite = TRUE)

caddom <- read.csv(caddom$local_path, sep = ',')

caddom <- caddom %>%
  transmute(d.fx_rfpc,
            cod_especie_domic_fam = d.cod_especie_domic_fam,
            cod_local_domic_fam = d.cod_local_domic_fam,
            cd_ibge = d.cd_ibge,
            cod_especie_domic_fam = d.cod_especie_domic_fam,
            cod_est_cadastral_fam = d.cod_est_cadastral_fam,
            dat_atual_fam = d.dat_atual_fam,
            vlr_renda_media_fam = d.vlr_renda_media_fam,
            qtd_comodos_dormitorio_fam = d.qtd_comodos_dormitorio_fam,
            cod_material_domic_fam = d.cod_material_domic_fam,
            cod_agua_canalizada_fam = d.cod_agua_canalizada_fam,
            cod_banheiro_domic_fam = d.cod_banheiro_domic_fam,
            cod_escoa_sanitario_domic_fam = d.cod_escoa_sanitario_domic_fam,
            cod_iluminacao_domic_fam = d.cod_iluminacao_domic_fam,
            qtd_pessoas_domic_fam = d.qtd_pessoas_domic_fam,
            cod_destino_lixo_domic_fam = d.cod_destino_lixo_domic_fam,
            cod_calcamento_domic_fam = d.cod_calcamento_domic_fam,
            d.cod_familiar_fam = as.numeric(d.cod_familiar_fam))

#   (3) - Juntando cadpes e caddom:
caddompes <- cadpes %>%
  mutate(p.cod_familiar_fam = as.numeric(p.cod_familiar_fam)) %>%
  left_join(caddom, by=c("p.cod_familiar_fam"="d.cod_familiar_fam"))

rm(caddom, cadpes)


#--------------------------------------------------------------------------------#
#                         Construindo variáveis                                  #
#--------------------------------------------------------------------------------#

#   (1) - Construindo amostra:
sample_domicilios <- as.data.frame(sample(unique(caddompes$p.cod_familiar_fam), 200000, replace = FALSE)) %>%
  rename("codfam"=`sample(unique(caddompes$p.cod_familiar_fam), 200000, replace = FALSE)`)

sample_domicilios_2 <- caddompes %>%
  filter(p.cod_familiar_fam %in% sample_domicilios$codfam)


#   (2) - Construindo Variáveis ao nível individual:
variaveis_indice <- sample_domicilios_2 %>%
  filter(cod_especie_domic_fam!=3 & # excluindo domicílios coletivos.
           is.na(ind_dormir_rua_memb) &
           cod_est_cadastral_fam==3) %>% # havia também um filtro que retirava do banco os faixa 4 e acima.
  filter(!is.na(vlr_renda_media_fam)) %>%
  mutate(pessoa = 1) %>%
  group_by(p.cod_familiar_fam) %>%
  mutate(n_moradores = sum(pessoa)) %>%
  ungroup() %>%
  mutate(data_do_cadastro = as.Date(dat_atual_fam),
         data_nascimento = as.Date(dta_nasc_pessoa),
         diff_data = difftime(data_do_cadastro, data_nascimento, units = "days"),
         idade = diff_data/365,
         idade = as.integer(idade),
         densidade_dormitorios = if_else(qtd_comodos_dormitorio_fam==0, n_moradores, n_moradores/qtd_comodos_dormitorio_fam),
         densidade_dormitorios = if_else(densidade_dormitorios>3, 1, 0),
         material_domicilio = if_else(cod_material_domic_fam>=5, 1, 0),
         agua_encanada = if_else(cod_agua_canalizada_fam==1, 0, 1),
         banheiro_escoamento = if_else(cod_banheiro_domic_fam==2 | (cod_banheiro_domic_fam==1 & cod_escoa_sanitario_domic_fam>=3), 1, 0),
         energia_eletrica = if_else(cod_iluminacao_domic_fam>=4, 1, 0),
         ca = if_else(idade<18, 1, 0),
         adulto = if_else(idade>=18, 1, 0),
         idade_30_ou_mais = if_else(idade>=30, 1, 0),
         idade_18_ou_mais = if_else(idade>=18, 1, 0),
         pea = if_else(idade>=14 & idade<=64, 1, 0),
         idade_escolar = if_else(idade>=6 & idade<=17, 1, 0),
         jovem_mais_de_18 = if_else(idade>=18 & idade<=29, 1, 0),
         responsavel = if_else(cod_parentesco_rf_pessoa==1, 1, 0),
         filho_resp_ca = if_else((cod_parentesco_rf_pessoa==3 | cod_parentesco_rf_pessoa==4) & idade<18, 1, 0),
         conjuge_resp = if_else(cod_parentesco_rf_pessoa==2, 1, 0),
         pessoa_com_deficiencia = if_else(cod_deficiencia_memb==1, 1, 0),
         idoso = if_else(idade>=65, 1, 0),
         trabalho_infantil= if_else(ind_trabalho_infantil_pessoa==1, 1, 0),
         responsavel = if_else(cod_parentesco_rf_pessoa==1, 1, 0),
         responsavel_analfabeto = if_else(cod_parentesco_rf_pessoa==1 & cod_sabe_ler_escrever_memb==2, 1, 0),
         fundamental_incompleto = if_else(cod_curso_frequenta_memb<=6 |
                                            cod_curso_frequentou_pessoa_memb %in% c(14,15,1,2,3,4,10) |
                                            (cod_curso_frequentou_pessoa_memb %in% c(5,6,7,11) & cod_concluiu_frequentou_memb==2), 1, 0),

         #fundamental_incompleto_antigo = if_else(cod_curso_frequenta_memb<=6 | (cod_curso_frequentou_pessoa_memb<=7 & cod_concluiu_frequentou_memb==2), 1, 0),
         maior_30_ensino_fund_incompleto = if_else(idade>=30 & fundamental_incompleto==1, 1, 0),
         ca_fora_da_escola = if_else(idade>=6 & idade<18 & (ind_frequenta_escola_memb==3 | ind_frequenta_escola_memb==4), 1, 0),
         desafasagem_escolar = case_when(idade==10 & cod_curso_frequenta_memb==4 & cod_ano_serie_frequenta_memb==1 ~ 1,
                                         idade==11 & cod_curso_frequenta_memb==4 & (cod_ano_serie_frequenta_memb==1 | cod_ano_serie_frequenta_memb==2) ~ 1,
                                         idade==12 & cod_curso_frequenta_memb==4 & (cod_ano_serie_frequenta_memb>=1 & cod_ano_serie_frequenta_memb<=3) ~ 1,
                                         idade==13 & cod_curso_frequenta_memb==4 & (cod_ano_serie_frequenta_memb>=1 & cod_ano_serie_frequenta_memb<=4) ~ 1,
                                         idade==14 & cod_curso_frequenta_memb==4 & (cod_ano_serie_frequenta_memb>=1 & cod_ano_serie_frequenta_memb<=5) ~ 1,
                                         idade==15 & cod_curso_frequenta_memb==4 & (cod_ano_serie_frequenta_memb>=1 & cod_ano_serie_frequenta_memb<=6) ~ 1,
                                         idade==16 & cod_curso_frequenta_memb==4 & (cod_ano_serie_frequenta_memb>=1 & cod_ano_serie_frequenta_memb<=7) ~ 1,
                                         idade==17 & cod_curso_frequenta_memb==4 & (cod_ano_serie_frequenta_memb>=1 & cod_ano_serie_frequenta_memb<=8) ~ 1,
                                         idade==9 & cod_curso_frequenta_memb==5 & cod_ano_serie_frequenta_memb==1 ~ 1,
                                         idade==10 & cod_curso_frequenta_memb==5 & cod_ano_serie_frequenta_memb==2 ~ 1,
                                         idade==11 & cod_curso_frequenta_memb==5 & (cod_ano_serie_frequenta_memb==3 | cod_ano_serie_frequenta_memb==2) ~ 1,
                                         idade==12 & cod_curso_frequenta_memb==5 & (cod_ano_serie_frequenta_memb>=4 & cod_ano_serie_frequenta_memb<=3) ~ 1,
                                         idade==13 & cod_curso_frequenta_memb==5 & (cod_ano_serie_frequenta_memb>=5 & cod_ano_serie_frequenta_memb<=4) ~ 1,
                                         idade==14 & cod_curso_frequenta_memb==5 & (cod_ano_serie_frequenta_memb>=6 & cod_ano_serie_frequenta_memb<=5) ~ 1,
                                         idade==15 & cod_curso_frequenta_memb==5 & (cod_ano_serie_frequenta_memb>=7 & cod_ano_serie_frequenta_memb<=6) ~ 1,
                                         idade==16 & cod_curso_frequenta_memb==5 & (cod_ano_serie_frequenta_memb>=8 & cod_ano_serie_frequenta_memb<=7) ~ 1,
                                         idade==17 & cod_curso_frequenta_memb==5 & (cod_ano_serie_frequenta_memb>=9 & cod_ano_serie_frequenta_memb<=8) ~ 1,
                                         idade<6 ~ 0,
                                         idade>17 ~ 0,
                                         TRUE ~ 0),
         jovem_nao_frequenta_escola = if_else(jovem_mais_de_18==1 & ind_frequenta_escola_memb==3, 1, 0),
         jovem_nunca_frequentou_escola = if_else(jovem_mais_de_18==1 & ind_frequenta_escola_memb==4, 1, 0),
         jovem_nao_frequenta_nao_foi_ate_em = if_else(jovem_nao_frequenta_escola==1 &
                                                        (cod_curso_frequentou_pessoa_memb %in% (1:7) |
                                                           cod_curso_frequentou_pessoa_memb==10 |
                                                           cod_curso_frequentou_pessoa_memb==11), 1 , 0),
         jovem_nao_frequenta_foi_ate_em = if_else(jovem_nao_frequenta_escola==1 &
                                                    (cod_curso_frequentou_pessoa_memb %in% (8:9) |
                                                       cod_curso_frequentou_pessoa_memb==12), 1, 0),
         jovem_nao_frequenta_foi_ate_em_nao_concluiu = if_else(jovem_nao_frequenta_foi_ate_em==1 &
                                                                 cod_concluiu_frequentou_memb==2, 1, 0),
         jovem_nao_frequentou_frequenta_nao_concluiu_em = if_else(jovem_nao_frequenta_nao_foi_ate_em==1 |
                                                                    jovem_nao_frequenta_foi_ate_em_nao_concluiu==1 |
                                                                    jovem_nunca_frequentou_escola==1, 1 ,0),
         adultos_que_nao_trabalham = if_else(cod_trabalhou_memb==2 & cod_afastado_trab_memb==2 & idade>=14 & idade<=64, 1, 0),
         adultos_sem_carteira = if_else((cod_principal_trab_memb==2 | cod_principal_trab_memb==3 | cod_principal_trab_memb==5 | cod_principal_trab_memb==7) &
                                          idade>=14 & idade<=64, 1, 0),
         adultos_empregados = if_else((cod_principal_trab_memb==2 | cod_principal_trab_memb==3 | cod_principal_trab_memb==4 | cod_principal_trab_memb==5 |
                                         cod_principal_trab_memb==6 | cod_principal_trab_memb==7 | cod_principal_trab_memb==8 | cod_principal_trab_memb==11) &
                                        idade>=14 & idade<=64, 1, 0),
         ano_cadastro = year(data_do_cadastro),
         n_salarios_minimos_fam = case_when(ano_cadastro==2014 ~ vlr_renda_media_fam/724,
                                            ano_cadastro==2015 ~ vlr_renda_media_fam/788,
                                            ano_cadastro==2016 ~ vlr_renda_media_fam/880,
                                            ano_cadastro==2017 ~ vlr_renda_media_fam/937,
                                            ano_cadastro==2018 ~ vlr_renda_media_fam/954),
         # Novas variáveis
         coleta_lixo = if_else(cod_destino_lixo_domic_fam >= 3, 1, 0),
         maisde30_semfundamental = if_else(idade > 30 & fundamental_incompleto == 1, 1, 0),
         rua_sem_calcamento = ifelse(cod_calcamento_domic_fam >= 2, 1, 0),
         fam_pobre = if_else(vlr_renda_media_fam <= 200, 1, 0),
         maisde18_desocupado = if_else(idade >= 18 & cod_trabalhou_memb ==2, 1, 0),
         maisde18_semfundamental_informal = if_else(idade >= 18 & fundamental_incompleto == 1 &
                                                      cod_principal_trab_memb %in% c(2,3,5,7), 1, 0),
         maisde18_com_rendimento = if_else(idade >= 18 & (val_renda_bruta_12_meses_memb > 0 |
                                                            val_remuner_emprego_memb > 0), 1, 0),
         sem_escoamento = if_else(cod_escoa_sanitario_domic_fam >= 3, 1, 0),
         sem_banheiro = if_else(cod_banheiro_domic_fam==2, 1, 0))

#   (3) - Selecionando variáveis de interesse:
variaveis_indice_2 <- variaveis_indice %>%
  select(cd_ibge, d.fx_rfpc, p.cod_familiar_fam, n_moradores, cod_local_domic_fam,
         data_do_cadastro, idade, densidade_dormitorios,
         material_domicilio,agua_encanada, banheiro_escoamento, energia_eletrica, ca, adulto, jovem_mais_de_18,
         idade_30_ou_mais, idade_18_ou_mais, pea, idade_escolar, responsavel, filho_resp_ca, conjuge_resp,
         pessoa_com_deficiencia,
         idoso, trabalho_infantil, responsavel, responsavel_analfabeto, maior_30_ensino_fund_incompleto,
         ca_fora_da_escola, desafasagem_escolar, jovem_nao_frequentou_frequenta_nao_concluiu_em,
         adultos_que_nao_trabalham, adultos_sem_carteira, adultos_empregados, n_salarios_minimos_fam,
         coleta_lixo, maisde30_semfundamental, rua_sem_calcamento, fam_pobre,
         maisde18_desocupado, maisde18_semfundamental_informal, maisde18_com_rendimento, sem_escoamento,
         sem_banheiro)

#   (4) - Agrupar as medidas a nível domiciliar:
variaveis_indice_3 <- variaveis_indice_2 %>%
  group_by(p.cod_familiar_fam) %>%
  summarise(n_moradores = first(n_moradores),
            cd_ibge = first(cd_ibge),
            cod_local_domic_fam = first(cod_local_domic_fam),
            faixa_de_renda = first(d.fx_rfpc),
            densidade_dormitorios = max(densidade_dormitorios, na.rm = TRUE),
            material_domicilio = max(material_domicilio, na.rm = TRUE),
            agua_encanada = max(agua_encanada, na.rm = TRUE),
            banheiro_escoamento = max(banheiro_escoamento, na.rm = TRUE),
            energia_eletrica = max(energia_eletrica, na.rm = TRUE),
            filho_resp_ca = max(filho_resp_ca, na.rm = TRUE),
            conjuge_resp = max(conjuge_resp, na.rm = TRUE),
            resp_tem_filho_ca = if_else(filho_resp_ca==1 & conjuge_resp==0, 1, 0),
            proporcao_ca = sum(ca, na.rm = TRUE)/n_moradores,
            n_pessoas_com_deficiencia = sum(pessoa_com_deficiencia, na.rm = TRUE),
            proporcao_pessoas_com_deficiencia = if_else(n_pessoas_com_deficiencia>=1,
                                                        n_pessoas_com_deficiencia/n_moradores, 0),
            n_idosos = sum(idoso, na.rm = TRUE),
            proporcao_idosos = if_else(n_idosos>=1, n_idosos/n_moradores, 0),
            trabalho_infantil = max(trabalho_infantil, na.rm = TRUE),
            responsavel_analfabeto= max(responsavel_analfabeto, na.rm = TRUE),
            n_adultos = sum(adulto, na.rm = TRUE),
            n_idade_30_ou_mais = sum(idade_30_ou_mais, na.rm=TRUE),
            n_idade_18_ou_mais = sum(idade_18_ou_mais, na.rm = TRUE),
            porporcao_maior_30_ensino_fund_incompleto = if_else(n_idade_30_ou_mais>=1, sum(maior_30_ensino_fund_incompleto, na.rm = TRUE)/n_idade_30_ou_mais, 0),
            n_ca_fora_da_escola = sum(ca_fora_da_escola, na.rm = TRUE),
            n_idade_escolar = sum(idade_escolar, na.rm = TRUE),
            proporcao_ca_fora_da_escola = if_else(n_idade_escolar>=1, (n_ca_fora_da_escola/n_idade_escolar), 0),
            n_defasagem_escolar = sum(desafasagem_escolar, na.rm = TRUE),
            proporcao_desafasagem_escolar = if_else(n_idade_escolar>=1, (n_defasagem_escolar/n_idade_escolar), 0),
            n_jovem_mais_de_18 = sum(jovem_mais_de_18, na.rm= TRUE),
            prop_jovem_nao_frequentou_frequenta_nao_concluiu_em = if_else(n_jovem_mais_de_18>=1,
                                                                          sum(jovem_nao_frequentou_frequenta_nao_concluiu_em, na.rm = TRUE)/n_jovem_mais_de_18,
                                                                          0),
            n_pea = sum(pea, na.rm = TRUE),
            proprocao_adultos_que_nao_trabalham = if_else(n_pea>=1, (sum(adultos_que_nao_trabalham, na.rm = TRUE)/n_pea), 0),
            n_adultos_empregados = sum(adultos_empregados, na.rm = TRUE),
            proporcao_adultos_sem_carteira = if_else(n_adultos_empregados>=1, (sum(adultos_sem_carteira, na.rm = TRUE)/n_adultos_empregados), 0),
            n_salarios_minimos_fam = first(n_salarios_minimos_fam),
            # Novas variáveis:
            coleta_lixo = max(coleta_lixo, na.rm = TRUE),
            pro_maisde30_semfundamental = if_else(n_idade_30_ou_mais>=1,
                                              sum(maisde30_semfundamental, na.rm = TRUE)/n_idade_30_ou_mais, 0),
            rua_sem_calcamento = max(rua_sem_calcamento, na.rm = TRUE),
            fam_pobre = max(fam_pobre, na.rm = TRUE),
            prop_maisde18_desocupado =  if_else(n_idade_18_ou_mais>=1,
                                           sum(maisde18_desocupado, na.rm = TRUE)/n_idade_18_ou_mais, 0),
            prop_maisde18_semfundamental_informal = if_else(n_idade_18_ou_mais>=1,
                                                         sum(maisde18_semfundamental_informal, na.rm = TRUE)/n_idade_18_ou_mais, 0),
            prop_maisde18_com_rendimento = if_else(n_idade_18_ou_mais>=1,
                                              sum(maisde18_com_rendimento, na.rm = TRUE)/n_idade_18_ou_mais, 0),
            sem_escoamento = max(sem_escoamento, na.rm = TRUE),
            sem_banheiro = max(sem_banheiro, na.rm = TRUE))

#    (5) - Selecionando apenas as vaiáveis do IPM
variaveis_indice_4 <- variaveis_indice_3 %>%
  select(p.cod_familiar_fam, cd_ibge, faixa_de_renda, n_moradores, cod_local_domic_fam,

         coleta_lixo, sem_escoamento, agua_encanada, sem_banheiro,
         porporcao_maior_30_ensino_fund_incompleto, responsavel_analfabeto, proporcao_desafasagem_escolar,
         proporcao_ca_fora_da_escola,
         energia_eletrica, material_domicilio, densidade_dormitorios, rua_sem_calcamento,
         fam_pobre, prop_maisde18_desocupado, prop_maisde18_semfundamental_informal,
         prop_maisde18_com_rendimento) %>%
  mutate(porporcao_maior_30_ensino_fund_incompleto = if_else(porporcao_maior_30_ensino_fund_incompleto>0.5, 1, 0),
         proporcao_desafasagem_escolar = if_else(proporcao_desafasagem_escolar>0.5, 1, 0),
         prop_maisde18_desocupado = if_else(prop_maisde18_desocupado>0.5, 1, 0),
         prop_maisde18_semfundamental_informal = if_else(prop_maisde18_semfundamental_informal>0.5, 1, 0),
         prop_maisde18_com_rendimento = if_else(prop_maisde18_com_rendimento>0.5, 1, 0),
         proporcao_ca_fora_da_escola = ifelse(proporcao_ca_fora_da_escola>0.33, 1, 0))


#    (6) - Tirar linhas com NA ou valores infinitos:

#    Obs: é necessário frisar que todos os casos com missing foram retirados da análise

variaveis_indice_4 <- variaveis_indice_4 %>%
  na.omit()

variaveis_indice_4 <- variaveis_indice_4 %>%
  filter(is.finite(densidade_dormitorios) & is.finite(material_domicilio) &
           is.finite(agua_encanada) & is.finite(coleta_lixo) & is.finite(energia_eletrica) &
           is.finite(fam_pobre) & is.finite(sem_banheiro) & is.finite(proporcao_ca_fora_da_escola) &
           is.finite(rua_sem_calcamento) & is.finite(sem_escoamento) &
           is.finite(responsavel_analfabeto) & is.finite(porporcao_maior_30_ensino_fund_incompleto) &
           is.finite(proporcao_desafasagem_escolar) & is.finite(prop_maisde18_desocupado) &
           is.finite(prop_maisde18_semfundamental_informal) & is.finite(prop_maisde18_com_rendimento))

#    (7) - Determinando famílias privadas (ou seja, pobres)
variaveis_indice_5 <- variaveis_indice_4 %>% #soma os 1 da var coleta_lixo em diante
  mutate(soma_privacoes = rowSums(.[6:21]),
         soma_privacoes_saneamento = rowSums(.[6:9]),
         soma_privacoes_educacao = rowSums(.[10:13]),
         soma_privacoes_padraodevida = rowSums(.[14:17]),
         soma_privacoes_trabalhorenda = rowSums(.[18:21]),
         familia_privada = ifelse(soma_privacoes >= 2.48, 1, 0), #aqui escolhi a soma de 3 privações, média, p/ determinar se a família é privada
         proporcao_privacoes = soma_privacoes/16)

mean(variaveis_indice_5$soma_privacoes)

#--------------------------------------------------------------------------------#
#    Calculando incidência, hiato de pobreza média e incidência ajustada         #
#--------------------------------------------------------------------------------#

#   (1) - Calculando pesos
n_fam <- caddompes %>%
  group_by(p.cod_familiar_fam) %>%
  summarise(n = n()) %>%
  summarise(n = n()) #2658633 p/ dez2020 e 2929747 p/out2021

peso_familia <- 2929747/100000

#   (2) -  Calculando incidência e hiato de pobreza média
ipm_municipal <- variaveis_indice_5 %>%
  group_by(cd_ibge) %>%
  summarise(n_familias_privadas = sum(familia_privada) * peso_familia,
            n_pessoas_privadas = sum(n_moradores[familia_privada==1]) * peso_familia,
            n_pessoas_analisadas_cad = sum(n_moradores) * peso_familia,
            incidencia_pobreza_popcad = n_pessoas_privadas/n_pessoas_analisadas_cad,
            soma_proporcao_privacoes = sum(proporcao_privacoes[familia_privada==1]) * peso_familia, # Dúvida: será que essa soma vale apenas para os pobres?
            hiato_pobreza_media = soma_proporcao_privacoes/n_familias_privadas)


#   (3) - Dados populacionais:
info_sidra("6579")

pop <- get_sidra(x = "6579", # Número da tabela SIDRA
                 variable = "9324",
                 period = "2021", #Mudar ano
                 geo = "City",
                 geo.filter = list("State" = 31)) %>%
  rename("Código.IBGE"=`Município (Código)`,
         "ano" = Ano,
         "pop" = Valor) %>%
  select(Código.IBGE, pop)

#   (3.1) - Salvando
setwd("C:\\Users\\alexandre pichilinga\\Documents\\00_trabalho_observatório\\diagnostico_pobreza\\outputs")

saveRDS(pop, "pop_2021.rds")

#   (4) - Calculando percentual de pobres por município
ipm_municipal_2 <- ipm_municipal %>%
  mutate(cd_ibge = as.character(cd_ibge)) %>%
  left_join(pop, by=c("cd_ibge"="Código.IBGE")) %>%
  mutate(incidencia_pobreza_popmuni = n_pessoas_privadas/pop)


#   (5) - Abrindo mg_dados p/ pegar dados de região geográfica
mg_dados <-  drive_download("https://docs.google.com/spreadsheets/d/1IcYCxW8SJU4ur8LMdcSdSztFEt6Xe_Nx8KG2CsusFLg/edit?usp=sharing",
                                     tempfile(fileext = ".xlsx"),
                                     overwrite = TRUE)

mg_dados <- openxlsx::read.xlsx(mg_dados$local_path, sep = ';') %>%
  filter(ano==2020) %>%
  select(Código.IBGE, `Diretoria;Regional`:`SOCIO_Tipologia;Rural/Urbana;-;Fonte:;IBGE`,
         lon_lat, `SOCIO_idhm;-;Fonte:;IBGE`)

#   (6) - Juntando com ipm_municipal_2
ipm_municipal_3 <- ipm_municipal_2 %>%
  mutate(cd_ibge = as.numeric(cd_ibge)) %>%
  left_join(mg_dados, by=c("cd_ibge"="Código.IBGE"))

#   (7) - Calculando a Incidência da pobreza no estado de MG
tabela_resumo <- ipm_municipal_3 %>%
  summarise(n_pessoas_privadas = sum(n_pessoas_privadas),
            pop_minas = sum(pop),
            pop_cad = sum(n_pessoas_analisadas_cad),
            incidencia_pobreza_minas_popmuni = n_pessoas_privadas/pop_minas,
            incidencia_pobreza_minas_popcad = n_pessoas_privadas/pop_cad)


#--------------------------------------------------------------------------------#
#                               Salvando                                         #
#--------------------------------------------------------------------------------#

setwd("C:\\Users\\alexandre pichilinga\\Documents\\00_trabalho_observatório\\diagnostico_pobreza\\outputs")

write.xlsx(tabela_resumo, "resumo_ipm_out2021_20220504.xlsx")

write.csv(ipm_municipal_3, "ipm_out2021_20220504.csv", row.names = FALSE)

drive_put("ipm_out2021_20220503.csv", path = as_id(""), name = "ipm_out2021_20220503", type = "spreadsheet")


#--------------------------------------------------------------------------------#
#                       Quem é pobre nos dois indicadores                        #
#--------------------------------------------------------------------------------#

tab_comparacao <- variaveis_indice_5 %>%
  transmute(p.cod_familiar_fam,
            faixa_de_renda,
            n_moradores,
            pobre_faixarenda = ifelse(faixa_de_renda %in% c(1, 2), 1, 0),
            familia_privada,
            pobre_também_na_renda = ifelse(familia_privada==1 & faixa_de_renda %in% c(1, 2), 1, 0)) %>%
  summarise(total_fam = n() * peso_familia,
            total_pes = sum(n_moradores) * peso_familia,
            npes_ipmerenda = sum(n_moradores[pobre_também_na_renda==1]) * peso_familia,
            npes_ipm = sum(n_moradores[familia_privada==1]) * peso_familia,
            npes_renda = sum(n_moradores[pobre_faixarenda==1]) * peso_familia,
            perc_ipmerenda_popminas = (npes_ipmerenda/21411923) * 100, #população de minas em 2021
            perc_ipmerenda_popcad = (npes_ipmerenda/total_pes) * 100)


#--------------------------------------------------------------------------------#
#                    Salvando                                                    #
#--------------------------------------------------------------------------------#

setwd("C:\\Users\\alexandre pichilinga\\Documents\\00_trabalho_observatório\\diagnostico_pobreza\\outputs")

write.xlsx(tab_comparacao, "comparacao_ipm e faixas renda_out2021_20220504.xlsx")


#--------------------------------------------------------------------------------#
#                              Mapa municípios                                   #
#--------------------------------------------------------------------------------#

setwd("C:\\Users\\alexandre pichilinga\\Documents\\00_trabalho_observatório\\diagnostico_pobreza\\outputs\\ipm_dez2019")

ipm_municipal_3 <- read.csv("ipm_dez2019_20220528.csv")


minas_cidades <- get_brmap(geo="City", geo.filter = list(State=31))

mapa_reg <- right_join(ipm_municipal_3, minas_cidades, by = c("cd_ibge" = "City")) %>%
  group_by(cd_ibge, Município) %>%
  summarise(geometry= st_union(geometry),
            centroide = st_centroid(geometry))

mapa_reg <- mapa_reg %>%
  separate(centroide, into = c("long", "lat"), sep = ",") %>%
  mutate(long = as.numeric(str_replace(long, "c\\(", "")),
         lat = as.numeric(str_replace(lat, "\\)", "")))

mapa_ipm <- inner_join(ipm_municipal_3, mapa_reg, by = "cd_ibge")

mapa_ipm %>%
  mutate(incidencia_pobreza_popmuni = as.numeric(sprintf("%.1f", (incidencia_pobreza_popmuni * 100)))) %>%
  ggplot(aes(fill=incidencia_pobreza_popmuni, geometry = geometry)) +
  geom_sf(size = 0.5, color = "gray40") +
  scale_fill_gradient(low = "white", high = "darkorange", name = "%", limits = c(0,100), breaks = seq(0,100,50)) +
  theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Percentual de pessoas pobres na população municipal \nsegundo o IPM - Índice de Pobreza Multidimensional") +
  xlab("") +
  ylab("") +
  labs(caption="Fonte: Cadúnico - Dez/2019")

#--------------------------------------------------------------------------------#
#                  Mapa municípios média de privações                            #
#--------------------------------------------------------------------------------#

mapa_ipm %>%
  mutate(media_proporcao_privacoes = as.numeric(sprintf("%.1f", (media_proporcao_privacoes * 100)))) %>%
  ggplot(aes(fill=media_proporcao_privacoes, geometry = geometry)) +
  geom_sf(size = 0.5, color = "gray40") +
  scale_fill_gradient(low = "white", high = "darkorange", name = "%", limits = c(0,50), breaks = seq(0,50,25)) +
  theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Média da proporção de indicadores do IPM \nem que as famílias estão em privação") +
  xlab("") +
  ylab("") +
  labs(caption="Fonte: Cadúnico - Dez/2019")


#--------------------------------------------------------------------------------#
#                 Calculando crescimento percentual da pobreza                   #
#--------------------------------------------------------------------------------#

setwd("C:\\Users\\alexandre pichilinga\\Documents\\00_trabalho_observatório\\diagnostico_pobreza\\outputs")

ipm_dez2020 <- write.xlsx("resumo_ipm_out2021_20220504.xlsx")

#   (1) - Calculando aumento percentual entre os anos

#   Fórmula: aumento_percentual_2020_2021 = (perc_2021 - perc_2020)/perc_2020 * 100)

perc_2021 <- 0.1383

perc_2020 <- 0.1330

(perc_2021 - perc_2020)/perc_2020

0.03984962 * 100

perc_2020 * 100
perc_2021 * 100

#   igual 3.984962% de aumento da pobreza entre 2020 e 2021


#--------------------------------------------------------------------------------#
#                Calculando diferenças percentuais por regional                  #
#--------------------------------------------------------------------------------#

setwd("C:\\Users\\alexandre pichilinga\\Documents\\00_trabalho_observatório\\diagnostico_pobreza\\outputs\\ipm_mar2022")

ipm_mar2022  <- read.csv("ipm_mar2022_20220504.csv")

setwd("C:\\Users\\alexandre pichilinga\\Documents\\00_trabalho_observatório\\diagnostico_pobreza\\outputs\\ipm_dez2019")

ipm_dez2019  <- read.csv("ipm_dez2019_20220528.csv")


#   (1) - Diferenças nos perc entre 2020 e 2021
dif_regionais <-ipm_dez2019 %>%
  left_join(ipm_mar2022, by="cd_ibge") %>%
  group_by(Diretoria.Regional.x) %>%
  summarise(pop_2019 = sum(pop.x),
            pop_2022 = sum(pop.y),
            n_pessoas_privadas_19 = sum(n_pessoas_privadas.x),
            n_pessoas_privadas_22 = sum(n_pessoas_privadas.y),
            perc_pobres_19 = n_pessoas_privadas_19/pop_2019,
            perc_pobres_22 = n_pessoas_privadas_22/pop_2022,
            crescimento_perc = (perc_pobres_22 - perc_pobres_19)/perc_pobres_19)


#--------------------------------------------------------------------------------#
#                      diferenças percentuais por regional                       #
#--------------------------------------------------------------------------------#

minas_cidades <- get_brmap(geo="City", geo.filter = list(State=31))

mapa_reg <- right_join(mg_dados, minas_cidades, by = c("Código.IBGE" = "City")) %>%
  mutate(Diretoria.Regional = str_to_upper(`Diretoria;Regional`)) %>%
  group_by(Diretoria.Regional) %>%
  summarise(geometry= st_union(geometry),
            centroide = st_centroid(geometry))

mapa_reg <- mapa_reg %>%
  separate(centroide, into = c("long", "lat"), sep = ",") %>%
  mutate(long = as.numeric(str_replace(long, "c\\(", "")),
         lat = as.numeric(str_replace(lat, "\\)", "")))

mapa_dif <- dif_regionais %>%
  rename("Diretoria.Regional"=Diretoria.Regional.x) %>%
  mutate(Diretoria.Regional = str_to_upper(Diretoria.Regional)) %>%
  inner_join(mapa_reg, by = "Diretoria.Regional")

mapa_dif %>%
  mutate(crescimento_perc = as.numeric(sprintf("%.1f", (crescimento_perc * 100))),
         rotulo = sprintf("%s (%s%%)", Diretoria.Regional, crescimento_perc)) %>%
  ggplot(aes(fill=crescimento_perc, geometry = geometry)) +
  geom_sf(size = 0.5, color = "gray40") +
  scale_fill_gradient(low = "white", high = "darkorange", name = "%", limits = c(-10,20), breaks = seq(-10,20,10)) +
  theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Tendência da pobreza em MG entre \nDezembro 2019 e Março 2022 com base no IPM",
       subtitle = "") +
  xlab("") +
  ylab("") +
  geom_text(aes(x=long, y=lat, label=str_wrap(rotulo, 17)), size=2.7, fontface="bold")





#                                      ~~~~~ Fim ~~~~~
