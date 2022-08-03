#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","glmmTMB","lmtest",
             "caret","e1071","pROC","car","rgl","reshape2")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# FONTE DOS DADOS
#
# BACEN 
# SCR.data - Painel de Operações de Créditos
# https://dadosabertos.bcb.gov.br/dataset/scr_data
#
# Our World in data - dados sobre vacinação contra COVID
# https://ourworldindata.org/
#
# Dados sobre COVID - GOV
# https://covid.saude.gov.br/
#  
# Economatica
# http://www.economatica.com.br
# Selic e IPCA
#
# Filtros efetuados
#
# cnae_subclasse=='PJ - Restaurantes e similares')
#
# período de 01/2017 a 11/2021
# dados posteriores a esse período não estavam disponíveis até 02/08/2022
#
# variável Y foi criada a partir da variável dados$y <- ifelse(dados$carteira_inadimplida_arrastada>0,1,0)


load("bacen_pj_novo.RData")
summary(dados_bacen)

# dados_bacen$total_vaccinations <- ifelse(dados_bacen$total_vaccinations>0,
#                                           log(dados_bacen$total_vaccinations),0)
#                                           
# dados_bacen$total_obitos <- ifelse(dados_bacen$total_obitos>0,
#                                     log(dados_bacen$total_obitos),0)

#quantidade de operações por data base
dados_bacen %>%
  rename(Mês = 1,
         `Quantidade de operações` = 2) %>% 
  group_by(Mês) %>% 
  summarise(`Quantidade de operações` = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Quantidade de operações por UF no período de 2017 a 2021
dados_bacen %>% 
  rename(uf = 10,
         `Quantidade de operações` = 1) %>% 
  group_by(uf) %>% 
  summarise(`Quantidade de operações` = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)


#Modelo Multinível com Interceptos Aleatórios
modelo <- glmmTMB(formula = y ~ total_vaccinations + total_obitos + IPCA_Ibge  + Selic + modalidade 
                          +  porte + tcb + sr + indexador + (1 | uf),
                                     data = dados_bacen,
                                     family = binomial,
                                     REML = TRUE)


#Parâmetros do modelo
summary(modelo)

#Intervalos de confiança
confint(modelo)

#LL
logLik(modelo)

#Apresentando os interceptos aleatórios do modelo
ranef(modelo)[["cond"]][["uf"]] %>% 
  rownames_to_column("uf") %>% 
  rename(v0j = 2) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Para observarmos graficamente o comportamento dos valores de v0j
ranef(modelo)[["cond"]][["uf"]] %>% 
  rownames_to_column("uf") %>% 
  rename(v0j = 2) %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(uf) %>% 
  ggplot(aes(label = round(v0j, digits = 3), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(uf), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = uf, y = 0), size = 3.1, color = "black") +
  coord_flip() +
  labs(x = "uf",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("darkorchid","orange")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")



#GLMM
confusionMatrix(table(predict(modelo,
                              type = "response") >= 0.5,
                      dados_bacen$y == 1)[2:1, 2:1])



#Curvas ROC

#GLMM
roc <- roc(response = dados_bacen$y,
                 predictor = predict(object = modelo,
                                     type = "response"))
roc$auc

# o codigo abaixo gera a curva ROC. 
# dependendo da quantidade e informações a curva ROC pode travar o R. 
ggplotly(
  ggroc(roc, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="orange",
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(roc$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((roc$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

#Curvas Sigmóides para estados específicos (SP, MG, ES, RJ e PR)
dados_bacen %>%
  mutate(fitted_probs_hnlm2 = predict(object = modelo, 
                                      type = "response")) %>%
  filter(uf %in% c("SP", "MG", "ES", "RJ", "PR")) %>%
  ggplot(aes(x = data_base, y = fitted_probs_hnlm2)) +
  #geom_point(alpha = 0.4, size = 4, color = "bisque4") +
  geom_smooth(aes(group = uf, color = uf), 
              method = "lm", formula = y ~ splines::bs(x), se = F, size = 2) +
  scale_colour_viridis_d() +
  labs(x = "Data Base",
       y = "Probabilidade de inadimplência",
       color = "Sigmóide") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "right")

#Curvas Sigmóides para toda a base de dados
dados_bacen %>% 
  mutate(fitted_probs_hnlm2 = predict(object = modelo, 
                                      type = "response")) %>% 
  ggplot(aes(x = data_base, y = fitted_probs_hnlm2)) +
  geom_smooth(aes(group = uf, color = uf), 
              method = "lm", formula = y ~ splines::bs(x), se = F) +
  scale_colour_viridis_d() +
  facet_wrap(~uf) +
  labs(y = "Fitted Probs",
       x = "Data Base") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")


