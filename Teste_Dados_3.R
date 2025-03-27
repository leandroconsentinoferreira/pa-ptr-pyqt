#### 00. Incluir bibliotecas e funcoes ####
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                             
if(!require(plyr)) install.packages("plyr")
require(plyr)
if(!require(car)) install.packages("car")   
library(car)                                
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)                    
if(!require(DescTools)) install.packages("DescTools") 
library(DescTools)
if(!require(emmeans)) install.packages("emmeans") 
library(emmeans)
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(ggpubr)) install.packages("ggpubr") 
library(ggpubr)
if(!require(RVAideMemoire)) install.packages("RVAideMemoire") 
library(RVAideMemoire)  
if(!require(psych)) install.packages("psych") 
library(psych)       
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)  
if(!require(goft)) install.packages("goft") 
library(goft)
if(!require(rcompanion)){install.packages("rcompanion")}
library(rcompanion)
if(!require(FSA)){install.packages("FSA")}
library(FSA)
if(!require(ggsignif)){install.packages("ggsignif")}
library(ggsignif)
if(!require(tidytext)){install.packages("tidytext")}
library(tidytext)


#### fim ####
#### 01. Importando e transformando dados ####

Dados <- read.csv2("consolidados2.csv")

Dados <- data.frame(lapply(Dados, function(x){gsub("Nao","Não",x)}))

Dados <- data.frame(lapply(Dados, function(x){gsub("ug","µg",x)}))

Dados <- data.frame(lapply(Dados, function(x){gsub("SHR 17 Semanas Não Tratados","SHR-17-NT",x)}))
Dados <- data.frame(lapply(Dados, function(x){gsub("SHR 17 Semanas Tratados","SHR-17-T",x)}))
Dados <- data.frame(lapply(Dados, function(x){gsub("SHR 21 Semanas Não Tratados","SHR-21-NT",x)}))
Dados <- data.frame(lapply(Dados, function(x){gsub("SHR 21 Semanas Tratados","SHR-21-T",x)}))
Dados <- data.frame(lapply(Dados, function(x){gsub("WKY 21 Semanas Não Tratados","WKY-21-NT",x)}))
Dados <- data.frame(lapply(Dados, function(x){gsub("WKY 21 Semanas Tratados","WKY-21-T",x)}))

Dados <- transform(Dados,
                   Grupo = as.factor(Grupo),
                   Animal = as.factor(Animal),
                   Dosagem = as.factor(Dosagem),
                   PA_mmHg = as.double(PA_mmHg),
                   Desvio_PA = as.double(Desvio_PA),
                   Ptr_cmH2O = as.double(Ptr_cmH2O),
                   Desvio_Ptr = as.double(Desvio_Ptr),
                   Perturbacao = as.factor(Perturbacao))

Dados$Grupo = factor(Dados$Grupo,
                     levels = c("SHR-17-NT",
                                "SHR-17-T",
                                "SHR-21-NT",
                                "SHR-21-T",
                                "WKY-21-NT",
                                "WKY-21-T"))


Dados$Dosagem = factor(Dados$Dosagem,
                       levels = c("PBS",
                                  "3 µg/kg",
                                  "10 µg/kg",
                                  "30 µg/kg",
                                  "100 µg/kg"))

Lista_Grupos = unique(Dados$Grupo)

Lista_Dosagens = unique(Dados$Dosagem)

Lista_Perturbacoes = unique(Dados$Perturbacao)

#### fim ####
#### 02. Selecionando Perturbacoes ####

PBS <- Dados[Dados$Dosagem == "PBS",]
P1 <- Dados[Dados$Perturbacao == 1,]
P2 <- Dados[Dados$Perturbacao == 2,]
P3 <- Dados[Dados$Perturbacao == 3,]
P14 <- Dados[Dados$Perturbacao == 14,]
P15 <- Dados[Dados$Perturbacao == 15,]

#### fim ####

#### 03. Tirando médias das perturbacoes selecionadas #####

PA_PBS_2 <- aggregate(PA_mmHg ~ Animal * Grupo, PBS, mean) # Agrupando por grupo

Ptr_PBS_2 <- aggregate(Ptr_cmH2O ~ Animal * Grupo, PBS, mean)

PA_P1 <- aggregate(PA_mmHg ~ Dosagem * Grupo, P1, mean)
Ptr_P1 <- aggregate(Ptr_cmH2O ~ Dosagem * Grupo, P1, mean) 

PA_P2 <- aggregate(PA_mmHg ~ Dosagem * Grupo, P2, mean)
Ptr_P2 <- aggregate(Ptr_cmH2O ~ Dosagem * Grupo, P2, mean)

PA_P3 <- aggregate(PA_mmHg ~ Dosagem * Grupo, P3, mean)
Ptr_P3 <- aggregate(Ptr_cmH2O ~ Dosagem * Grupo, P3, mean)

PA_P14 <- aggregate(PA_mmHg ~ Dosagem * Grupo, P14, mean)
Ptr_P14 <- aggregate(Ptr_cmH2O ~ Dosagem * Grupo, P14, mean)

PA_P15 <- aggregate(PA_mmHg ~ Dosagem * Grupo, P15, mean)
Ptr_P15 <- aggregate(Ptr_cmH2O ~ Dosagem * Grupo, P15, mean)

#### fim ####


#### 09. Scheirer-Ray-Hare e Mann-Whitney-Wilcoxon ####

scheirerRayHare(PA_mmHg ~ Dosagem + Grupo, data = P1)

# scheirerRayHare(PA_mmHg ~ Dosagem + Grupo,
#                 data = Dados[Dados$Perturbacao == 1,])

scheirerRayHare(PA_mmHg ~ Dosagem + Grupo, data = P2)
scheirerRayHare(PA_mmHg ~ Dosagem + Grupo, data = P3)
scheirerRayHare(PA_mmHg ~ Dosagem + Grupo, data = P15)

scheirerRayHare(Ptr_cmH2O ~ Dosagem + Grupo, data = P1)
scheirerRayHare(Ptr_cmH2O ~ Dosagem + Grupo, data = P2)
scheirerRayHare(Ptr_cmH2O ~ Dosagem + Grupo, data = P3)
scheirerRayHare(Ptr_cmH2O ~ Dosagem + Grupo, data = P15)

Teste <- Dados[Dados$Perturbacao %in% c(1,15),]

wilcox.test(PA_mmHg ~ Perturbacao, data = Teste)

Teste %>% group_by(Grupo, Dosagem) %>% 
  do(w = wilcox.test(PA_mmHg ~ Perturbacao,data=., paired=FALSE)) %>% 
  summarise(Grupo, Dosagem, Wilcox = w$p.value)

ddply(Teste, .(Grupo, Dosagem), 
   function(x) { wilcox.test(Ptr_cmH2O~Perturbacao,data=x)$p.value}) -> Teste2

## https://www.youtube.com/watch?v=ekvKa0HzeGo&ab_channel=FernandaPeres

#### fim ####

#### 10. Post hoc Dunn ####

Consolidado_Dosagens_PA <- data.frame(matrix(ncol = 11, nrow = 0))
Consolidado_Dosagens_Ptr <- data.frame(matrix(ncol = 11, nrow = 0))

perturbacao <- "3"
dosagem <- "30 µg/kg"

for (dosagem in Lista_Dosagens){
  for(perturbacao in c("1","2","3","4")){
    DT_Dosagem_PA <- dunn_test(PA_mmHg ~ Grupo,
                                 data=Dados[Dados$Dosagem == dosagem & 
                                              Dados$Perturbacao == perturbacao,])
    DT_Dosagem_Ptr <- dunn_test(Ptr_cmH2O ~ Grupo,
                                  data=Dados[Dados$Dosagem == dosagem & 
                                               Dados$Perturbacao == perturbacao,],
                                p.adjust.method = "hommel")
    
    
    Dosagem_PA <- rep(dosagem, nrow(DT_Dosagem_PA))
    Perturbacao_PA <- rep(perturbacao, nrow(DT_Dosagem_PA))
    Dosagem_Ptr <- rep(dosagem, nrow(DT_Dosagem_Ptr))
    Perturbacao_Ptr <- rep(perturbacao, nrow(DT_Dosagem_Ptr))
    
    df_aux_PA <- cbind(DT_Dosagem_PA,Dosagem_PA,Perturbacao_PA)
    df_aux_Ptr <- cbind(DT_Dosagem_Ptr,Dosagem_Ptr,Perturbacao_Ptr)
    
    Consolidado_Dosagens_PA <- rbind(Consolidado_Dosagens_PA,df_aux_PA)
    Consolidado_Dosagens_Ptr <- rbind(Consolidado_Dosagens_Ptr,df_aux_Ptr)
    
    print(Consolidado_Dosagens_PA)
    print(Consolidado_Dosagens_Ptr)
  }
  
}

write.csv2(Consolidado_Dosagens_PA, "DT_Dosagens_PA.csv",fileEncoding = "UTF-8")
write.csv2(Consolidado_Dosagens_Ptr, "DT_Dosagens_Ptr.csv",fileEncoding = "UTF-8")


Consolidado_Grupos_PA <- data.frame(matrix(ncol = 11, nrow = 0))
Consolidado_Grupos_Ptr <- data.frame(matrix(ncol = 11, nrow = 0))

for (grupo in Lista_Grupos){
  for(perturbacao in c("1","2","3","4")){
    
    DT_Grupo_PA = dunn_test(PA_mmHg ~ Dosagem,
                            data=Dados[Dados$Grupo == grupo &
                                         Dados$Perturbacao == perturbacao,])
    DT_Grupo_Ptr = dunn_test(Ptr_cmH2O ~ Dosagem,
                             data=Dados[Dados$Grupo == grupo &
                                          Dados$Perturbacao == perturbacao,])
    
    Grupo_PA <- rep(grupo, nrow(DT_Grupo_PA))
    Perturbacao_PA <- rep(perturbacao, nrow(DT_Grupo_PA))
    Grupo_Ptr <- rep(grupo, nrow(DT_Grupo_Ptr))
    Perturbacao_Ptr <- rep(perturbacao, nrow(DT_Grupo_Ptr))
    
    df_aux_PA <- cbind(DT_Grupo_PA,Grupo_PA,Perturbacao_PA)
    df_aux_Ptr <- cbind(DT_Grupo_Ptr,Grupo_Ptr,Perturbacao_Ptr)
    
    Consolidado_Grupos_PA <- rbind(Consolidado_Grupos_PA,df_aux_PA)
    Consolidado_Grupos_Ptr <- rbind(Consolidado_Grupos_Ptr,df_aux_Ptr)
    
    print(Consolidado_Grupos_PA)
    print(Consolidado_Grupos_Ptr)
  }
}

write.csv2(Consolidado_Grupos_PA, "DT_Grupos_PA.csv",fileEncoding = "UTF-8")
write.csv2(Consolidado_Grupos_Ptr, "DT_Grupos_Ptr.csv",fileEncoding = "UTF-8")

#### fim ####
#### 11. Plots P1/P15 ####

# Plots P1 e P15

# Criando novo data frame

df_perturbacoes_1_15 <- Dados[0,] # cria DF vazio porém com as colunas do DF origem


# comparacao_1_1 <- Dados[Dados$Perturbacao == 1 &
#                         Dados$Dosagem == "PBS",]
# 
# comparacao_1_15 <- Dados[Dados$Perturbacao == 15 &
#                           Dados$Dosagem == "PBS",]
# 
# comparacao_1 <- rbind(comparacao_1_1, comparacao_1_15)
# 
# comp_texto <- rep("comp_1", nrow(comparacao_1))
# df_perturbacoes_1_15 <- cbind(comparacao_1, comp_texto)

#

comparacao_2_15 <- Dados[(Dados$Perturbacao==15 &
                        Dados$Dosagem == "PBS"),]

comparacao_2_1 <- Dados[(Dados$Perturbacao==1 & 
                           Dados$Dosagem == "3 µg/kg"),]

comparacao_2 <- rbind(comparacao_2_15, comparacao_2_1)


comp_texto <- rep("comp_2", nrow(comparacao_2))
df_aux <- cbind(comparacao_2, comp_texto)
df_perturbacoes_1_15 <- rbind(df_perturbacoes_1_15, df_aux)

#

comparacao_3_15 <- Dados[(Dados$Perturbacao==15 &
                            Dados$Dosagem == "3 µg/kg"),]


comparacao_3_1 <- Dados[(Dados$Perturbacao==1 & 
                           Dados$Dosagem == "10 µg/kg"),]

comparacao_3 <- rbind(comparacao_3_15, comparacao_3_1)

comp_texto <- rep("comp_3", nrow(comparacao_3))
df_aux <- cbind(comparacao_3, comp_texto)
df_perturbacoes_1_15 <- rbind(df_perturbacoes_1_15, df_aux)

#

comparacao_4_15 <- Dados[(Dados$Perturbacao==15 &
                            Dados$Dosagem == "10 µg/kg"),]


comparacao_4_1 <- Dados[(Dados$Perturbacao==1 & 
                           Dados$Dosagem == "30 µg/kg"),]

comparacao_4 <- rbind(comparacao_4_15, comparacao_4_1)

comp_texto <- rep("comp_4", nrow(comparacao_4))
df_aux <- cbind(comparacao_4, comp_texto)
df_perturbacoes_1_15 <- rbind(df_perturbacoes_1_15, df_aux)

#

comparacao_5_15 <- Dados[(Dados$Perturbacao==15 &
                            Dados$Dosagem == "30 µg/kg"),]


comparacao_5_1 <- Dados[(Dados$Perturbacao==1 & 
                           Dados$Dosagem == "100 µg/kg"),]

comparacao_5 <- rbind(comparacao_5_15, comparacao_5_1)

comp_texto <- rep("comp_5", nrow(comparacao_5))
df_aux <- cbind(comparacao_5, comp_texto)
df_perturbacoes_1_15 <- rbind(df_perturbacoes_1_15, df_aux)

#

# comparacao_6_1 <- Dados[Dados$Perturbacao == 1 &
#                         Dados$Dosagem == "100 µg/kg",]
# 
# comparacao_6_15 <- Dados[Dados$Perturbacao == 15 &
#                           Dados$Dosagem == "100 µg/kg",]
# 
# comparacao_6 <- rbind(comparacao_6_1, comparacao_6_15)
# 
# comp_texto <- rep("comp_6", nrow(comparacao_6))
# df_aux <- cbind(comparacao_6, comp_texto)
# df_perturbacoes_1_15 <- rbind(df_perturbacoes_1_15, df_aux)

# row.names(df_perturbacoes_1_15) <-NULL

grupo <- "SHR-17-T"

Lista_Grupos = unique(Dados$Grupo)

comp.labs <- c("PBS x 3 µg/kg","3 µg/kg x 10 µg/kg","10 µg/kg x 30 µg/kg","30 µg/kg x 100 µg/kg")
names(comp.labs) <- c("comp_2","comp_3","comp_4","comp_5")

for (grupo in Lista_Grupos){
  
  df_perturbacoes_1_15_grupo <- 
    df_perturbacoes_1_15[df_perturbacoes_1_15$Grupo == grupo,]
  
  
  g_p1_p15_pa <- ggboxplot(df_perturbacoes_1_15_grupo, x = "Perturbacao", y = "PA_mmHg",
                           color = "Perturbacao", palette = "lancet",
                           ggtheme = theme_bw(base_size = 11),size=0.6,
                           order=c(15,1),
                           facet.by = "comp_texto", 
                           short.panel.labs=TRUE, ylab='Pressão Arterial (mmHg)') +
    ggtitle(paste("Comparativo entre perturbações do grupo ",grupo," (PA)",sep=""))+
    stat_compare_means(aes(label=paste0("(",..p.signif..,")")))+
    # stat_summary(fun=mean,geom="text",aes(label=Dosagem))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(face='bold'),
          axis.title = element_text(size="12"),
          strip.text.x = element_text(face='bold'),
          legend.position = "bottom",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black"))+
    labs(color="Perturbação")+
    # scale_x_discrete(limits=c(1,15))+
    # facet_grid(~comp_texto, labeller=labeller(comp_texto=comp.labs))
    facet_wrap(~comp_texto, labeller=labeller(label_wrap_gen(width=13),
                                              comp_texto=comp.labs),ncol=6)
  
  print(g_p1_p15_pa)
  
  # https://datavizpyr.com/how-to-reorder-barplots-with-facetting-with-ggplot2-in-r/
  
  # https://stackoverflow.com/questions/69001545/ggplot-how-to-show-boxplots-in-a-given-order
  
  # primeira tentativa, consegui trocar ordem das perturbações
  
  # ggplot(df_perturbacoes_1_15_grupo, aes(reorder_within(Perturbacao, desc(Perturbacao),
  #                                                       comp_texto), PA_mmHg)) +
  #   geom_boxplot() +
  #   scale_x_reordered() +
  #   facet_wrap(. ~ comp_texto, scales = "free_x")
  # 
  # # tentativa 2 por filter sem sucesso
  # 
  # df_perturbacoes_1_15_grupo %>% filter(comp_texto %in% c("comp_2", "comp_3")) %>% 
  #   mutate(Perturbacao = reorder_within(Perturbacao, desc(Perturbacao), comp_texto)) %>% 
  #   ggplot(aes(y=PA_mmHg,x=Perturbacao))+
  #   geom_boxplot()+
  #   scale_x_reordered() +
  #   facet_wrap(. ~ comp_texto, scales = "free_x")
  # 
  # # tentativa 3 ordenar facets "na mão"
  # 
  # facet_group <- c("comp_1","comp_1",
  #                  "comp_2","comp_2",
  #                  "comp_3","comp_3",
  #                  "comp_4","comp_4",
  #                  "comp_5","comp_5",
  #                  "comp_6","comp_6")
  # 
  # in_facet <- c("1","15",
  #               "15","1",
  #               "15","1",
  #               "15","1",
  #               "15","1",
  #               "1","15")
  # 
  # 
  # df_perturbacoes_1_15_grupo %>% 
  #   ggplot(aes(Perturbacao,y=PA_mmHg,group=Dosagem))+
  #   geom_boxplot()+
  #   facet_wrap(. ~ comp_texto, scales = "free_x")
  # 
  
  
  
  ggsave(paste("P1_P15_",grupo,"_PA.png",sep=""),
         plot=g_p1_p15_pa,device="png",width=20,height=10,dpi=150,units="cm")
  
  Sys.sleep(3)
  
  g_p1_p15_ptr <- ggboxplot(df_perturbacoes_1_15_grupo, x = "Perturbacao", y = "Ptr_cmH2O",
                           color = "Perturbacao", palette = "lancet",
                           ggtheme = theme_bw(base_size = 11),size=0.6,
                           order=c(15,1),
                           facet.by = "Dosagem", 
                           short.panel.labs=TRUE, ylab=expression("Pressão Traqueal (cmH "[2]*"O)")) +
    ggtitle(paste("Comparativo entre perturbações do grupo ",grupo," (Ptr)",sep=""))+
    stat_compare_means(aes(label=paste0("(",..p.signif..,")")))+
    # stat_summary(fun=mean,geom="text",aes(label=Dosagem))+#,vjust=6)+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(face='bold'),
          axis.title = element_text(size="12"),
          strip.text.x = element_text(face='bold'),
          legend.position = "bottom",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black"))+
    labs(color="Perturbação")+
    facet_wrap(~comp_texto, labeller=labeller(label_wrap_gen(width=13),
                                              comp_texto=comp.labs),ncol=6)
  
  print(g_p1_p15_ptr)
  
  ggsave(paste("P1_P15_",grupo,"_Ptr.png",sep=""),
         plot=g_p1_p15_ptr,device="png",width=20,height=10,dpi=150,units="cm")
  
  Sys.sleep(3)

}




dosagem = "PBS"

for (dosagem in Lista_Dosagens){
  dados_p1_p15 <- Dados[Dados$Perturbacao %in% c(1,15) &
                          Dados$Dosagem == dosagem,
                        ]
  
  g_p1_p15_pa <- ggboxplot(dados_p1_p15, x = "Perturbacao", y = "PA_mmHg",
            color = "Perturbacao", palette = "lancet",
            ggtheme = theme_bw(base_size = 11),size=0.6,
            facet.by = "Grupo",
            short.panel.labs=TRUE, ylab='Pressão Arterial (mmHg)') +
    ggtitle(paste("Comparativo entre primeira perturbação e última perturbação da etapa ",dosagem," (PA)",sep=""))+
    # stat_compare_means(label.y=230)+
    stat_compare_means(aes(label=paste0("(",..p.signif..,")")))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(face='bold'),
          axis.title = element_text(face="bold",size="12"),
          strip.text.x = element_text(face='bold'),
          legend.position = "bottom",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black"))+
    labs(color="Perturbação")+
    facet_wrap(~ Grupo, labeller = label_wrap_gen(width=13),ncol=6)
  
  print(g_p1_p15_pa)
  
  ggsave(paste("P1_P15_",gsub(" µg/kg","u",dosagem),"_PAXXX.png",sep=""),
         plot=g_p1_p15_pa,device="png",width=20,height=10,dpi=150,units="cm")
  
  Sys.sleep(2)
  
  
  g_p1_p15_ptr <- ggboxplot(dados_p1_p15, x = "Perturbacao", y = "Ptr_cmH2O",
                           color = "Perturbacao", palette = "lancet",
                           ggtheme = theme_bw(base_size = 11),size=0.6,
                           facet.by = "Grupo", # ylim=c(2.5,6),
                           short.panel.labs=TRUE, ylab=expression("Pressão Traqueal (cmH "[2]*"O)")) +
    ggtitle(paste("Comparativo entre primeira perturbação e última perturbação da etapa ",dosagem," (Ptr)",sep=""))+
    # stat_compare_means(label.y=230)+
    stat_compare_means(aes(label=paste0("(",..p.signif..,")")))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(face='bold'),
          axis.title = element_text(face="bold",size="12"),
          strip.text.x = element_text(face='bold'),
          legend.position = "bottom",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black"))+
    labs(color="Perturbação")+
    facet_wrap(~ Grupo, labeller = label_wrap_gen(width=13),ncol=6)
  
  print(g_p1_p15_ptr)
  
  ggsave(paste("P1_P15_",gsub(" µg/kg","u",dosagem),"XXX_Ptr.png",sep=""),
         plot=g_p1_p15_ptr,device="png",width=20,height=10,dpi=150,units="cm")
  
  Sys.sleep(2)
  
  
}

#### fim ####

#### Plots PA Ptr nas P1, P2, e P3 ####

perturbacao = 3

Select_Perturbacoes = c(1,2,3,4)

for (perturbacao in Select_Perturbacoes){
  Dados_Perturbacao <- Dados[Dados$Perturbacao == perturbacao,]
  
  # kruskal.test(PA_mmHg ~ Dosagem, data=Dados_Perturbacao, subset=Grupo=="SHR 17 Semanas Tratados")
  # https://stackoverflow.com/questions/3472980/how-to-change-facet-labels
  # http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software
  # (ns) não significativo; (*) p$<$0,05; (**) p$<$0,01; (***) p$<$0,001; (****) p$<$0,0001.

  g_pert_pa_grupo <- 
    ggboxplot(Dados_Perturbacao, x = "Dosagem", y = "PA_mmHg",
                               color = "Dosagem", palette = "lancet",
                               # ylim=c(25,200),
                               short.panel.labs=TRUE, ylab="Pressão Arterial (mmHg)",
                               ggtheme = theme_bw()) +
    ggtitle(paste("Comparativos de pressão arterial entre dosagens durante a ",as.character(perturbacao),"ª perturbação",sep=""))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(face='bold'),
          axis.title = element_text(size="12"),
          strip.text.x = element_text(face='bold'),
          legend.position = "bottom",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black")) +
    # stat_compare_means(label.y=230, label.x=1.5)+
    stat_compare_means(aes(label=paste0("(",..p.signif..,")")))+
    theme(plot.title = element_text(hjust = 0.5))+
    facet_wrap(~factor(Grupo,levels = c("SHR-17-NT",
                                        "SHR-17-T",
                                        "SHR-21-NT",
                                        "SHR-21-T",
                                        "WKY-21-NT",
                                        "WKY-21-T")))+
    facet_wrap(~ Grupo, labeller = label_wrap_gen(width=15), ncol=6)
  
  
  print(g_pert_pa_grupo)
  
  ggsave(paste("P",as.character(perturbacao),"_PA_Grupo.png",sep=""),
        plot=g_pert_pa_grupo,device="png",width=20,height=10,dpi=150,units="cm")
  
  Sys.sleep(3)
  
  
  
  g_pert_ptr_grupo <- ggboxplot(Dados_Perturbacao, x = "Dosagem", y = "Ptr_cmH2O",
                               color = "Dosagem", palette = "lancet",
                               # ylim=c(2.5,7),
                               short.panel.labs=TRUE, ylab=expression("Pressão Traqueal (cmH "[2]*"O)"),
                               ggtheme = theme_bw()) +
    ggtitle(paste("Comparativos de pressão traqueal entre dosagens durante a ",as.character(perturbacao),"ª perturbação",sep=""))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(face='bold'),
          axis.title = element_text(size="12"),
          strip.text.x = element_text(face='bold'),
          legend.position = "bottom",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black")) +
    # stat_compare_means(label.y=230, label.x=1.5)+
    stat_compare_means(method="kruskal",aes(label=paste0("(",..p.signif..,")")))+
    theme(plot.title = element_text(hjust = 0.5))+
    facet_wrap(~factor(Grupo,levels = c("SHR-17-NT",
                                        "SHR-17-T",
                                        "SHR-21-NT",
                                        "SHR-21-T",
                                        "WKY-21-NT",
                                        "WKY-21-T")))+
    facet_wrap(~ Grupo, labeller = label_wrap_gen(width=15), ncol=6)
  
  
  print(g_pert_ptr_grupo)
  
  ggsave(paste("P",as.character(perturbacao),"_Ptr_Grupo.png",sep=""),
         plot=g_pert_ptr_grupo,device="png",width=20,height=10,dpi=150,units="cm")
  
  Sys.sleep(3)
  
  
  
  
  g_pert_pa_dosagem <- ggboxplot(Dados_Perturbacao, x = "Grupo", y = "PA_mmHg",
                               color = "Grupo", palette = "lancet",
                               # ylim=c(25,200),
                               short.panel.labs=TRUE, ylab="Pressão Arterial (mmHg)",
                               ggtheme = theme_bw()) +
    ggtitle(paste("Comparativos de pressão arterial entre grupos durante a ",as.character(perturbacao),"ª perturbação",sep=""))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(face='bold'),
          axis.title = element_text(size="12"),
          strip.text.x = element_text(face='bold'),
          legend.position = "bottom",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black")) +
    # stat_compare_means(label.y=230, label.x=1.5)+
    stat_compare_means(aes(label=paste0("(",..p.signif..,")")))+
    theme(plot.title = element_text(hjust = 0.5))+
    facet_wrap(~factor(Dosagem,levels=c("PBS",
                                        "3 µg/kg",
                                        "10 µg/kg",
                                        "30 µg/kg",
                                        "100 µg/kg"))) +
    facet_wrap(~ Dosagem, labeller = label_wrap_gen(width=13),ncol=5)
  
  
  print(g_pert_pa_dosagem)
  
  ggsave(paste("P",as.character(perturbacao),"_PA_Dosagem.png",sep=""),
         plot=g_pert_pa_dosagem,device="png",width=20,height=10,dpi=150,units="cm")
  
  Sys.sleep(3)
  
  
  
  
  g_pert_ptr_dosagem <- ggboxplot(Dados_Perturbacao, x = "Grupo", y = "Ptr_cmH2O",
                                 color = "Grupo", palette = "lancet",
                                 # ylim=c(2.5,7),
                                 short.panel.labs=TRUE, ylab=expression("Pressão Traqueal (cmH "[2]*"O)"),
                                 ggtheme = theme_bw()) +
    ggtitle(paste("Comparativos de pressão traqueal entre grupos durante a ",as.character(perturbacao),"ª perturbação",sep=""))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(face='bold'),
          axis.title = element_text(size="12"),
          strip.text.x = element_text(face='bold'),
          legend.position = "bottom",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black")) +
    # stat_compare_means(label.y=230, label.x=1.5)+
    stat_compare_means(method="kruskal.test", aes(label=paste0("(",..p.signif..,")")))+
    theme(plot.title = element_text(hjust = 0.5))+
    facet_wrap(~factor(Dosagem,levels=c("PBS",
                                        "3 µg/kg",
                                        "10 µg/kg",
                                        "30 µg/kg",
                                        "100 µg/kg"))) +
    facet_wrap(~ Dosagem, labeller = label_wrap_gen(width=15),ncol=5)
  
  
  print(g_pert_ptr_dosagem)
  
  ggsave(paste("P",as.character(perturbacao),"_Ptr_Dosagem.png",sep=""),
         plot=g_pert_ptr_dosagem,device="png",width=20,height=10,dpi=150,units="cm")
  
  Sys.sleep(3)
  
}

#### fim ####

#### Plot PBS ####

Ptr_PBS_2 <- aggregate(Ptr_cmH2O ~ Animal * Grupo, PBS, mean)
PA_PBS_2 <- aggregate(PA_mmHg ~ Animal * Grupo, PBS, mean)

PA_PBS_2$Grupo = factor(PA_PBS_2$Grupo,
                     levels = c("SHR-17-NT",
                                "SHR-17-T",
                                "SHR-21-NT",
                                "SHR-21-T",
                                "WKY-21-NT",
                                "WKY-21-T"))

# write.csv2(PA_PBS_2, "PA_PBS_2.csv",fileEncoding = "UTF-8")



PHT_PA_PBS <- dunn_test(PA_mmHg ~ Grupo,data=PA_PBS_2)

Ptr_ANOVA <- aov(Ptr_cmH2O ~ Grupo, Ptr_PBS_2)
summary(Ptr_ANOVA)
PostHocTest(Ptr_ANOVA, method="bonf", conf.level = 0.95)

g_pbs_ptr <- ggboxplot(Ptr_PBS_2, x = "Grupo", y = "Ptr_cmH2O",
          color = "Grupo", palette = "lancet",
          ylim=c(2.5,5.5),
          # title="Comparativo de pressão traqueal entre grupos durante etapa PBS",
          ylab=expression("Pressão Traqueal (cmH "[2]*"O)"),
          ggtheme = theme_bw()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(face='bold'),
        axis.title = element_text(size="12"),
        legend.position = "bottom",
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black")) +
  stat_compare_means(label.y=5,aes(label=paste0("(",..p.signif..,")")),method = "anova")+
  theme(plot.title = element_text(hjust = 0.5))+  geom_signif(comparisons = list(c("SHR-21-NT",
                                                                                   "WKY-21-T")
  ),
  y_position = c(4.5),
  annotations = c("*"))

print(g_pbs_ptr)

ggsave("Ptr_PBS.png",
       plot=g_pbs_ptr,device="png",width=20,height=10,dpi=150,units="cm")

g_pbs_pa <- ggboxplot(PA_PBS_2, x = "Grupo", y = "PA_mmHg",
          color = "Grupo", palette = "lancet",
          ylim=c(25,200),
          # title="Comparativo de pressão arterial entre grupos durante etapa PBS",
          ylab="Pressão Arterial (mmHg)",
          ggtheme = theme_bw()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(face='bold'),
        axis.title = element_text(size="12"),
        legend.position = "bottom",
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black")) +
  stat_compare_means(label.y=180,aes(label=paste0("(",..p.signif..,")")),method = "kruskal")+
  theme(plot.title = element_text(hjust = 0.5))

print(g_pbs_pa)

ggsave("PA_PBS.png",
       plot=g_pbs_pa,device="png",width=20,height=10,dpi=150,units="cm")

#### fim ####

#### Direta x Indireta ####

Direta_Indireta <- read.csv2("Direta_Indireta.csv")

Direta_Indireta <- transform(Direta_Indireta,
                             Grupo = as.factor(Grupo),
                             Animal = as.factor(Animal),
                             PA_Pre_mmHg = as.double(PA_Pre_mmHg),
                             PA_Pos_mmHg = as.double(PA_Pos_mmHg),
                             Medicao = as.factor(Medicao))

Direta_Indireta$Grupo = factor(Direta_Indireta$Grupo,
                               levels = c("SHR-17-NT",
                                          "SHR-17-T",
                                          "SHR-21-NT",
                                          "SHR-21-T",
                                          "WKY-21-NT",
                                          "WKY-21-T"))

colnames(Direta_Indireta) <- c("Animal","Grupo","PA_Pre_mmHg","PA_Pos_mmHg","Medição")

g_direta_indireta <- ggboxplot(Direta_Indireta, x = "Medição", y = "PA_Pos_mmHg",
          color = "Medição", palette = "lancet",
          ggtheme = theme_bw(base_size = 11),size=0.6,
          order=c("Indireta","Direta"),
          facet.by = "Grupo", 
          short.panel.labs=TRUE, ylab='Pressão Arterial (mmHg)') +
  ggtitle(paste("Comparativo PA medição direta x medição indireta"))+
  stat_compare_means(aes(label=paste0("(",..p.signif..,")")))+
  # stat_summary(fun=mean,geom="text",aes(label=Dosagem))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(face='bold'),
        axis.title = element_text(size="12"),
        strip.text.x = element_text(face='bold'),
        legend.position = "bottom",
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black"))+
  # labs(color="Medicao")+
  # scale_x_discrete(limits=c(1,15))+
  # facet_grid(~comp_texto, labeller=labeller(comp_texto=comp.labs))
  facet_wrap(~Grupo, ncol=6)

g_direta_indireta

ggsave("Direta_Indireta.png",
       plot=g_direta_indireta,device="png",width=20,height=10,dpi=150,units="cm")


Pre_Pos <- read.csv2("Pre_Pos.csv")

Pre_Pos <- data.frame(lapply(Pre_Pos, function(x){gsub("Pre","Pré-tratamento",x)}))

Pre_Pos <- data.frame(lapply(Pre_Pos, function(x){gsub("Pos","Pós-tratamento",x)}))

Pre_Pos <- transform(Pre_Pos,
                             Grupo = as.factor(Grupo),
                             Animal = as.factor(Animal),
                             PA_mmHg = as.double(PA_mmHg),
                             Etapa = as.factor(Etapa))

Pre_Pos$Grupo = factor(Pre_Pos$Grupo,
                               levels = c("SHR-17-NT",
                                          "SHR-17-T",
                                          "SHR-21-NT",
                                          "SHR-21-T",
                                          "WKY-21-NT",
                                          "WKY-21-T"))

g_pre_pos <- ggboxplot(Pre_Pos, x = "Etapa", y = "PA_mmHg",
                             color = "Etapa", palette = "lancet",
                             ggtheme = theme_bw(base_size = 11),size=0.6,
                             order=c("Pré-tratamento","Pós-tratamento"),
                             facet.by = "Grupo", 
                             short.panel.labs=TRUE, ylab='Pressão Arterial Indireta (mmHg)') +
  ggtitle(paste("Comparativo PA pré-tratamento x pós-tratamento com hidralazina"))+
  stat_compare_means(aes(label=paste0("(",..p.signif..,")")))+
  # stat_summary(fun=mean,geom="text",aes(label=Dosagem))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(face='bold'),
        axis.title = element_text(size="12"),
        strip.text.x = element_text(face='bold'),
        legend.position = "bottom",
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black"))+
  # labs(color="Medicao")+
  # scale_x_discrete(limits=c(1,15))+
  # facet_grid(~comp_texto, labeller=labeller(comp_texto=comp.labs))
  facet_wrap(~Grupo, ncol=6)

g_pre_pos

ggsave("Pre_Pos.png",
       plot=g_pre_pos,device="png",width=20,height=10,dpi=150,units="cm")

#### fim ####

#### Plots P2/P3 ####

# Criando novo data frame

df_perturbacoes_2_3 <- Dados[0,] # cria DF vazio porém com as colunas do DF origem


comparacao_2_2 <- Dados[(Dados$Perturbacao==2 &
                            Dados$Dosagem == "3 µg/kg"),]

comparacao_2_3 <- Dados[(Dados$Perturbacao==3 & 
                           Dados$Dosagem == "3 µg/kg"),]

comparacao_2 <- rbind(comparacao_2_2, comparacao_2_3)


comp_texto <- rep("comp_2", nrow(comparacao_2))
df_aux <- cbind(comparacao_2, comp_texto)
df_perturbacoes_2_3 <- rbind(df_perturbacoes_2_3, df_aux)

#

comparacao_3_2 <- Dados[(Dados$Perturbacao==2 &
                            Dados$Dosagem == "10 µg/kg"),]


comparacao_3_3 <- Dados[(Dados$Perturbacao==3 & 
                           Dados$Dosagem == "10 µg/kg"),]

comparacao_3 <- rbind(comparacao_3_2, comparacao_3_3)

comp_texto <- rep("comp_3", nrow(comparacao_3))
df_aux <- cbind(comparacao_3, comp_texto)
df_perturbacoes_2_3 <- rbind(df_perturbacoes_2_3, df_aux)

#

comparacao_4_2 <- Dados[(Dados$Perturbacao==2 &
                            Dados$Dosagem == "30 µg/kg"),]


comparacao_4_3 <- Dados[(Dados$Perturbacao==3 & 
                           Dados$Dosagem == "30 µg/kg"),]

comparacao_4 <- rbind(comparacao_4_2, comparacao_4_3)

comp_texto <- rep("comp_4", nrow(comparacao_4))
df_aux <- cbind(comparacao_4, comp_texto)
df_perturbacoes_2_3 <- rbind(df_perturbacoes_2_3, df_aux)

#

comparacao_5_2 <- Dados[(Dados$Perturbacao==2 &
                            Dados$Dosagem == "100 µg/kg"),]


comparacao_5_3 <- Dados[(Dados$Perturbacao==3 & 
                           Dados$Dosagem == "100 µg/kg"),]

comparacao_5 <- rbind(comparacao_5_2, comparacao_5_3)

comp_texto <- rep("comp_5", nrow(comparacao_5))
df_aux <- cbind(comparacao_5, comp_texto)
df_perturbacoes_2_3 <- rbind(df_perturbacoes_2_3, df_aux)

#

grupo <- "SHR-17-T"

Lista_Grupos = unique(Dados$Grupo)

comp.labs <- c("3 µg/kg","10 µg/kg","30 µg/kg","100 µg/kg")
names(comp.labs) <- c("comp_2","comp_3","comp_4","comp_5")

for (grupo in Lista_Grupos){
  
  df_perturbacoes_2_3_grupo <- 
    df_perturbacoes_2_3[df_perturbacoes_2_3$Grupo == grupo,]
  
  
  g_p2_p3_pa <- ggboxplot(df_perturbacoes_2_3_grupo, x = "Perturbacao", y = "PA_mmHg",
                           color = "Perturbacao", palette = "lancet",
                           ggtheme = theme_bw(base_size = 11),size=0.6,
                           order=c(2,3),
                           facet.by = "comp_texto", 
                           short.panel.labs=TRUE, ylab='Pressão Arterial (mmHg)') +
    ggtitle(paste("Comparativo entre perturbações do grupo ",grupo," (PA)",sep=""))+
    stat_compare_means(method="wilcox.test",aes(label=paste0("(",..p.signif..,")")))+
    # stat_summary(fun=mean,geom="text",aes(label=Dosagem))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(face='bold'),
          axis.title = element_text(size="12"),
          strip.text.x = element_text(face='bold'),
          legend.position = "bottom",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black"))+
    labs(color="Perturbação")+
    # scale_x_discrete(limits=c(1,15))+
    # facet_grid(~comp_texto, labeller=labeller(comp_texto=comp.labs))
    facet_wrap(~comp_texto, labeller=labeller(label_wrap_gen(width=13),
                                              comp_texto=comp.labs),ncol=6)
  
  print(g_p2_p3_pa)
  
  
  ggsave(paste("P2_P3_",grupo,"_PA.png",sep=""),
         plot=g_p2_p3_pa,device="png",width=20,height=10,dpi=150,units="cm")
  
  Sys.sleep(3)
  
  g_p2_p3_ptr <- ggboxplot(df_perturbacoes_2_3_grupo, x = "Perturbacao", y = "Ptr_cmH2O",
                            color = "Perturbacao", palette = "lancet",
                            ggtheme = theme_bw(base_size = 11),size=0.6,
                            order=c(2,3),
                            facet.by = "Dosagem", 
                            short.panel.labs=TRUE, ylab=expression("Pressão Traqueal (cmH "[2]*"O)")) +
    ggtitle(paste("Comparativo entre perturbações do grupo ",grupo," (Ptr)",sep=""))+
    stat_compare_means(method="wilcox.test",aes(label=paste0("(",..p.signif..,")")))+
    # stat_summary(fun=mean,geom="text",aes(label=Dosagem))+#,vjust=6)+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(face='bold'),
          axis.title = element_text(size="12"),
          strip.text.x = element_text(face='bold'),
          legend.position = "bottom",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black"))+
    labs(color="Perturbação")+
    facet_wrap(~comp_texto, labeller=labeller(label_wrap_gen(width=13),
                                              comp_texto=comp.labs),ncol=6)
  
  print(g_p2_p3_ptr)
  
  ggsave(paste("P2_P3_",grupo,"_Ptr.png",sep=""),
         plot=g_p2_p3_ptr,device="png",width=20,height=10,dpi=150,units="cm")
  
  Sys.sleep(3)
  
}

#### fim ####



#### Plots P2/P4 ####

# Criando novo data frame

df_perturbacoes_2_4 <- Dados[0,] # cria DF vazio porém com as colunas do DF origem


comparacao_2_2 <- Dados[(Dados$Perturbacao==2 &
                           Dados$Dosagem == "3 µg/kg"),]

comparacao_2_4 <- Dados[(Dados$Perturbacao==4 & 
                           Dados$Dosagem == "3 µg/kg"),]

comparacao_2 <- rbind(comparacao_2_2, comparacao_2_4)


comp_texto <- rep("comp_2", nrow(comparacao_2))
df_aux <- cbind(comparacao_2, comp_texto)
df_perturbacoes_2_4 <- rbind(df_perturbacoes_2_4, df_aux)

#

comparacao_3_2 <- Dados[(Dados$Perturbacao==2 &
                           Dados$Dosagem == "10 µg/kg"),]


comparacao_3_4 <- Dados[(Dados$Perturbacao==4 & 
                           Dados$Dosagem == "10 µg/kg"),]

comparacao_3 <- rbind(comparacao_3_2, comparacao_3_4)

comp_texto <- rep("comp_3", nrow(comparacao_3))
df_aux <- cbind(comparacao_3, comp_texto)
df_perturbacoes_2_4 <- rbind(df_perturbacoes_2_4, df_aux)

#

comparacao_4_2 <- Dados[(Dados$Perturbacao==2 &
                           Dados$Dosagem == "30 µg/kg"),]


comparacao_4_4 <- Dados[(Dados$Perturbacao==4 & 
                           Dados$Dosagem == "30 µg/kg"),]

comparacao_4 <- rbind(comparacao_4_2, comparacao_4_4)

comp_texto <- rep("comp_4", nrow(comparacao_4))
df_aux <- cbind(comparacao_4, comp_texto)
df_perturbacoes_2_4 <- rbind(df_perturbacoes_2_4, df_aux)

#

comparacao_5_2 <- Dados[(Dados$Perturbacao==2 &
                           Dados$Dosagem == "100 µg/kg"),]


comparacao_5_4 <- Dados[(Dados$Perturbacao==4 & 
                           Dados$Dosagem == "100 µg/kg"),]

comparacao_5 <- rbind(comparacao_5_2, comparacao_5_4)

comp_texto <- rep("comp_5", nrow(comparacao_5))
df_aux <- cbind(comparacao_5, comp_texto)
df_perturbacoes_2_4 <- rbind(df_perturbacoes_2_4, df_aux)

#

grupo <- "SHR-17-T"

Lista_Grupos = unique(Dados$Grupo)

comp.labs <- c("3 µg/kg","10 µg/kg","30 µg/kg","100 µg/kg")
names(comp.labs) <- c("comp_2","comp_3","comp_4","comp_5")

for (grupo in Lista_Grupos){
  
  df_perturbacoes_2_4_grupo <- 
    df_perturbacoes_2_4[df_perturbacoes_2_4$Grupo == grupo,]
  
  
  g_p2_p4_pa <- ggboxplot(df_perturbacoes_2_4_grupo, x = "Perturbacao", y = "PA_mmHg",
                          color = "Perturbacao", palette = "lancet",
                          ggtheme = theme_bw(base_size = 11),size=0.6,
                          order=c(2,4),
                          facet.by = "comp_texto", 
                          short.panel.labs=TRUE, ylab='Pressão Arterial (mmHg)') +
    ggtitle(paste("Comparativo entre perturbações do grupo ",grupo," (PA)",sep=""))+
    stat_compare_means(method="wilcox.test",aes(label=paste0("(",..p.signif..,")")))+
    # stat_summary(fun=mean,geom="text",aes(label=Dosagem))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(face='bold'),
          axis.title = element_text(size="12"),
          strip.text.x = element_text(face='bold'),
          legend.position = "bottom",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black"))+
    labs(color="Perturbação")+
    # scale_x_discrete(limits=c(1,15))+
    # facet_grid(~comp_texto, labeller=labeller(comp_texto=comp.labs))
    facet_wrap(~comp_texto, labeller=labeller(label_wrap_gen(width=13),
                                              comp_texto=comp.labs),ncol=6)
  
  print(g_p2_p4_pa)
  
  
  ggsave(paste("P2_P4_",grupo,"_PA.png",sep=""),
         plot=g_p2_p4_pa,device="png",width=20,height=10,dpi=150,units="cm")
  
  Sys.sleep(3)
  
  g_p2_p4_ptr <- ggboxplot(df_perturbacoes_2_4_grupo, x = "Perturbacao", y = "Ptr_cmH2O",
                           color = "Perturbacao", palette = "lancet",
                           ggtheme = theme_bw(base_size = 11),size=0.6,
                           order=c(2,4),
                           facet.by = "Dosagem", 
                           short.panel.labs=TRUE, ylab=expression("Pressão Traqueal (cmH "[2]*"O)")) +
    ggtitle(paste("Comparativo entre perturbações do grupo ",grupo," (Ptr)",sep=""))+
    stat_compare_means(method="wilcox.test",aes(label=paste0("(",..p.signif..,")")))+
    # stat_summary(fun=mean,geom="text",aes(label=Dosagem))+#,vjust=6)+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(face='bold'),
          axis.title = element_text(size="12"),
          strip.text.x = element_text(face='bold'),
          legend.position = "bottom",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black"))+
    labs(color="Perturbação")+
    facet_wrap(~comp_texto, labeller=labeller(label_wrap_gen(width=13),
                                              comp_texto=comp.labs),ncol=6)
  
  print(g_p2_p4_ptr)
  
  ggsave(paste("P2_P4_",grupo,"_Ptr.png",sep=""),
         plot=g_p2_p4_ptr,device="png",width=20,height=10,dpi=150,units="cm")
  
  Sys.sleep(3)
  
}

#### fim ####

# P1

cors_p1 <- ddply(P1, c("Dosagem", "Grupo"), summarise, 
              cor = round(cor(Ptr_cmH2O, PA_mmHg), 2))

g_p1_cor <- ggplot(data=P1, mapping = aes(x=Ptr_cmH2O,y=PA_mmHg), title="P1")+
  geom_point()+ facet_grid(Dosagem~Grupo)+ ggtitle("Correlação 1ª Perturbação")+
  geom_text(data=cors_p1, aes(label=paste("r=", cor, sep="")), x=4, y=150)

g_p1_cor

cors_p1_g <- ddply(P1, c("Grupo"), summarise, 
                 cor = round(cor(Ptr_cmH2O, PA_mmHg), 2))

g_p1_cor_g <- ggplot(data=P1, mapping = aes(x=Ptr_cmH2O,y=PA_mmHg), title="P1")+
  geom_point()+ facet_grid(.~Grupo)+ ggtitle("Correlação 1ª Perturbação (Grupos)")+
  geom_text(data=cors_p1_g, aes(label=paste("r=", cor, sep="")), x=4, y=150)

g_p1_cor_g


# P2

cors_p2 <- ddply(P2, c("Dosagem", "Grupo"), summarise, 
                 cor = round(cor(Ptr_cmH2O, PA_mmHg), 2))

g_p2_cor <- ggplot(data=P2, mapping = aes(x=Ptr_cmH2O,y=PA_mmHg), title="P2")+
  geom_point()+ facet_grid(Dosagem~Grupo)+ ggtitle("Correlação 2ª Perturbação")+
  geom_text(data=cors_p2, aes(label=paste("r=", cor, sep="")), x=4, y=150)

g_p2_cor

cors_p2_g <- ddply(P2, c("Grupo"), summarise, 
                   cor = round(cor(Ptr_cmH2O, PA_mmHg), 2))

g_p2_cor_g <- ggplot(data=P2, mapping = aes(x=Ptr_cmH2O,y=PA_mmHg), title="P2")+
  geom_point()+ facet_grid(.~Grupo)+ ggtitle("Correlação 2ª Perturbação (Grupos)")+
  geom_text(data=cors_p2_g, aes(label=paste("r=", cor, sep="")), x=4, y=150)

g_p2_cor_g

# P3

cors_p3 <- ddply(P3, c("Dosagem", "Grupo"), summarise, 
                 cor = round(cor(Ptr_cmH2O, PA_mmHg), 2))

g_p3_cor <- ggplot(data=P3, mapping = aes(x=Ptr_cmH2O,y=PA_mmHg), title="P3")+
  geom_point()+ facet_grid(Dosagem~Grupo)+ ggtitle("Correlação 3ª Perturbação")+
  geom_text(data=cors_p3, aes(label=paste("r=", cor, sep="")), x=4, y=150)

g_p3_cor

cors_p3_g <- ddply(P3, c("Grupo"), summarise, 
                   cor = round(cor(Ptr_cmH2O, PA_mmHg), 2))

g_p3_cor_g <- ggplot(data=P3, mapping = aes(x=Ptr_cmH2O,y=PA_mmHg), title="3")+
  geom_point()+ facet_grid(.~Grupo)+ ggtitle("Correlação 3ª Perturbação (Grupos)")+
  geom_text(data=cors_p3_g, aes(label=paste("r=", cor, sep="")), x=4, y=150)

g_p3_cor_g


P1_PBS <- P1[(P1$Dosagem == "PBS"),]
P2_PBS <- P2[(P2$Dosagem == "PBS"),]
P3_PBS <- P3[(P3$Dosagem == "PBS"),]

P1_3u <- P1[(P1$Dosagem == "3 µg/kg"),]
P2_3u <- P2[(P2$Dosagem == "3 µg/kg"),]
P3_3u <- P3[(P3$Dosagem == "3 µg/kg"),]

P1_10u <- P1[(P1$Dosagem == "10 µg/kg"),]
P2_10u <- P2[(P2$Dosagem == "10 µg/kg"),]
P3_10u <- P3[(P3$Dosagem == "10 µg/kg"),]

P1_30u <- P1[(P1$Dosagem == "30 µg/kg"),]
P2_30u <- P2[(P2$Dosagem == "30 µg/kg"),]
P3_30u <- P3[(P3$Dosagem == "30 µg/kg"),]

P1_100u <- P1[(P1$Dosagem == "100 µg/kg"),]
P2_100u <- P2[(P2$Dosagem == "100 µg/kg"),]
P3_100u <- P3[(P3$Dosagem == "100 µg/kg"),]

pairwise.wilcox.test(P1_PBS$PA_mmHg, P1_PBS$Grupo, p.adjust.method="none")
pairwise.wilcox.test(P2_PBS$PA_mmHg, P2_PBS$Grupo, p.adjust.method="bonf")
pairwise.wilcox.test(P3_PBS$PA_mmHg, P3_PBS$Grupo, p.adjust.method="bonf")

pairwise.wilcox.test(P1_3u$PA_mmHg, P1_3u$Grupo, p.adjust.method="bonf")
pairwise.wilcox.test(P2_3u$PA_mmHg, P2_3u$Grupo, p.adjust.method="bonf")
pairwise.wilcox.test(P3_3u$PA_mmHg, P3_3u$Grupo, p.adjust.method="bonf")

pairwise.wilcox.test(P1_10u$PA_mmHg, P1_10u$Grupo, p.adjust.method="bonf")
pairwise.wilcox.test(P2_10u$PA_mmHg, P2_10u$Grupo, p.adjust.method="bonf")
pairwise.wilcox.test(P3_10u$PA_mmHg, P3_10u$Grupo, p.adjust.method="bonf")

pairwise.wilcox.test(P1_30u$PA_mmHg, P1_30u$Grupo, p.adjust.method="bonf")
pairwise.wilcox.test(P2_30u$PA_mmHg, P2_30u$Grupo, p.adjust.method="bonf")
pairwise.wilcox.test(P3_30u$PA_mmHg, P3_30u$Grupo, p.adjust.method="bonf")

pairwise.wilcox.test(P1_100u$PA_mmHg, P1_100u$Grupo, p.adjust.method="bonf")
pairwise.wilcox.test(P2_100u$PA_mmHg, P2_100u$Grupo, p.adjust.method="bonf")
pairwise.wilcox.test(P3_100u$PA_mmHg, P3_100u$Grupo, p.adjust.method="bonf")
