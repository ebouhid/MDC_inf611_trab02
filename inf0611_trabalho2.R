#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao       
#                       
# Trabalho Avaliativo 2
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# -                                        
# -                                        
# -                                        
# 
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares 
#----------------------------------------------------------------#
# configure o caminho antes de executar
# setwd("") 
options(warn=-1)
source("./ranking_metrics.R")
source("./trabalho2_base.R")

# caminho da pasta de imagens
path_plantas = './plantas'

#----------------------------------------------------------------#
# Leitura das imagens 
#----------------------------------------------------------------#
imagens <- <to-do>

#----------------------------------------------------------------#
# Obtem classe de cada imagem 
#----------------------------------------------------------------#
nome_classes <- <to-do>



#----------------------------------------------------------------#
# obtem ground_truth para cada classe 
#----------------------------------------------------------------#
ground_truth_biloba <- <to-do>
ground_truth_europaea <- <to-do>
ground_truth_ilex <- <to-do>
ground_truth_monogyna <- <to-do>
ground_truth_regia <- <to-do>


#----------------------------------------------------------------#
# Questao 1 
#----------------------------------------------------------------#

# obtem caracteristicas de cor  
hist_cor_desc <- function(img){
  <to-do>
}

# obtem caracteristicas de textura   
lbp_desc <- function(img){
  <to-do>
}


# obtem caracteristicas de forma 
Momentos <-function(img){
  
  centroide <- function(M) {
    c(momento(M, 1, 0) / momento(M, 0, 0),
      momento(M, 0, 1) / momento(M, 0, 0))
  }
  
  momento <- function(M, p, q, central = FALSE) {
    r <- 0
    if (central) {
      c <- centroide(M)
      x <- c[1]
      y <- c[2]
    } else {
      x <- 0
      y <- 0
    }
    for (i in 1:nrow(M))
      for (j in 1:ncol(M))
        r <- r + (i - x)^p * (j - y)^q * M[i,j]  
    return(r)
  }
  
  <to-do>
}


#----------------------------------------------------------------#
# obtem características de cor, textura e forma  
# para todas as imagens e armazena em matrizes 
# onde uma linha e uma imagem 
features_c <- <to-do>
rownames(features_c) <- names(imagens)
features_t <- <to-do>
rownames(features_t) <- names(imagens)
features_s <- <to-do>
rownames(features_s) <- names(imagens)



#----------------------------------------------------------------#
# Questao 2                               
#----------------------------------------------------------------#

# definindo as consultas
# obs.:  use o caminho completo para a imagem
consulta_biloba <- "./plantas/biloba_02.jpg"
consulta_europaea <- "./plantas/europaea_01.jpg"
consulta_ilex <- "./plantas/ilex_08.jpg"
consulta_monogyna <- "./plantas/monogyna_04.jpg"
consulta_regia <- "./plantas/regia_07.jpg"

# visualizando as consultas
par(mfrow = c(3,3), mar = rep(2, 4))
mostrarImagemColorida(<to-do>)
<to-do>
  
#-----------------------------#
# construindo rankings                          
# para cada uma das 5 consultas, construa um ranking com base na cor
ranking_c_biloba <- <to-do>
ranking_c_europaea <- <to-do>
ranking_c_ilex <- <to-do>
ranking_c_monogyna <- <to-do>
ranking_c_regia <- <to-do>

# para cada uma das 5 consultas, construa um ranking com base na textura
ranking_t_biloba <- <to-do>
ranking_t_europaea <- <to-do>
ranking_t_ilex <- <to-do>
ranking_t_monogyna <- <to-do>
ranking_t_regia <- <to-do>
  
# para cada uma das 5 consultas, construa um ranking com base na forma
ranking_s_biloba <- <to-do>
ranking_s_europaea <- <to-do>
ranking_s_ilex <- <to-do>
ranking_s_monogyna <- <to-do>
ranking_s_regia <- <to-do>

#-----------------------------#
# comparando  rankings                              

## utilize as funções do arquivo ranking_metrics.R para calcular 
# a precisão, revocação, taxa F1 e precisão média nos 
# top 5, 10, 15 e 20
  
analyse_rankings <- function(ranking, ground_truth) {
  <to-do>
}

# analisando rankings gerados com caracteristicas de cor
analyse_rankings(<to-do>)

# analisando rankings gerados com caracteristicas de textura
analyse_rankings(<to-do>)

# analisando rankings gerados com caracteristicas de forma
analyse_rankings(<to-do>)

#----------------------------------------------------------------#
# Questao 2 - RESPONDA:                   
# (e) 
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
# (f)  
#                                         
#                                         
#                                         
#                                         
#                                         
# (g) dica: use a função generate_df_11_points para gerar o 
# data.frame interpolado para cada descritor. Depois, use a função
# plot_precision_x_recall_11_points_t2 para gerar o gráfico. 
# Exemplo de chamada da função generate_df_11_points:
# df_c_11_points <- generate_df_11_points(list(list(ground_truth_biloba, ranking_c_biloba), list(ground_truth_europaea, ranking_c_europaea)))
# Exemplo de chamada da função plot_precision_x_recall_11_points_t2:
# plot_precision_x_recall_11_points_t2(rbind(df_c_11_points, df_t_11_points, df_s_11_points), c("Cor", "Textura", "Forma"), "Titulo do Gráfico") 
#                                         
#                                         
#                                         
#----------------------------------------------------------------#



#----------------------------------------------------------------#
# Questao 3
#----------------------------------------------------------------#
# concatenando caracteristicas                      

## obter vetores finais de caracteristicas pela concatenação de 
# cada tipo de caracteristica (cor, textura e forma):
features_concat <- <to-do>
  
# gerar novos rankings
ranking_concat_biloba <- <to-do>
ranking_concat_europaea <- <to-do>
ranking_concat_ilex <- <to-do>
ranking_concat_monogyna <- <to-do>
ranking_concat_regia <- <to-do>
  
# analisando rankings gerados com caracteristicas concatenadas
analyse_rankings(<to-do>)


#----------------------------------------------------------------#
# Questao 3 - RESPONDA:  
# (d) 
# 
# 
# (e) 
# 
# 
# 
# 
# (f)
# 
# 
# 
#----------------------------------------------------------------#




#----------------------------------------------------------------#
# Questao 4
#----------------------------------------------------------------#

# calculando as distancias, descritor:  histograma de cor 
dist_hist_<to-select> <- get_distance_vector(<to-do>, <to-do>) 
dist_hist_<to-select> <- <to-do>
  
# calculando as distancias, descritor:  textura 
dist_text_<to-select> <- get_distance_vector(<to-do>, <to-do>) 
dist_text_<to-select> <- <to-do> 
  
# calculando as distancias, descritor:  forma 
dist_forma_<to-select> <- get_distance_vector(<to-do>, <to-do>) 
dist_forma_<to-select> <- <to-do> 
  
# calculando e analisando  rankings combmin
r_combmin_<to-select> <- names(imagens)[combmin(<to-do>, <to-do>, <to-do>)]
r_combmin_<to-select> <- <to-do>

# calculando e analisando  rankings combmax
<to-do>
  
# calculando e analisando  rankings combsum
<to-do>
  
# calculando e analisando  rankings borda
<to-do>

  
analyse_rankings(<to-do>)
analyse_rankings(<to-do>)
  
#----------------------------------------------------------------#
# Questao 4 - RESPONDA:                   
# (i) 
# 
# 
# 
# (j)
# 
# 
# 
# (k)
# 
# 
#
# (l)
# 
#
#
#----------------------------------------------------------------#
