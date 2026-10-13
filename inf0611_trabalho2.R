#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao       
#                       
# Trabalho Avaliativo 2
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# - Anderson Luis Bento Soares
# - Eduardo Bouhid Neto                      
# - Mateus Coelho                         
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
imagens <- read_images("plantas/")

#----------------------------------------------------------------#
# Obtem classe de cada imagem 
#----------------------------------------------------------------#
nome_classes <- get_classes("plantas/")

#----------------------------------------------------------------#
# obtem ground_truth para cada classe 
#----------------------------------------------------------------#
ground_truth_biloba <- get_ground_truth("plantas/", nome_classes, "biloba")
ground_truth_europaea <- get_ground_truth("plantas/", nome_classes, "europaea")
ground_truth_ilex <- get_ground_truth("plantas/", nome_classes, "ilex")
ground_truth_monogyna <- get_ground_truth("plantas/", nome_classes, "monogyna")
ground_truth_regia <- get_ground_truth("plantas/", nome_classes, "regia")


#----------------------------------------------------------------#
# Questao 1 
#----------------------------------------------------------------#

# obtem caracteristicas de cor  
hist_cor_desc <- function(img){
  r <- hist(img[,,1]*255, plot=FALSE, breaks=0:255)$counts
  g <- hist(img[,,2]*255, plot=FALSE, breaks=0:255)$counts
  b <- hist(img[,,3]*255, plot=FALSE, breaks=0:255)$counts
  return(c(r, g, b))
}

# obtem caracteristicas de textura   
lbp_desc <- function(img){
  r1 <- lbp(grayscale(img)[,,1,1],1)
  lbp_uniforme <- hist(r1$lbp.u2, plot=FALSE, breaks=59)$counts
  return(c(lbp_uniforme))
}


# obtem caracteristicas de forma 
Momentos <-function(img){
  img <- as.matrix(grayscale(img))
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
  features <- NULL
  for(i in 0:2){
    for(j in 0:2){
      #adiciona um novo momento como caracteristica no vetor de caracteristicas da imagem
      features <- cbind(features,momento(img, i,j, central=TRUE))
    }
  }
  return(features)
}


#----------------------------------------------------------------#
# obtem características de cor, textura e forma  
# para todas as imagens e armazena em matrizes 
# onde uma linha e uma imagem 
features_c <- do.call(rbind, lapply(imagens, hist_cor_desc))
rownames(features_c) <- names(imagens)
features_t <- do.call(rbind, lapply(imagens, lbp_desc))
rownames(features_t) <- names(imagens)
features_s <- do.call(rbind, lapply(imagens, Momentos))
rownames(features_s) <- names(imagens)



#----------------------------------------------------------------#
# Questao 2                               
#----------------------------------------------------------------#

# definindo as consultas
# obs.:  use o caminho completo para a imagem
consulta_biloba <- "plantas//biloba_02.jpg"
consulta_europaea <- "plantas//europaea_01.jpg"
consulta_ilex <- "plantas//ilex_08.jpg"
consulta_monogyna <- "plantas//monogyna_04.jpg"
consulta_regia <- "plantas//regia_07.jpg"

# visualizando as consultas
par(mfrow = c(3,3), mar = rep(2, 4))
mostrarImagemColorida(consulta_biloba)
mostrarImagemColorida(consulta_europaea)
mostrarImagemColorida(consulta_ilex)
mostrarImagemColorida(consulta_monogyna)
mostrarImagemColorida(consulta_regia)

#-----------------------------#
# construindo rankings                          
# para cada uma das 5 consultas, construa um ranking com base na cor
ranking_c_biloba <- get_ranking_by_distance(features_c, consulta_biloba, method = "euclidean")
ranking_c_europaea <- get_ranking_by_distance(features_c, consulta_europaea, method = "euclidean")
ranking_c_ilex <- get_ranking_by_distance(features_c, consulta_ilex, method = "euclidean")
ranking_c_monogyna <- get_ranking_by_distance(features_c, consulta_monogyna, method = "euclidean")
ranking_c_regia <- get_ranking_by_distance(features_c, consulta_regia, method = "euclidean")

# para cada uma das 5 consultas, construa um ranking com base na textura
ranking_t_biloba <- get_ranking_by_distance(features_t, consulta_biloba, method = "euclidean")
ranking_t_europaea <- get_ranking_by_distance(features_t, consulta_europaea, method = "euclidean")
ranking_t_ilex <- get_ranking_by_distance(features_t, consulta_ilex, method = "euclidean")
ranking_t_monogyna <- get_ranking_by_distance(features_t, consulta_monogyna, method = "euclidean")
ranking_t_regia <- get_ranking_by_distance(features_t, consulta_regia, method = "euclidean")
  
# para cada uma das 5 consultas, construa um ranking com base na forma
ranking_s_biloba <- get_ranking_by_distance(features_s, consulta_biloba, method = "euclidean")
ranking_s_europaea <- get_ranking_by_distance(features_s, consulta_europaea, method = "euclidean")
ranking_s_ilex <- get_ranking_by_distance(features_s, consulta_ilex, method = "euclidean")
ranking_s_monogyna <- get_ranking_by_distance(features_s, consulta_monogyna, method = "euclidean")
ranking_s_regia <- get_ranking_by_distance(features_s, consulta_regia, method = "euclidean")

#-----------------------------#
# comparando  rankings                              

## utilize as funções do arquivo ranking_metrics.R para calcular 
# a precisão, revocação, taxa F1 e precisão média nos 
# top 5, 10, 15 e 20
  
analyse_rankings <- function(ranking, ground_truth) {
  ks <- c(5, 10, 15, 20)
  data.frame(
    k = ks,
    precisao = sapply(ks, function(k) precision(ground_truth, ranking, k)),
    revocacao = sapply(ks, function(k) recall(ground_truth, ranking, k)),
    f1 = sapply(ks, function(k) f1_score(ground_truth, ranking, k)),
    ap = sapply(ks, function(k) ap(ground_truth, ranking, k))
  )
}

# analisando rankings gerados com caracteristicas de cor
analyse_rankings(ranking_c_biloba, ground_truth_biloba)
analyse_rankings(ranking_c_europaea, ground_truth_europaea)
analyse_rankings(ranking_c_ilex, ground_truth_ilex)
analyse_rankings(ranking_c_monogyna, ground_truth_monogyna)
analyse_rankings(ranking_c_regia, ground_truth_regia)

# analisando rankings gerados com caracteristicas de textura
analyse_rankings(ranking_t_biloba, ground_truth_biloba)
analyse_rankings(ranking_t_europaea, ground_truth_europaea)
analyse_rankings(ranking_t_ilex, ground_truth_ilex)
analyse_rankings(ranking_t_monogyna, ground_truth_monogyna)
analyse_rankings(ranking_t_regia, ground_truth_regia)

# analisando rankings gerados com caracteristicas de forma
analyse_rankings(ranking_s_biloba, ground_truth_biloba)
analyse_rankings(ranking_s_europaea, ground_truth_europaea)
analyse_rankings(ranking_s_ilex, ground_truth_ilex)
analyse_rankings(ranking_s_monogyna, ground_truth_monogyna)
analyse_rankings(ranking_s_regia, ground_truth_regia)

#-----------------------------#
# (e) ANALISE da consulta regia: o que domina a
# distancia euclidiana em cada descritor

# fracao media de d^2 = sum((x_i - q_i)^2) explicada por cada
# componente do vetor (na.rm descarta a propria consulta, 0/0)
contrib_dist <- function(M, query) {
  d2 <- sweep(M, 2, M[query, ])^2
  colMeans(d2 / rowSums(d2), na.rm = TRUE)
}

classes <- as.vector(nome_classes)
dims <- t(sapply(imagens, function(img)
  c(largura = width(img), pixels = width(img) * height(img))))

# textura: bins com maior peso na distancia
contrib_t <- contrib_dist(features_t, consulta_regia)
top_t <- order(contrib_t, decreasing = TRUE)[1:5]
round(setNames(contrib_t[top_t], top_t), 3)
# convertendo lbp.u2 para a notação convencional
bn <- matrix(dec2bin(0:255), ncol = 8)
cod_uniformes <- which(apply(bn, 1, lbnum) <= 2) - 1 # até 2 transições
bn[cod_uniformes[top_t[1] + 1] + 1, ]

# imagens de ilex que apareceram nas 10 primeiras posições do ranking
ilex_top10 <- c("plantas//ilex_06.jpg", "plantas//ilex_08.jpg",
                "plantas//ilex_10.jpg")
cbind(padrao = features_t[c(consulta_regia, ilex_top10), top_t[1]],
      dims[c(consulta_regia, ilex_top10), ])
tapply(features_t[, top_t[1]], classes, mean)
head(ranking_t_regia, 10)

# cor: fracao da distancia nos bins claros (200-255 nos 3 canais)
head(ranking_c_regia, 10)
contrib_c <- contrib_dist(features_c, consulta_regia)
bins_claros <- c(200:255, 255 + 200:255, 510 + 200:255)
sum(contrib_c[bins_claros])
# pixels claros (canal R) e total de pixels, na ordem do ranking
data.frame(claros = rowSums(features_c[, 200:255]),
           pixels = dims[, "pixels"])[ranking_c_regia[1:25], ]
which(ground_truth_regia[ranking_c_regia] == 1)
# cor media dos pixels da folha (cinza < 0.6), por classe
cor_folha <- function(img, limiar = 0.6) {
  folha <- as.matrix(grayscale(img)) < limiar
  c(R = mean(R(img)[folha]), G = mean(G(img)[folha]),
    B = mean(B(img)[folha]))
}
round(apply(t(sapply(imagens, cor_folha)), 2,
            function(x) tapply(x, classes, mean)), 2)

# forma: fracao da distancia por momento
contrib_s <- contrib_dist(features_s, consulta_regia)
round(setNames(contrib_s, paste0("mu", rep(0:2, each = 3),
                                 rep(0:2, 3))), 3)
# relacao entre a distancia de forma e a diferenca de largura
cor(get_distance_vector(features_s, consulta_regia),
    abs(dims[, "largura"] - dims[consulta_regia, "largura"]),
    method = "spearman")
tapply(dims[, "largura"], classes, range)
head(ranking_s_regia, 10)

#-----------------------------#
# (f) precisao media em top 10 nas 5 consultas

# pares (ground truth, ranking) de cada descritor, no formato usado
# por mean_average_precision e generate_df_11_points
pares_c <- list(biloba   = list(ground_truth_biloba, ranking_c_biloba),
                europaea = list(ground_truth_europaea, ranking_c_europaea),
                ilex     = list(ground_truth_ilex, ranking_c_ilex),
                monogyna = list(ground_truth_monogyna, ranking_c_monogyna),
                regia    = list(ground_truth_regia, ranking_c_regia))
pares_t <- list(biloba   = list(ground_truth_biloba, ranking_t_biloba),
                europaea = list(ground_truth_europaea, ranking_t_europaea),
                ilex     = list(ground_truth_ilex, ranking_t_ilex),
                monogyna = list(ground_truth_monogyna, ranking_t_monogyna),
                regia    = list(ground_truth_regia, ranking_t_regia))
pares_s <- list(biloba   = list(ground_truth_biloba, ranking_s_biloba),
                europaea = list(ground_truth_europaea, ranking_s_europaea),
                ilex     = list(ground_truth_ilex, ranking_s_ilex),
                monogyna = list(ground_truth_monogyna, ranking_s_monogyna),
                regia    = list(ground_truth_regia, ranking_s_regia))
descritores <- list(cor = pares_c, textura = pares_t, forma = pares_s)

# AP@10 de cada consulta e a media (MAP@10)
ap_10 <- sapply(descritores, function(p) 
  sapply(p, function(x) ap(x[[1]], x[[2]], 10)))
round(rbind(ap_10, MAP = sapply(descritores, mean_average_precision, 
                                k = 10)), 3)

# cor: peso dos bins claros na distancia, para cada consulta
consultas <- c(biloba = consulta_biloba, europaea = consulta_europaea,
               ilex = consulta_ilex, monogyna = consulta_monogyna,
               regia = consulta_regia)
round(sapply(consultas, function(q) 
  sum(contrib_dist(features_c, q)[bins_claros])), 3)

# textura: biloba e monogyna se misturam nos rankings uma da outra
head(ranking_t_biloba, 10)
head(ranking_t_monogyna, 10)

#-----------------------------#
# (g) precisao x revocacao interpolada em 11 pontos (media das 5 
# consultas)
df_c_11_points <- generate_df_11_points(pares_c)
df_t_11_points <- generate_df_11_points(pares_t)
df_s_11_points <- generate_df_11_points(pares_s)
plot_precision_x_recall_11_points_t2(
  rbind(df_c_11_points, df_t_11_points, df_s_11_points),
  c("Cor", "Textura", "Forma"),
  "Precisao x Revocacao interpolada em 11 pontos (5 consultas)")
# os 11 pontos lado a lado e a media de cada curva
pontos_11 <- data.frame(rec = df_c_11_points$rec, 
                        cor = df_c_11_points$prec,
                        textura = df_t_11_points$prec, 
                        forma = df_s_11_points$prec)
round(pontos_11, 3)
round(colMeans(pontos_11[, -1]), 3)
# curvas de 11 pontos por consulta, para ver quem puxa cada media
lapply(descritores, function(p)
  round(sapply(p, function(x) generate_df_11_points(list(x))$prec), 2))
# media das curvas so nas consultas europaea, ilex e regia
round(cbind(rec = df_c_11_points$rec, sapply(descritores, function(p)
  generate_df_11_points(p[c("europaea", "ilex", "regia")])$prec)), 3)

#----------------------------------------------------------------#
# Questao 2 - RESPONDA:                   
# (e) Consulta escolhida: regia_07
#     Resultados (P@5 | P@10 | AP@10 | R@20 | AP@20):
#       - Cor:     0,8 | 0,6 | 0,444 | 0,8 | 0,538
#       - Textura: 0,6 | 0,7 | 0,575 | 1,0 | 0,778
#       - Forma:   0,4 | 0,2 | 0,167 | 0,4 | 0,217
#     O melhor ranking foi o de TEXTURA. A cor só leva vantagem no top 5
#     (P@5 = 0,8 contra 0,6) e cai a partir do top 10. A textura
#     tem precisão, F1 e precisão média maiores do top 10 em diante e
#     recupera as 90% das regias com k=15. A recuperação por forma foi
#     bem pior que as outras duas.
#     Visualmente, as folhas régias ocupam boa parte da imagem. A cor é
#     marrom-acinzentada com aspecto enrugado e nervuras marcadas.
#     - Textura: cerca de 80% da distância vem do bin do padrão LBP
#       11111111, típico de regiões homogêneas (como o fundo liso). Como a
#       folha grande e enrugada deixa pouca área homogênea, a regia_07 tem
#       ~161 mil pixels nesse padrão, contra ~320 mil em biloba e
#       monogyna, que têm muito fundo branco. Os erros (ilex_06, ilex_08 e
#       ilex_10, nas posições 4, 5 e 8) são folhas escuras e granuladas
#       em imagens menores, que acabam com uma contagem parecida
#       (~170 mil).
# 
#     - Cor: o histograma não guarda informação espacial e usa contagens
#       absolutas, e ~87% da distância vem dos bins claros, ou seja, do
#       fundo. As europaea têm folha também escura e pouco saturada e, por
#       serem imagens pequenas, uma quantidade de fundo parecida com a da
#       regia_07. Por isso entram no ranking nas posições 2, 6, 7 e 9.
# 
#     - Forma: os momentos não são normalizados e são calculados na imagem
#       inteira, então a distância depende quase só do momento mu22, que
#       reflete as dimensões da imagem. A largura das regia (534-671 px) é
#       parecida com a de biloba, ilex e monogyna, e o ranking traz
#       imagens de tamanho parecido de qualquer classe (só regia_03 entra
#       no top 10).
# (f) AP@10 de cada consulta e a média (MAP@10):
#       consulta    cor    textura  forma
#       biloba      1,000  0,392    0,125
#       europaea    0,569  0,900    1,000
#       ilex        0,332  0,300    0,393
#       monogyna    1,000  0,271    0,275
#       regia       0,444  0,575    0,167
#       MAP@10      0,669  0,487    0,392
#     Por essa medida, a COR teve o melhor resultado, seguida da textura
#     e da forma. A vantagem, porém, vem quase toda de biloba e monogyna,
#     as duas com AP@10 = 1. Nas outras três consultas a cor fica entre
#     0,33 e 0,57 e não é a melhor em nenhuma delas.
#     Isso acontece porque, em todas as consultas, 87% a 99.6% da
#     distância de cor vem dos bins claros (fundo).
#     A textura é a melhor só na regia e vai bem na europaea (mAP 0.90), mas
#     confunde biloba e monogyna (5 imagens da outra classe no top 10 de
#     cada uma).
# 
# (g) dica: use a função generate_df_11_points para gerar o 
# data.frame interpolado para cada descritor. Depois, use a função
# plot_precision_x_recall_11_points_t2 para gerar o gráfico. 
# Exemplo de chamada da função generate_df_11_points:
# df_c_11_points <- generate_df_11_points(list(list(ground_truth_biloba, ranking_c_biloba), list(ground_truth_europaea, ranking_c_europaea)))
# Exemplo de chamada da função plot_precision_x_recall_11_points_t2:
# plot_precision_x_recall_11_points_t2(rbind(df_c_11_points, df_t_11_points, df_s_11_points), c("Cor", "Textura", "Forma"), "Titulo do Gráfico")
#
#     Precisão interpolada (média das 5 consultas):
#       rev.     0,0  0,1  0,2  0,3  0,4  0,5  0,6  0,7  0,8  0,9  1,0
#       cor      1,00 1,00 0,96 0,87 0,87 0,77 0,68 0,66 0,66 0,65 0,65
#       textura  1,00 1,00 0,91 0,67 0,67 0,65 0,64 0,64 0,64 0,64 0,52
#       forma    1,00 1,00 0,74 0,54 0,54 0,52 0,43 0,43 0,43 0,43 0,41
#     Média dos 11 pontos: cor 0,798, textura 0,726, forma 0,587.
#     A curva da COR fica acima das outras em todos os níveis de
#     recall, então ela teve o melhor resultado geral, seguida da
#     textura. A forma fica abaixo de todas a partir de recall = 0.2.
#     Note que os pontos 0,0 e 0,1 valem 1 nos
#     três descritores porque a própria consulta é sempre a 1ª imagem do
#     ranking.
#     A maior diferença entre cor e textura está no recall entre 0,3 e 0,5
#     (precisão = 0,874 contra 0,672 em 0,3 e 0,4). De 0,6 a 0,9, as duas quase empatam
#     (diferença menor que 0,04), e em 1,0 a textura cai para 0,515,
#     enquanto a cor fica em 0,653.
#     As curvas por consulta mostram que a média da cor é sustentada por
#     biloba e monogyna, que têm precisão 1 em todos os níveis (pelo
#     fundo, como explicado na (f)). Considerando só europaea, ilex e
#     regia, a textura fica acima da cor a partir de 0,5 de revocação
#     (0,717 contra 0,613 em 0,5, 0,717 contra 0,468 em 0,6 e 0,561
#     contra 0,422 em 1,0), e até a forma supera a cor nessa faixa.
#     Assim, pela média a cor é a melhor, mas a textura é mais estável
#     nas consultas em que o fundo não ajuda a separar as classes.
#----------------------------------------------------------------#



#----------------------------------------------------------------#
# Questao 3
#----------------------------------------------------------------#
# concatenando caracteristicas                      

## obter vetores finais de caracteristicas pela concatenação de 
# cada tipo de caracteristica (cor, textura e forma):
features_concat <- cbind(features_c, features_t, features_s)

# gerar novos rankings
ranking_concat_biloba <- get_ranking_by_distance(features_concat, consulta_biloba, method = "euclidean")
ranking_concat_europaea <- get_ranking_by_distance(features_concat, consulta_europaea, method = "euclidean")
ranking_concat_ilex <- get_ranking_by_distance(features_concat, consulta_ilex, method = "euclidean")
ranking_concat_monogyna <- get_ranking_by_distance(features_concat, consulta_monogyna, method = "euclidean")
ranking_concat_regia <- get_ranking_by_distance(features_concat, consulta_regia, method = "euclidean")

# analisando rankings gerados com caracteristicas concatenadas
analyse_rankings(ranking_concat_biloba, ground_truth_biloba)
analyse_rankings(ranking_concat_europaea, ground_truth_europaea)
analyse_rankings(ranking_concat_ilex, ground_truth_ilex)
analyse_rankings(ranking_concat_monogyna, ground_truth_monogyna)
analyse_rankings(ranking_concat_regia, ground_truth_regia)

#-----------------------------#
# (d) consulta regia: concatenado x descritores individuais
pares_concat <- list(biloba   = list(ground_truth_biloba, ranking_concat_biloba),
                     europaea = list(ground_truth_europaea, ranking_concat_europaea),
                     ilex     = list(ground_truth_ilex, ranking_concat_ilex),
                     monogyna = list(ground_truth_monogyna, ranking_concat_monogyna),
                     regia    = list(ground_truth_regia, ranking_concat_regia))
descritores_concat <- c(descritores, list(concat = pares_concat))
lapply(descritores_concat, function(p)
  analyse_rankings(p$regia[[2]], p$regia[[1]]))
# top 10 com as dimensoes das imagens (todas com altura 800)
dims[head(ranking_concat_regia, 10), ]
# so a europaea tem imagens pequenas, o que a separa pelo tamanho
tapply(dims[, "largura"], classes, range)

#-----------------------------#
# (e) peso de cada descritor na distancia do vetor concatenado

# fracao de d^2 explicada por cada bloco (cor, textura, forma)
blocos <- rep(c("cor", "textura", "forma"),
              c(ncol(features_c), ncol(features_t), ncol(features_s)))
signif(sapply(consultas, function(q)
  tapply(contrib_dist(features_concat, q), blocos, sum)), 3)
# dentro do bloco de forma, so o mu22 importa
round(contrib_dist(features_concat, consulta_regia)[blocos == "forma"], 4)
# ordem de grandeza dos valores e das distancias de cada bloco
feats <- list(cor = features_c, textura = features_t, forma = features_s)
signif(sapply(feats, function(M) max(abs(M))), 3)
signif(sapply(feats, function(M)
  median(sqrt(rowSums(sweep(M, 2, M[consulta_regia, ])^2)))), 3)
# o ranking concatenado e identico ao de forma em todas as consultas
sapply(names(consultas), function(q)
  identical(pares_concat[[q]][[2]], pares_s[[q]][[2]]))
# sobreposicao do top 10 com cada descritor individual
round(sapply(descritores[c("cor", "textura", "forma")], function(p)
  sapply(names(consultas), function(q)
    jaccard_index(pares_concat[[q]][[2]], p[[q]][[2]], 10))), 3)

# experimento: padronizando cada coluna (z-score) antes de concatenar
features_concat_z <- cbind(scale(features_c), scale(features_t),
                           scale(features_s))
round(sapply(consultas, function(q)
  tapply(contrib_dist(features_concat_z, q), blocos, sum)), 3)
pares_concat_z <- lapply(names(consultas), function(q)
  list(pares_concat[[q]][[1]],
       get_ranking_by_distance(features_concat_z, consultas[[q]])))
names(pares_concat_z) <- names(consultas)
analyse_rankings(pares_concat_z$regia[[2]], ground_truth_regia)
round(c(sapply(pares_concat_z, function(x) ap(x[[1]], x[[2]], 10)),
        MAP = mean_average_precision(pares_concat_z, 10)), 3)

#-----------------------------#
# (f) curva de 11 pontos incluindo o descritor concatenado
df_concat_11_points <- generate_df_11_points(pares_concat)
plot_precision_x_recall_11_points_t2(
  rbind(df_c_11_points, df_t_11_points, df_s_11_points, df_concat_11_points),
  c("Cor", "Textura", "Forma", "Concatenado"),
  "Precisao x Revocacao interpolada em 11 pontos (5 consultas)")
pontos_11_concat <- cbind(pontos_11, concat = df_concat_11_points$prec)
round(pontos_11_concat, 3)
round(colMeans(pontos_11_concat[, -1]), 3)
# MAP@10 com o concatenado
round(sapply(descritores_concat, mean_average_precision, k = 10), 3)
# curva do concatenado por consulta
round(sapply(pares_concat, function(x)
  generate_df_11_points(list(x))$prec), 2)
# media da curva do concatenado padronizado (z-score)
round(mean(generate_df_11_points(pares_concat_z)$prec), 3)


#----------------------------------------------------------------#
# Questao 3 - RESPONDA:
# (d) Consulta escolhida: regia_07
#     Resultados (P@5 | P@10 | AP@10 | R@20 | AP@20):
#       - Concatenado: 0,4 | 0,2 | 0,167 | 0,4 | 0,217
#       - Cor:         0,8 | 0,6 | 0,444 | 0,8 | 0,538
#       - Textura:     0,6 | 0,7 | 0,575 | 1,0 | 0,778
#       - Forma:       0,4 | 0,2 | 0,167 | 0,4 | 0,217
#     Os descritores combinados apresentaram resultados piores. O ranking
#     concatenado tem exatamente as mesmas métricas da forma, que já era
#     o pior descritor para a regia, e fica bem abaixo da cor e da
#     textura em todos os tops. No top 10, além da própria consulta, só
#     aparece a regia_03; o resto são imagens de outras classes
#     (ilex_05, biloba_03, monogyna_10, ...) com tamanho parecido com o
#     da regia_07: todas têm 800 px de altura e largura entre 473 e
#     586 px (a regia_07 tem 534).
#     O mesmo acontece nas outras consultas, o concatenado repete a
#     forma. Ele vai bem na europaea (AP@10 = 1), cujas imagens são as
#     únicas pequenas (196 a 265 px de largura) e se separam das demais
#     só pelo tamanho, e na ilex (0,393), onde a própria forma já era
#     o melhor descritor, mas vai mal na biloba (0,125) e na monogyna (0,275),
#     justamente as consultas em que a cor tinha AP@10 = 1. Ou seja, a concatenação
#     simples não juntou o que cada descritor tinha de melhor.
#
# (e) Sim, a forma domina completamente o ranking concatenado. A
#     concatenação foi feita sem normalização, e os blocos têm ordens
#     de grandeza muito diferentes, cor e textura são contagens de
#     pixels (até ~4,3e5), enquanto os momentos centrais de forma chegam
#     a ~1,4e15 (o mu22 da regia_07 vale 4,3e14). A distância mediana
#     até a regia_07 é de 1,6e5 na cor, 8,4e4 na textura e 2,2e14 na
#     forma. Assim, a forma responde por praticamente 100% de d^2 em
#     todas as consultas (a cor e a textura somam menos de 3e-16), e
#     dentro da forma o mu22 sozinho explica 100%.
#     Como consequência, o ranking concatenado é idêntico ao de forma
#     nas 5 consultas (Jaccard@10 = 1 com a forma, contra 0,11 com a cor
#     e com a textura na regia). Na prática, ele ordena as imagens pelo
#     mu22, que, como vimos na Q2 (e), reflete o tamanho da imagem. A
#     forma não domina por ser mais informativa, mas por causa da escala.
#     Para confirmar, padronizamos cada coluna (z-score) antes de
#     concatenar: aí a regia_07 passa a ter P@10 = 0,8 e AP@10 = 0,800
#     (melhor que qualquer descritor isolado) e o MAP@10 sobe para
#     0,603. Mesmo assim, a influência continua desigual, agora a favor
#     da cor, que responde por 87% a 94% de d^2 por ter 765 das 832
#     dimensões. Logo, a combinação por concatenação exige normalizar e
#     ponderar os blocos.
#
# (f) Precisão interpolada (média das 5 consultas):
#       rev.         0,0  0,1  0,2  0,3  0,4  0,5  0,6  0,7  0,8  0,9  1,0
#       concatenado  1,00 1,00 0,74 0,54 0,54 0,52 0,43 0,43 0,43 0,43 0,41
#     Média dos 11 pontos: cor 0,798, textura 0,726, forma 0,587 e
#     concatenado 0,587. MAP@10: cor 0,669, textura 0,487, forma 0,392 e
#     concatenado 0,392.
#     A curva do concatenado fica exatamente sobre a da forma (ponto a
#     ponto; no gráfico, a linha da forma cobre a do concatenado e só
#     os pontos aparecem) e, a partir de 0,2 de revocação, abaixo das curvas de cor
#     e de textura. Por consulta, o concatenado só é bom na europaea
#     (precisão 1 em todos os níveis); na biloba fica em ~0,25 e na
#     regia em 0,28 a partir de 0,3 de revocação.
#     As conclusões se mantêm e ficam mais fortes:
#     - (d): o concatenado é pior não só na regia, mas também na média
#       das 5 consultas, empatando com o pior descritor individual.
#     - (e): a sobreposição exata das curvas (e dos rankings) em todas as
#       consultas mostra que o domínio da forma não é um caso isolado da
#       regia, mas um efeito da escala dos momentos.
#     Com a padronização (z-score), a média da curva sobe para 0,758,
#     acima da textura e da forma, mas ainda abaixo da cor, o que é
#     coerente com a cor dominar a distância nessa versão.
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
