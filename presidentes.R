# Primero ####
library("pdftools")
library(tidyverse)
library(tidytext)
library(GGally)
library(ggthemes)
#Defino la dirección de trabajo a la ubicación del file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls()) #Borro todo en el environment


#Primero llamo a los planes de gobierno en PDF
Fuerza_Popular = pdf_data("Data/Fuerza_popular.PDF")
Partido_Morado= pdf_data("Data/Partido_morado.PDF")
Accion_Popular = pdf_data("Data/Accion_popular(peor_partido).PDF")
JP = pdf_data("Data/Juntos_por_Peru(JP).PDF")
Somos_Peru = pdf_data("Data/Somos_peru.PDF")
Victoria_Nacional = pdf_data("Data/Victoria_nacional.PDF")
#Luego los trato de agrupar
z = tibble() #Base de datos vacia
cosas = objects()  #los nombres de todas las variables en el environment

#Ahora, sumaré todos los planes de gobierno (y sus hojas) en una sola base de
#datos
for (i in cosas[cosas!="z"]){
  x = get(i)
  for(j in 1:length(x)){
    if(j==1){
      z1 = tibble(x[[j]]["text"],Hoja = j,Partido = i)
      z = bind_rows(z,z1)
    }else{
      z = x[[j]]["text"] %>% 
        tibble(Hoja = j,Partido=i) %>% 
        {bind_rows(z,.)}
    }
  }
}

#Elimino todas las variables que no sean "z" 
rm(list=setdiff(ls(),"z"))

#El procesamiento####
#Primero convoco a las stop_words (palabras inútiles)
stop_words = read.table("Stopwords.txt",encoding = "UTF-8") %>%
  rename("word" = V1) %>%
  mutate(word = as.character(word)) %>%
  add_row(word=c("año")) %>% #Añades palabras inutiles según el contexto (y los resultados)
  as_tibble()

#Proceso el acumulado de planes de gobierno
  tidy_detalle = z %>%
    unnest_tokens(word,text) %>% #Función unnest, cada palabra es una observación (sin signos)
  anti_join(stop_words) %>% #Elimino stop words
  filter(grepl("^[A-Za-z]+$",word)) #Solo palabras (sin numeros ni signos)

  tidy_lineas = z %>%
    group_by(Partido) %>%
    summarise(text = paste(text,collapse = " ")) %>%
    unnest_tokens(output = word,token = "regex",text,pattern = "(?<=\\.)\\s+")
  
# Sentimientos
  sentiment  = readxl::read_excel("dictionary.xlsx") %>% 
    rename("word" = "term") %>% 
    filter(word!="mil" & word!="popular" & word!="victoria")
  
  palabras_sentimiento = tidy_detalle %>%
    inner_join(sentiment) %>%
    mutate(score = ifelse(score_pos=="1","Positivo","Negativo")) %>%
    count(word,score,sort=TRUE) 
  #Bigramas
  
  omision2 = tibble(bigram = c("partido morado","fuerza popular",
                               "victoria nacional","acción popular",
                               "sua ama","llulla ama",
                               "morado propone","ingreso 15",
                               "expuesto proponemos",
                               "sentido proponemos",
                               "indicadores porcentaje","objetivos indicadores",
                               "pbi 1.1","pilar estratégico","20 mil","mil soles","mil habitantes",
                               "indicadores número","identificados objetivos","problemas identificados",
                               "metas 2021","lineamientos generales","indicadores metas",
                               "problema indicadores","metas contar","situación actual",
                               "físico legal","alto corresponde","porcentual anual",
                               "política política"))

# Parte Global ------------------------------------------------------------
  
  library(widyr)
  library(igraph)
  library(wordcloud)
  library(wordcloud2)
  library(RColorBrewer)
  library(scales)
  #Hago el conteo simple
  
  tidy_detalle %>% 
    count(word,sort=TRUE) %>% 
    top_n(5,n) 
#El mapa de palabras total
  set.seed(42)
  tidy_detalle %>%
    count(word,sort = TRUE) %>%
    wordcloud2(size=1,fontWeight = "bold",color="random-dark",minSize = "Hola todos",
               maxRotation = pi/9,minRotation = -pi/9) 
 
  #Sentimiento global (positivo y negativo)

  dev.new(width = 2000, height = 2000, unit = "px")
  palabras_sentimiento  %>%
    select(word,score,n) %>%
    acast(word ~ score, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("darkred", "darkblue"),random.order = FALSE,
                     max.words = 400,scale=c(4,.1),title.size = 2,5)
  
  #Palabras comunes en todos los planes
  tidy_detalle %>%
    inner_join(sentiment) %>%
    mutate(score = ifelse(score_pos=="1","Positivo","Negativo")) %>%
    count(word,Partido,sort=TRUE)  %>% 
    acast(word ~ Partido, value.var = "n", fill = 0) %>% 
    commonality.cloud(colors = brewer.pal(5, "Dark2"))
  
  # Bigrama total
  library(ggraph)
  library(tidygraph)
  
# Comparación Partidos ----------------------------------------------------

 #El top 3 por partido
  tidy_detalle %>% 
    group_by(Partido) %>% 
    count(word,sort=TRUE) %>% 
    top_n(3,n) %>% 
    arrange(Partido,-n)
  
  #Proceso la data para graficarla
  p = tidy_detalle%>% 
    group_by(Partido) %>% #Agrupo por partido
    count(word, sort = TRUE)  %>%  #Cuento las palabras
    top_n(15,n) %>% #Me quedo con el top 15
    mutate(word = reorder(str_to_title(word), n)) %>% #Le pongo mayuscula al inicio
    ungroup() %>% 
    arrange(Partido,n) %>% #Ordeno por partido y numero
    mutate(order = row_number()) #El orden (para el gráfico)
  
  #grafico con GGPLOT
  p %>% 
    ggplot(aes(order, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    facet_wrap(~Partido,scales = "free") +
    scale_x_continuous(
      breaks = p$order,
      labels = p$word,
      expand=c(0,0)
    ) +
    theme_wsj() +
    theme(strip.text.x = element_text(size = 12, face ="bold",colour = "black"))

z %>%
  group_by(Partido) %>%
  summarise(text = paste(text,collapse = " ")) %>% 
  ungroup() %>% 
  unnest_tokens(bigram,text,token = "ngrams", n=2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(bigram, word1, word2, sep = " ") %>%
  anti_join(omision2) %>% 
  count(Partido, bigram) %>%
  bind_tf_idf(bigram, Partido, n) %>%
  group_by(Partido) %>% 
  arrange(desc(tf_idf)) %>%
  dplyr::top_n(n=10) %>% 
  ungroup() %>% 
  mutate(bigram = str_to_title(bigram)) %>% 
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = Partido)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Partido, ncol = 2, scales = "free") +
  theme_wsj() +
  theme(strip.text.x = element_text(size = 12, face ="bold",colour = "black")) +
  labs(x = "TF-IDF", y = "Palabras",title = "Palabras TD_IDF en planes de gobierno",subtitle = "Palabras poco comunes en los demás planes de gobierno")

ggsave("imagen.png",height = 30,width = 21,units = "cm")
z %>%
  unnest_tokens(bigram,text,token = "ngrams", n=3)  %>%
  na.omit() %>% 
  separate(bigram, c("word1", "word2","word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  count(word1, word2,word3, sort = TRUE) 

z %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>% 
  unnest_tokens(word,text) %>% #Función unnest, cada palabra es una observación (sin signos)
  anti_join(stop_words) %>% #Elimino stop words
  filter(word != "fuerza",word != "popular",word !="morado") %>% 
  filter(grepl("^[A-Za-z]+$",word)) %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE) %>%
  filter(correlation > .30) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

frecuencia = tidy_detalle%>%
  count(Partido, word) %>%
  group_by(Partido) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(Partido, proportion) %>% 
  gather(Partido, proportion, c(`Accion_Popular`,`Fuerza_Popular`,`JP`,`Somos_Peru`,`Victoria_Nacional`))

omision3 = tibble(word=c("mmm","ii","alta","altas","inclusive",
                         "mil","etc","art","partido","haga","forma",
                         "asimismo","alcance","camino","vii"))
frecuenia = tidy_detalle%>%
  count(Partido, word) %>%
  group_by(Partido) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  anti_join(omision3)
library(scales)

# expect a warning about rows with missing values being removed
ggplot(frecuencia, aes(x = proportion, y = `Partido_Morado`, color = abs(`Partido_Morado` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~Partido) +
  theme(legend.position="none") +
  labs(y = "Partido_Morado", x = NULL)  +
  theme(strip.text.x = element_text(size = 12, face ="bold",colour = "black"))

ggplot(frecuenia, aes(x = proportion, y = proportion)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~Partido) +
  theme(legend.position="none")

#Hare un grafico matriz con correlación en la punta derecha

tidy_detalle %>% 
  inner_join(sentiment) %>%
  group_by(Partido) %>% 
  count(word,sort=TRUE) %>% 
  top_n(15,n) %>% 
  arrange(Partido,-n) %>% 
  View()

detalle_sentimiento = tidy_detalle %>%
  inner_join(sentiment) %>%
  mutate(score = ifelse(score_pos=="1","Positivo","Negativo")) %>%
  count(Partido, score) %>%
  spread(score, n, fill = 0) %>%
  mutate(score = (Positivo - Negativo)/(Positivo+Negativo)) %>% 
  arrange(score)

p2 = tidy_detalle %>% 
  inner_join(sentiment) %>%
  group_by(Partido) %>% 
  count(word,sort=TRUE) %>% 
  top_n(15,n) %>%
  ungroup() %>% 
  arrange(Partido,n) %>% 
  mutate(order = row_number()) 

p2 %>% 
  ggplot(aes(order, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~Partido,scales = "free") +
  scale_x_continuous(
    breaks = p2$order,
    labels = p2$word,
    expand=c(0,0)
  ) +
  theme_wsj() +
  theme(strip.text.x = element_text(size = 12, face ="bold",colour = "black"))

ggplot(detalle_sentimiento, aes(fct_reorder(Partido,desc(score)),score, fill = Partido)) +
  geom_col(show.legend = FALSE)

p3 = palabras_sentimiento %>%
  group_by(score,Partido) %>%
  top_n(10,pro) %>%
  ungroup() %>%
  mutate(word = reorder(word, pro)) %>%
  mutate(order = row_number())
p3 %>% 
  ggplot(aes(order, pro, fill = score)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Partido , scales = "free") +
  scale_x_continuous(
    breaks = p3$order,
    labels = p3$word,
    expand=c(0,0)
  ) +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()



  library(reshape2)
  par(mar = rep(0, 4))
  palabras_sentimiento  %>%
    select(word,Partido,score,n) %>%
    acast(word ~ Partido, value.var = "n", fill = 0) %>%
    comparison.cloud(random.order = FALSE,title.size=1,colors = c("black", "orange2","red","mediumblue","#6600CC","seagreen4"),
                     max.words = 600)
  
 tidy_itf =  z %>%
    unnest_tokens(word,text) %>% 
    filter(grepl("^[A-Za-z]+$",word))  %>% 
    count(Partido,word,sort=TRUE)
 
 omision = tibble(word = c("fujimorismo","morado","morada","c",
                           "g","victoria","ama","seas","sua","quella",
                           "llulla","https","mil","juan","forma",
                           "respectivas","lineamientos","ocioso","mentiroso","b",
                           "partido","modos","identificados","indicadores","inclusive",
                           "encontramos","respectivamente","permitido",
                           "lineamiento","plantean"))
  
 tidy_itf %>%
   anti_join(omision) %>% 
    bind_tf_idf(word, Partido, n) %>% 
   group_by(Partido) %>% 
   slice_max(tf_idf,n=15) %>% 
   ungroup() %>% 
   mutate(word = str_to_title(word)) %>% 
   ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Partido)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~Partido, ncol = 2, scales = "free") +
   theme_wsj() +
   theme(strip.text.x = element_text(size = 12, face ="bold",colour = "black")) +
   labs(x = "TF-IDF", y = "Palabras",title = "Palabras TD_IDF en planes de gobierno",subtitle = "Palabras poco comunes en los demás planes de gobierno")
 
 tidy_detalle%>%
   count(Partido, word) %>%
   group_by(Partido) %>%
   mutate(proportion = n / sum(n)) %>% 
   select(-n) %>% 
   anti_join(omision) %>% 
   filter()
 
 
 ggplot(frecuencia, aes(x = proportion, y = `Partido_Morado`, color = abs(`Partido_Morado` - proportion))) +
   geom_abline(color = "gray40", lty = 2) +
   geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
   geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
   scale_x_log10(labels = percent_format()) +
   scale_y_log10(labels = percent_format()) +
   scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
   facet_wrap(~Partido) +
   theme(legend.position="none") +
   labs(y = "Partido_Morado", x = NULL)  +
   theme(strip.text.x = element_text(size = 12, face ="bold",colour = "black"))
 
 p80 = frecuenia %>% 
   pivot_wider(word,names_from=Partido,values_from=proportion)
 
 p80%>%
   select(-word) %>% 
   ggpairs(diag=list(continuous = "autopointDiag"),
           lower = list(continuous = 
                          function(data,mapping,...){
                            ggplot(data = data,mapping=mapping) + 
                              geom_abline(color="gray20",lty = 2) + 
                              geom_jitter(alpha=0.1,size=2.5,width = 0.3,height = 0.3,colour="gray75") +
                              geom_text(aes(label = p80$word), check_overlap = TRUE, vjust = 1.5,size=2,colour="red") +
     scale_x_log10(labels = percent_format()) +
     scale_y_log10(labels = percent_format()) +
     scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
                              theme_minimal()
                            }))
 