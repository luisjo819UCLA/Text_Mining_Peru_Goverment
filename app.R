#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidytext)
library(GGally)
library(scales)

#Primero llamo a los planes de gobierno en PDF
z = read_rds("z.rds")

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


# Parte Global ------------------------------------------------------------

omision3 = tibble(word=c("mmm","ii","alta","altas",
                         "inclusive","mil","etc","art",
                         "partido","haga","forma",
                         "asimismo","alcance","camino","vii"))



choices = data.frame(
    var = unique(tidy_detalle$Partido),
    num = 1:length(unique(tidy_detalle$Partido)))

mylist <- as.list(choices$num)
# Name it
names(mylist) <- choices$var
rm(choices)

frecuenia = tidy_detalle%>%
    count(Partido, word) %>%
    group_by(Partido) %>%
    mutate(proportion = n / sum(n)) %>% 
    select(-n) %>% 
    anti_join(omision3)


shinyApp(
    
    ui = fluidPage(fluidRow(column(
        width = 4,
        selectInput("partido1", "Partido Eje X:",
                    choices = mylist)
    ), column(
        width = 4,
        selectInput(
            "partido2",
            "Partido Eje Y:",
            choices = mylist,
            selected = 2
        )
    ),column(4,downloadLink(outputId = "downloadData1",label = "Descargar Todo"))),
    plotOutput("phonePlot", width = "100%")), 
    
    server = function(input, output) {
        
        output$downloadData1 <- downloadHandler(
            filename = function() {
                paste('Comparacion_planes.pdf', sep='')
            },
            content = function(con) {
                file.copy("hola.pdf",con)
            })
        
        p80 = reactive({
            test = frecuenia %>%
                filter(Partido == unique(frecuenia$Partido)[as.numeric(input$partido1)] | 
                           Partido == unique(frecuenia$Partido)[as.numeric(input$partido2)]) %>% 
                pivot_wider(word,names_from=Partido,values_from=proportion)
            if(length(names(test))==2){
                test %>% 
                    mutate("El mismo" =  .[2])
            } else{
                test
            }
            
        }
        )
        
        correla = reactive({
            cor(p80()[2],p80()[3],use = "complete.obs")
        })
        
        output$phonePlot = renderPlot({
            p80() %>%
                select(-word) %>%
                ggplot(aes(x=.[[1]],y=.[[2]],color=(.[[2]]-.[[1]]))) +
                geom_abline(color="gray20",lty = 2) +
                geom_jitter(alpha=0.1,size=2.5,width = 0.3,height = 0.3,show.legend = FALSE) +
                geom_text(aes(label = p80()$word), check_overlap = TRUE, vjust = 1.5,size=4.5,show.legend = FALSE) +
                scale_x_log10(labels = percent_format()) +
                scale_y_log10(labels = percent_format()) +
                theme_minimal() +
                labs(title = str_replace_all(paste("Planes de Gobierno:",names(p80())[2],"vs",names(p80())[3]),"_"," "),
                     x = paste(names(p80()[2])),
                     y = paste(names(p80()[3])),
                     subtitle = paste("Correlación:",round(correla()*100),"%")) +
                theme(text=element_text(face="bold")) +
                coord_equal()
        },height = 650)
    },      
    options = list(height = 800)
) 
