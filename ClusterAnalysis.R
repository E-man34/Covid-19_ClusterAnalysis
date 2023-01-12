#LIBRERIE

install.packages("syuzhet") #dizionari in diverse lingu che assegna un punteggio ad alcune emozioni
install.packages("rtweet")
install.packages("tuber")
install.packages("quanteda")
install.packages("quanteda.textplots")
install.packages("tm")
install.packages("tidytext")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("stringr")
install.packages("openxlsx")
install.packages("topicmodels") #x analisi dei topic sui testi
install.packages("tibble") #x data manipulation sui dataframe
install.packages("RColorBrewer")
pkgs <- c("factoextra","NbClust")
install.packages(pkgs)


library(rtweet)
library(tuber)
library(quanteda)
library(quanteda.textplots)
library(tm)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library(stringr)
library(openxlsx)
library(topicmodels)
library(tibble)
library(syuzhet)
library(RColorBrewer)
library(factoextra)
library(NbClust)


#IMPORT DATASET(Dataset_TLAB)

Dataset_TLAB$testo <- gsub("#\\$+", "", Dataset_TLAB$testo) #rimuovere hastag --> df_word$text con il dollaro per dire che si vuole accedere al testo del dataframe "word"
Dataset_TLAB$testo <- gsub("https\\$*", "", Dataset_TLAB$testo) #rimuovere link
Dataset_TLAB$testo <- gsub("@\\$+", "", Dataset_TLAB$testo) #rimuovere menzioni
Dataset_TLAB$testo <- gsub("amp", "", Dataset_TLAB$testo) #rimuovere alcuni caratteri speciali
Dataset_TLAB$testo <- gsub("[\r\n]", "", Dataset_TLAB$testo) #rimuovere interruzioni di riga
Dataset_TLAB$testo <- gsub("[[:punct:]]", "", Dataset_TLAB$testo) #rimuovere segni di punteggiatura 
Dataset_TLAB$testo <- gsub("[[:digit:]]", "", Dataset_TLAB$testo) #rimuovere numeri
Dataset_TLAB$testo <- gsub("[^\x01-\x7F]", "", Dataset_TLAB$testo) #rimuovere caratteri non ASCII


#IMPORTARE STOPWORDS

facebook <- Dataset_TLAB %>% #comando pipe in cui il risultato di questa riga è l'input della riga dopo
  select(testo) %>%
  unnest_tokens(word,testo) #dividiamo la colonna "text" dei post nelle singole parole
facebook <- facebook %>% #si sovrascrive l'oggetto facebook
  anti_join(stopwords) #da cui si sottraggono le stopwords

write.xlsx(facebook,"Facebook_Dataset.xlsx")

#GRAFICO CON TERMINI PIU UTILIZZATI
#facebook %>%                                  #input
 # count(word, sort=TRUE) %>%                  #calcolo numero di ciascuna parola e ordine decrescente da quella che compare di + a quella di -
  #top_n(30) %>%                               #restituire le top 30 parole
  #mutate(word = reorder(word, n)) %>%         #grafico riordinato in modo decrescente
  #ggplot(aes(x = word, y = n)) +              #valori asse X e asse Y
  #geom_col() +                                #più una parola compare e maggiore sarà la grandezza della barra
  #coord_flip() +                              #orientamento orizzontale delle barre
  #labs(y = "frequenza", x = "parole", title = "le parole più utilizzate\n")    #etichette


paroleFB <- str_extract_all(facebook$word, "[A-Za-z-0-9_]+")

parole_vectorFB <- tolower(unlist(paroleFB)) #creazione di una lista con le parole estratte ridotti in minuscolo

tb_paroleFB <- table(parole_vectorFB) #convertiamo la lista in una tabella
head(sort(table(parole_vectorFB), decreasing = TRUE), n = 10) #stampare le prime 10 righe di hashtag ricorrenti in ordine decrescente 
tb_parole2FB <- as.data.frame(tb_paroleFB) #convertire la tabella in dataframe
tb_parole2FB <- tb_parole2FB[order(-tb_parole2FB$Freq),] #ordinare dataframe in ordine decrescente
tb_parole_subFB <- tb_parole2FB[1:30,] #subset con i top 20



#parole <- str_extract_all(Dataset_TLAB$testo, "[A-Za-z-0-9_]+")

#parole_vector <- tolower(unlist(parole)) #creazione di una lista con le parole estratte ridotti in minuscolo

#tb_parole <- table(parole_vector) #convertiamo la lista in una tabella
#head(sort(table(parole_vector), decreasing = TRUE), n = 10) #stampare le prime 10 righe di hashtag ricorrenti in ordine decrescente 
#tb_parole2 <- as.data.frame(tb_parole) #convertire la tabella in dataframe
#tb_parole2 <- tb_parole2[order(-tb_parole2$Freq),] #ordinare dataframe in ordine decrescente
#tb_parole_sub <- tb_parole2[1:30,] #subset con i top 20 

#write.xlsx(tb_parole2,"paroleFrequenza.xlsx")


# RAPPRESENTAZIONE GRAFICA
ggplot(tb_parole_subFB, aes(x=reorder(parole_vectorFB,Freq),y=Freq, fill=parole_vectorFB)) + #rappresentiamo graficamente i primi 20 valori e mettiamo come assi gli # e la frequenza, ma X avrà gli #  ordinati per frequenza e su Y la frequenza
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) + #lunghezza barra proporzionata alla freq degli hashtag
  labs(title = "Parole più utilizzate\n", x="Lemma", y="Frequenza di comparsa")+
  theme_classic() +  #impostare un tema
  coord_flip() #ordinamento barre orizzontali



# WORD CLOUD
set.seed(1234)  #generare un valore casuale utile a creare la visualizzazione
png("wordcloud_TLAB.png", #generare PNG("nome del file da salvare)
    width = 1024,
    height = 1024,
    units = "px",
    res = 500)  #risoluzione

#wordcloud(parole_vector, min.freq = 30, scale = c(3.5, .5), random.order = TRUE, rot.per = 0.35, #parametri per la wordcloud(dove si trovano i valori, frequenza minima, valori di scala, ordine randomico, valore di rotazione)
 #random.color = FALSE, colors = brewer.pal(8, "Dark2")) #creare una palette di colori -> il primo valore indica il numero di colori e il secondo indica il colore primario da input per creare la scala di colori
wordcloud2(tb_parole2FB, size = 1.6, color = "random-dark", shape = "circle")
dev.off() #salvare fisicamente il file





#SENTIMENT ANALYSIS

corpus <- as.character(facebook$word) #convertire colonna commenti in un vettore di char

etichette <- get_nrc_sentiment(corpus, language = "italian") #calcolare i dizionari per le diverse emozioni

#unione calcolo sentiment con il dataset originale
total <- cbind(facebook$word, etichette)

#creazione etichette colonne 
c <- c("Testo","Rabbia","Aspettativa","Disgusto","Paura","Gioia","Tristezza","Sorpresa","Fiducia","Negativo","Positivo")
names(total) <- c
write.xlsx(total,"SentimentAnalysis.xlsx")

get_nrc_sentiment("morire", language = "italian") #ricerca punteggi per parola "emergenza"

#GRAFICO sentiment analysis
barplot(colSums(etichette),
        las = 2,
        col = rainbow(10),
        ylab = "Valore",
        main = "Punteggio Sentiment")


# -*-*-*-*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*__*

# RELAZIONI TRA PAROLE

#creazione dataset di partenza
a_corp <- as.character(Dataset_TLAB$testo) #trasformare colonna del testo in vettore carattere
dataset_dfm <- dfm(a_corp, remove_punct=TRUE) #creare matrice documenti (riga) - termini dei testi (colonna) per vedere quale termine ricorre in quale testo + rimozione punteggiatura

parole_dfm <- dfm_select(dataset_dfm, pattern = ("*")) #selezione su tweet_dfm cercando il pattern # seguito da una parole per mentenere solo i termini che corrispondono ad un hashtag
topparole <- names(topfeatures(parole_dfm, 150)) #mantiene solo le top 500 parole delle parole che ricorrono maggiormente
head(topparole)

parole_fcm <- fcm(parole_dfm) #creare matrice di co-occorrenza
head(parole_fcm)

topparole_fcm <- fcm_select(parole_fcm, pattern = topparole) #selezionare top tag individuati in oggetto toptag_fcm

write.xlsx(topparole_fcm, "Analisi_occorrenze.xlsx")


#creazione grafico network analisi
textplot_network(topparole_fcm,  #matrice di co-occorrenza
                 min_freq = 0.1, #soglia minima di frequenza
                 edge_alpha = 1.2, #opacità legame
                 edge_color = "orange", #colore legame
                 edge_size = 0.5, #spessore del legame
                 vertex_size = 1.2, #grandezza dei vertici
                 vertex_labelsize = 3) 

# -*-*-*-*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*__*

# CALCOLO NUMERO DI K OTTIMALE

# Elbow method
fviz_nbclust(Dataset_TLAB, kmeans, method = "silhouette") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

