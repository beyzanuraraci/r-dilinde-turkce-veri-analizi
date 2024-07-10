
#YAZILIM DILLERIYLE DUYGU VE EMOJI ANALIZI :TURKCEPERSPEKTIF

install.packages("tuber")
install.packages("magrittr")
install.packages("purrr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("promises")
install.packages("httpuv")
install.packages("readr")
install.packages("readxl")
install.packages("stringi")
install.packages("stringr")
install.packages("tm")
install.packages("pander")
install.packages("RCurl")
install.packages("wordcloud")
install.packages("ROAuth")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("tidytext")
install.packages("RColorBrewer")
install.packages("ggthemes")
install.packages("ggpubr")
install.packages("formattable")
install.packages("psych")
install.packages("GGally")
install.packages("rstatix")
install.packages("sentimentr")
install.packages("webshot")
install.packages("htmlwidgets")
install.packages("syuzhet")
install.packages("skimr")
install.packages("janitor")
install.packages("openssl")
install.packages("writexl")
install.packages("stopwords")
install.packages("devtools")
install.packages("tibble")
install.packages("rmarkdown")
install.packages("ggstance")
install.packages("pastecs")
install.packages("kableExtra")
install.packages("citation")
install.packages("lubridate")

library(tuber) #Youtube videolarinda yayinlanan videolar icin kullanilir.
library(magrittr) #Fonksiyonlari %>% operatoru ile birbirine baglar.
library(purrr) #D0slevler ve vektorlerle calismak icin eksiksiz ve tutarli bir arac seti saglayan paket.
library(dplyr) #Veri manipC<lasyon islemini yapar.
library(tidyverse) # veri setlerini dC<zenli, anlasilir ve etkili bir sekilde islemeyi saDlayan pakettir.
library(httpuv) #HTTP ve webSocket isteklerini islemek icin dC<sC<k seviyeli soket ve protokol destegi saglayan paket.
library(readr) # verileri okumayi kolaylastirir.
library(readxl) #Excel dosyalarini okur ve R'a yukleme yapar.
library(stringi) #Hizli ve tasinabilir karakter dizisi isleme tesisleri paketi.
library(stringr) #Karakter yapili veriler icin kullanilan paket.
library(tm) #Metin madenciliginde kullanilan paket.
library(pander) # tablolari ve cerceveleri goruntulemek icin tasarlanmistir.
library(wordcloud) #Kelime bulutu icin kullanilan pakettir.
library(ROAuth) # OAuth 1.0 protokolunu uygulamak icin kullanilan bir pakettir.
library(ggplot2) #Verileri gorsellestirmek icin kullanilan paket.
library(tidytext) #DC<zenli veri ilkelerini kullanmak ve bircok metin madenciligi gorevini yerine getirmek icin kullanilir.
library(RColorBrewer) #Kelime bulutunun renklendirilmesi icin kullanilir.
library(ggthemes) #grafiklerinizi daha cekici ve profesyonel gorunumlu hale getirmenize yardimci olur.
library(ggpubr) #ggplot2 ile birlikte kullanilir.
library(GGally) #cesitli grafik araclarini bir araya getiren bir pakettir. 
library(skimr) #veri cercevelerini ve veri setlerini ozetlemek ve kesfetmek icin kullanilan bir pakettir.
library(janitor) #veri setlerini temizlemeye yarar.
library(writexl) #Elde edilen verilerin excel formatinda disa aktarilmasi icin kullanilan pakettir.
library(stopwords) #Veri dosyasindan atilacak kelimeleri cikarmaya yarar. 
library(ggstance) #grafikler olusturmaya yarar.
library(xlsx) #Excel  dosyalarini okumak, yazmak ve bicimlendirmek icin kullanilan pakettir.
library(plyr)
library(ggeasy)
library(RCurl)
library(psych)
library(formattable)
library(rstatix)
library(sentimentr)
library(webshot)
library(htmlwidgets)
library(syuzhet)
library(skimr)
library(janitor)
library(openssl)
library(writexl)
library(stopwords)
library(devtools)
library(tibble)
library(rmarkdown)
library(ggstance)
library(pastecs)
library(kableExtra)
library(citation)
library(lubridate)



#API'LER ILE YOUTUBE VIDEOSUNDAN VERI CEKIYORUZ
client_id <- "****" 
client_secret <-"*****"
yt_oauth(client_id, client_secret, token ='')


#YORUMLARI CEKIYORUZ
T1<- get_all_comments(video_id = "uJzcP5La27s")

T2 <- get_all_comments(video_id = "YHYtyXv1bh4")

T3<- get_all_comments(video_id = "7illTa84TL8")

T4<- get_all_comments(video_id = "4fJkcPCpXS8")



#VERILERIMIZI BIRLESTIRIYORUZ
tr_data <- bind_rows(T1,T2,T3,T4 )

#ISIMIZE YARAMAYACAK SUTUNLARI SILIYORUZ
tr_data<-tr_data[-1:-3]
tr_data<-tr_data[-2:-13]

#BILGISAYARA KAYIT
write_xlsx(tr_data,"C:/Users/beyza/Desktop/datatr.xlsx")

tr_veri1<-tr_data

#VERI TEMIZLEME ISLEMI BASLASIN

#RakamlarD1n temizlenmesi
tr_veri1$textOriginal <- removeNumbers(tr_veri1$textOriginal)

#URL link temizleme
tr_veri1$textOriginal <- str_replace_all(tr_veri1$textOriginal, "http[^[:space:]]*","")

#Tum harflerin kuculmesi
tr_veri1$textOriginal <- str_to_lower(tr_veri1$textOriginal,"tr")

#hastag ve @ kaldD1rD1lmasD1
tr_veri1$textOriginal <- str_replace_all(tr_veri1$textOriginal, "#\\S+","")
tr_veri1$textOriginal <- str_replace_all(tr_veri1$textOriginal, "@\\S+","")

#noktlama isaretleri temizlenmesi
tr_veri1$textOriginal <- str_replace_all(tr_veri1$textOriginal, "[[:punct:][:blank:]]+", " ")

#ASCII formatD1na uymayan karakterlerin temizlenmesi
tr_veri1$textOriginal <- str_replace_all (tr_veri1$textOriginal, "[<].*[>]", " ") 
tr_veri1$textOriginal<- gsub ("\uFFFD", "", tr_veri1$textOriginal, fixed = TRUE)
tr_veri1$textOriginal <- gsub("\n","", tr_veri1$textOriginal, fixed = TRUE)

tr_veri1$textOriginal <- str_replace_all(tr_veri1$textOriginal, "[^[:alnum:]]"," ")
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")



library(stopwords)

stopwords::stopwords("tr",source = "stopwords-iso")

liste<- stopwords::stopwords("tr",source = "stopwords-iso")

kelime_liste = c("acaba","acep","adamakD1llD1","adeta","ait","altmD1E","altD1","ama","amma","anca","ancak","arada","artD1k",
                 "aslD1nda","aynen","ayrD1ca","az","aC'D1kC'a","aC'D1kC'asD1","bana","bari","bazen","bazD1","baEkasD1","baEka","belki",
                 "ben","benden","beni","benim","beri","beriki","ben","beE","bilcC<mle","bile","bin","binaen","binaenaleyh",
                 "bir","biraz","birazdan","birbiri","birden","birdenbire","biri","birice","birileri","birisi","birkaC'","birkaC'D1",
                 "birkez","birlikte","birC'ok","birC'oDu","birEey","birEeyi","bitevi","biteviye","bittabi","biz","bizatihi","bizce",
                 "bizcileyin","bizden","bize","bizi","bizim","bizimki","bizzat","boEuna","bu","buna","bunda","bundan","bunlar",
                 "bunlarD1","bunlarD1n","bunu","bunun","buracD1kta","burada","buradan","burasD1","bC6yle","bC6ylece","bC6ylecene",
                 "bC6ylelikle","bC6ylemesine","bC6ylesine","bC<sbC<tC<n","bC<tC<n","cuk","cC<mlesi","da","daha","dahi","dahil","dahilen",
                 "daima","dair","dayanarak","de","defa","dek","demin","demincek","deminden","denli","derakap","derhal","derken",
                 "deDil","deDin","diye","diDer","diDer","diDeri","doksan","dokuz","dolayD1","dolayD1sD1yla","doDru","dC6rt","edecek",
                 "eden","ederek","edilecek","ediliyor","edilmesi","ediyor","elbet","elbette","elli","emme","en","enikonu","epey",
                 "epeyce","epeyi","esasen","esnasD1nda","etmesi","etraflD1","etraflD1ca","etti","ettiDi","ettiDini","evleviyetle",
                 "evvel","evvela","evvelce","evvelden","evvelemirde","evveli","eDer","eDer","fakat","filanca","gah","gayet",
                 "gayetle","gayri","gayrD1","gelgelelim","gene","gerek","gerC'i","geC'ende","geC'enlerde","gibi","gibilerden",
                 "gibisinden","gine","gC6re","gD1rla","hakeza","halbuki","halen","halihazD1rda","haliyle","handiyse","hangi",
                 "hangisi","hani","hariC'","hasebiyle","hasD1lD1","hatta","hele","hem","henC<z","hep","hepsi","her","herhangi",
                 "herkes","herkesin","hiC'","hiC'bir","hiC'biri","hoE","hulasaten","iken","iki","ila","ile","ilen","ilgili",
                 "ilk","illa","illaki","imdi","indinde","inen","insermi","ise","ister","itibaren","itibariyle","itibarD1yla",
                 "iyi","iyice","iyicene","iC'in","iE","iEte","kadar","kaffesi","kah","kala","kanC=mca","karED1n","katrilyon",
                 "kaynak","kaC'D1","kelli","kendi","kendilerine","kendini","kendisi","kendisine","kendisini","kere","kez",
                 "keza","kezalik","keEke","keEke","ki","kim","kimden","kime","kimi","kimisi","kimse","kimsecik","kimsecikler",
                 "kC<lliyen","kD1rk","kD1saca","kD1rk","kD1saca","lakin","leh","lC<tfen","maada","madem","mademki","mamafih","mebni",
                 "meDer","meDerki","meDerse","milyar","milyon","mu","mC<","mi","mD1","nasC=l","nasD1l","nasD1lsa","nazaran","naEi",
                 "ne","neden","nedeniyle","nedenle","nedense","nerde","nerden","nerdeyse","nere","nerede","nereden","neredeyse",
                 "neresi","nereye","netekim","neye","neyi","neyse","nice","nihayet","nihayetinde","nitekim","niye","niC'in","o",
                 "olan","olarak","oldu","olduklarD1nD1","oldukC'a","olduDu","olduDunu","olmadD1","olmadD1DD1","olmak","olmasD1",
                 "olmayan","olmaz","olsa","olsun","olup","olur","olursa","oluyor","on","ona","onca","onculayD1n","onda",
                 "ondan","onlar","onlardan","onlari","onlarC=n","onlarD1","onlarD1n","onu","onun","oracD1k","oracD1kta","orada",
                 "oradan","oranca","oranla","oraya","otuz","oysa","oysaki","pek","pekala","peki","pekC'e","peyderpey","raDmen",
                 "sadece","sahi","sahiden","sana","sanki","sekiz","seksen","sen","senden","seni","senin","siz","sizden","sizi",
                 "sizin","sonra","sonradan","sonralarD1","sonunda","tabii","tam","tamam","tamamen","tamamD1yla","tarafD1ndan","tek",
                 "trilyon","tC<m","var","vardD1","vasD1tasD1yla","ve","velev","velhasD1l","velhasD1lD1kelam","veya","veyahut","ya","yahut",
                 "yakinen","yakD1nda","yakD1ndan","yakD1nlarda","yalnD1z","yalnD1zca","yani","yapacak","yapmak","yaptD1","yaptD1klarD1","yaptD1DD1",
                 "yaptD1DD1nD1","yapD1lan","yapD1lmasD1","yapD1yor","yedi","yeniden","yenilerde","yerine","yetmiC>","yetmiE","yetmiE#","yine","yirmi",
                 "yok","yoksa","yoluyla","yC<z","yC<zC<nden","zarfD1nda","zaten","zati","zira","C'abuk","C'abukC'a","C'eEitli","C'ok","C'oklarD1",
                 "C'oklarD1nca","C'okluk","C'oklukla","C'okC'a","C'oDu","C'oDun","C'oDunca","C'oDunlukla","C'C<nkC<","C6bC<r","C6bC<rkC<","C6bC<rC<","C6nce",
                 "C6nceden","C6nceleri","C6ncelikle","C6teki","C6tekisi","C6yle","C6ylece","C6ylelikle","C6ylemesine","C6z","C<zere","C<C'","bey",
                 "beyden","beyi","beyler","bu","C>una","bunda","bundan","bunu","Eayet","Eey","Eeyden","Eeyi","Eeyler","Eu","Euna",
                 "EuncacD1k","Eunda","Eundan","Eunlar","EunlarD1","Eunu","Eunun","Eura","EuracD1k","EuracD1kta","EurasD1","EC6yle",
                 "Eayet","Eimdi","Eu","EC6yle","bi","merhaba","teEekkC<rler","abi","cok","hocam","dili","dil","teEekkC<r","ederim",
                 "istiyorum","teEekkC<rler","abi","hocam","mi","bence","teEekkC<r","size","sizde","iC'in","fazla","evet","Eekilde",
                 "nedir","miyim","falan","sizce","mesela","dan","lazD1m","yada","arkadaElar","bence","bende","den","icin")

#ISTENMEYEN KELIMELERIMIZI CIKARIYORUZ
tr_veri1$textOriginal <- removeWords(tr_veri1$textOriginal, kelime_liste)
tr_veri1$textOriginal <- removeWords(tr_veri1$textOriginal, liste)

#TEMIZLEDIGIMIZ VERIYI KAYDEDELIM
write_xlsx(tr_veri1,"C:/Users/beyza/Desktop/tt_veri.xlsx")

#TEMIZ VERI R AKTARALIM
temiz_veri<- read_xlsx(file.choose())

t_1<- data.frame(text=temiz_veri$textOriginal)



tr_tveri1<- t_1%>% mutate(linenumber=row_number()) %>% unnest_tokens(word,text)
tr_tveri1

#BU ASAMADA "PLYR" KUTUPHANESI KULLANILAMMALI

# Kelime frekanslarD1nD1 hesaplama ve filtreleme
kelime_frekans <- tr_tveri1 %>%
  count(word, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  filter(!is.na(word)) %>%
  filter(n > 0)

#top20
kelime_frekans_top20 <- kelime_frekans %>%
  top_n(20, n)

# KELIME FREKANS GORSELLESTIRME
ggplot(kelime_frekans_top20, aes(x = word, y = n, fill = word)) +
  geom_col() +
  coord_flip() +
  labs(title = "En Yuksek Frekansa Sahip 20 Kelime", x = "Kelime", y = "Frekans") +
  theme_minimal()


# KELIME BULUTU ICIN GEREKLI PAKETLER
library(wordcloud)
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)


# KELIME BULUTU OLUSTURALIM
wordcloud(tr_tveri1$word,
          min.freq = 1,max.words = 100,
          colors = brewer.pal(6,"Dark2"),
          random.color = T,
          random.order = F)

#DUYGU ANALIZI

lexicon <- read.table(file.choose(),
                      header = TRUE,
                      sep = ';',
                      quote = "",
                      stringsAsFactors = FALSE)

#lexicon verisetindeki WORD ve POLARITY sutunlarCB1nCB1 alip
#word ve value olarak lexicon2 veriseti olusturur

lexicon2 <- lexicon %>%
  select(c("WORD","POLARITY")) %>%
  rename('word'="WORD", 'value'="POLARITY")
kutuphane<-temiz_veri

kutuphane %>%
  mutate(linenumber = row_number()) %>% #her satira bir numara ekler
  unnest_tokens(word, textOriginal) %>% #metinleri kelimelere ayirir
  inner_join(lexicon2) %>% #bu kelimeleri lexicon ile eslestirir
  group_by(linenumber) %>% #her satir icin duygu degerlerini toplar ve linenumber ile gruplandirir
  summarise(sentiment = sum(value)) %>% #sonuclari dosyaya yazdirir
  left_join(
    kutuphane %>%
      mutate(linenumber = row_number())
  ) %>% write_csv("sentiment_output.csv")

#sonuclari analiz etme ve gorsellestirme
yeni <- read_csv("sentiment_output.csv")

#duygu analizi sonuclarini degerlendirme
neutral <- length(which(yeni$sentiment == 0))
positive <- length(which(yeni$sentiment > 0))
negative <- length(which(yeni$sentiment < 0))

#toplam duygu sayisini hesaplama
toplam = positive + neutral + negative

#duygu kategorileri ve oranlari icin veri cercevesi olusturma
Sentiment <- c("Pozitif", "NC6tr", "Negatif")
Count <- c((positive/toplam)*100, (neutral/toplam)*100, (negative/toplam)*100)
output <- data.frame(Sentiment, Count)

#duygu kategorilerini factore donusturme
output$Sentiment <- factor(output$Sentiment, levels=Sentiment)

#bar plot ile duygu analizi oranlarini gorsellestirme
ggplot(output, aes(x=Sentiment, y=Count))+
  geom_bar(stat = "identity", aes(fill = Sentiment))+
  ggtitle("YorumlarD1n Duygu Analizinin OranlarD1")

#oranlarin head degerleri
head((positive/toplam)*100)
head((neutral/toplam)*100)
head((negative/toplam)*100)

#------------EMOJI ALANI-----------

tr_data2<-tr_data
#YORUMLARDA HANGD0 EMOJD0 KAC KERE KULLANILMIE  
emoji_sayisi <- table(unlist(str_extract_all(tr_data2$textOriginal, "[\\x{1F600}-\\x{1F64F}]|[\\x{1F300}-\\x{1F5FF}]|[\\x{1F680}-\\x{1F6FF}]")))
tr_emojiler<- as.matrix(emoji_sayisi)

names(tr_emojiler) <- "Emoji_1"

# YORUMLARDAKI EMOJI SAYISI
tr_emoji_sayisi <- table(unlist(str_extract_all(tr_data2$textOriginal, "[\\x{1F600}-\\x{1F64F}]|[\\x{1F300}-\\x{1F5FF}]|[\\x{1F680}-\\x{1F6FF}]")))

# TOPLAM KULLANILAN EMOJI SAYISI
tr_toplam_emoji_sayisi <- sum(emoji_sayisi)

# TOPLAM EMOJI SAYISI YAZDIRALIM
print(tr_toplam_emoji_sayisi)


# FONKSIYONU TANIMLAYALIM
tr_extract_emojis <- function(text) {
  # Text'in NULL veya NA olmadD1DD1ndan emin olun
  if (is.null(text) || is.na(text)) return(NA_character_)

# UNICODE ARALIGINA GORE EMOJI SECIMI
  tr_emojis <- str_extract_all(text, "[\\x{1F600}-\\x{1F64F}]|[\\x{1F300}-\\x{1F5FF}]|[\\x{1F680}-\\x{1F6FF}]|[\\x{1F700}-\\x{1F77F}]|[\\x{1F780}-\\x{1F7FF}]|[\\x{1F800}-\\x{1F8FF}]|[\\x{1F900}-\\x{1F9FF}]|[\\x{1FA00}-\\x{1FA6F}]|[\\x{1FA70}-\\x{1FAFF}]|[\\x{2600}-\\x{26FF}]|[\\x{2700}-\\x{27BF}]|[\\x{2300}-\\x{23FF}]|[\\x{2B50}-\\x{2B50}]|[\\x{1F004}-\\x{1F004}]|[\\x{1F0CF}-\\x{1F0CF}]|[\\x{1F18E}-\\x{1F18E}]|[\\x{1F191}-\\x{1F19A}]|[\\x{1F201}-\\x{1F201}]|[\\x{1F21A}-\\x{1F21A}]|[\\x{1F22F}-\\x{1F23A}]|[\\x{1F250}-\\x{1F251}]|[\\x{3297}-\\x{3297}]|[\\x{3299}-\\x{3299}]")
  return(unlist(tr_emojis))
}


#METIN VERISINDEKI EMOJILERI AYIKLAMA
tr_emo_emojis <- tr_data2 %>%
  mutate(tr_emojiler = map(textOriginal, tr_extract_emojis))

#DATA FRAME CEVIRME ISLEMI
tr_emoji_df<- data.frame(emoji = names(tr_emoji_sayisi), frequency = as.numeric(tr_emoji_sayisi))

#ILK 30 EMOJI BUL
top_30_emojiler_tr<- tr_emoji_df %>%
  arrange(desc(frequency)) %>%
  head(30)

#CUBUK GRAFIGI
ggplot(data = top_30_emojiler_tr, aes(x = factor(emoji, levels = emoji), y = frequency)) +
  geom_bar(fill="pink", stat = "identity") +
  geom_text(aes(label = frequency), vjust = -0.5, color = "red", size = 4) +  # Frekans sayD1laeD1
  labs(title = "En Cok KullanD1lan 30 Emoji", x = "Emoji", y = "KullanD1m SayD1sD1") +
  scale_x_discrete(labels = top_30_emojiler_tr$emoji) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(family = "EmojiOne", size = 25, color = "purple4"),
        axis.text.y = element_text(family = "EmojiOne", size = 20, color = "brown"),
        plot.title = element_text(color = "black", size = 25),
        axis.title.x = element_text(color = "black", size = 20),
        axis.title.y = element_text(color = "black", size = 20)) +
  coord_cartesian(ylim = c(0, max(top_30_emojiler_tr$frequency) * 1.1))



# Emojifont paketinden EmojiOne fontunu indiriyoruz
load.emojifont(font = "EmojiOne.ttf")

# KELIME BULUTU
wordcloud(words = emoji_df$emoji,
          freq = emoji_df$frequency,
          scale = c(6, 0.2),
          colors = brewer.pal(5, "Dark2"),
          family = "EmojiOne")


#EGER SIZE KISMINI 2 YAPARSANIZ ILK SIRADAKI EMOJIYI ALMIYOR
wordcloud2(data= top_30_emojiler_tr,
           size=1,
           backgroundColor = "pink",
           shape = "circle")



