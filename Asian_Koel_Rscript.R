#frequency range detection of Animal sound
##Eudynamys scolopaceus
install.packages("warbleR")    ##githubinstall("warbleR", ask = FALSE)
install.packages("tuneR")
install.packages("Rraven")
install.packages("devtools")
install.packages("knitr")
install.packages("magick")
library("warbleR")
library("tuneR")
library("Rraven")
library("devtools")
library("knitr")
library("magick")
library("raster")
##https://en.m.wikipedia.org/wiki/Dinagat_Islands

## set temporary working directory
setwd("C:/Users/PC/Desktop/MB2_PRESENTATION")
getwd()
##setwd("C:/Users/PC/Desktop/MB2FINAL")

##Query  xeno-canto for all recordings of ASIAN KOEL
Asian<- querxc(qword="Eudynamys", download= FALSE)

##check out the structure of resulting the data frame
str(Asian)

##Query xeno-canto for all Eudynamys scolopaceus recordings
Asian.koel<-querxc(qword="Eudynamys scolopaceus", download = FALSE)

##Check out the structure of resulting the data frame
str(Asian.koel)

#search and query and get the numbers and how the species are spread accross
X <- quer_xc("Eudynamys scolopaceus", download = FALSE) ##Eudynamys-scolopaceus
View(X)
xcmaps(X)
xcmaps(X, img = FALSE, it = "jpeg") # or in tiff format

##Read image xcmaps from Directory with magick function
xcmaps<- image_read("C:/Users/PC/Desktop/MB2_PRESENTATION/Map of Eudynamys scolopaceus recordings.jpeg")
print(xcmaps)


##Query to get the number of signal that exist in xeno-canto for Asian-koel
levels(Asian.koel$Vocalization_type)

##Query to get the number of recordings per signal type
table(Asian.koel$Vocalization_type)

## Query meta data to select the best signal using using three filter; first by quality
# Quality
Asian.koel<-Asian.koel[Asian.koel$Quality=="A",]
nrow(Asian.koel)

##Second by signal type
Asian.koel.call<-Asian.koel[grep("call", Asian.koel$Vocalization_type, ignore.case = TRUE), ]
nrow(Asian.koel.call)

###Third by locality
Asian.koel.LS<-Asian.koel.call[grep("Dinigat Island",Asian.koel.call$Locality, ignore.case = FALSE),]

##Check resulting data frame
str(Asian.koel.LS)

###come back and use call to filter not by song

querxc(X=Asian.koel.LS)


# Query and download  Xeno-Canto for metadata using genus and species as keywords 
##asian.koel <- quer_xc(qword = "nr:446387", download = TRUE, pb = FALSE)

# Convert mp3 to wav format

r <- readMP3("Eudynamys-scolopaceus-446387.mp3")  ## MP3 file in working directory
writeWave(r,"tmp.wav",extensible=FALSE)

# Simultaneously lower sampling rate to speed up down stream analyses
mp32wav(samp.rate = 22.05, pb = FALSE)


ad <- auto_detec(wl = 200, threshold = 3.5, ssmooth = 1200, bp = c(4, 9.6), 
                 pb = FALSE,  mindur = 0.1, maxdur = 0.25, img = FALSE)

snr <- sig2noise(ad, pb = FALSE, mar = 0.05)

ad <- snr[rank(-snr$SNR) < 60, ]
# A look at the subset of signals to be analyzed
catalog(ad, nrow = 5, ncol = 4, mar = 0.01, flim = c(3.5, 10.5), 
        labels = "selec", pal = reverse.heat.colors, pb = FALSE)

open_wd()

##Read image catalog1 from Directory with magick function
catalog1<- image_read("C:/Users/PC/Desktop/MB2_PRESENTATION/Catalog_p1.jpeg")
print(catalog1)


##Read image catalog2 from Directory with magick function
catalog2<- image_read("C:/Users/PC/Desktop/MB2_PRESENTATION/Catalog_p2.jpeg")
print(catalog2)


#frequent range detection for selection 2 only
w2 <- readWave(as.character(ad$sound.files[2]), from = ad$start[2], 
               to = ad$end[2], units = "seconds")

freq_range_detec(w2, bp = c(2, 9.6), fsmooth = 1, ovlp = 95, 
                 wl = 200, threshold = 8)

#frequency range detection for all selection
for(i in 1:nrow(ad))
{
  wv <- readWave(as.character(ad$sound.files[i]), from = ad$start[i], 
                 to = ad$end[i], units = "seconds")
  
  freq_range_detec(wv, bp = c(2, 12), fsmooth = 1, ovlp = 95, wl = 200, 
                   threshold = 8, flim = c(0, 13), 
                   main = paste("selection", i))
  
  Sys.sleep(0.8)
}

fr_ad <- freq_range(X = ad, bp = c(2, 12), fsmooth = 0.001, ovlp = 95, 
                    wl = 200, threshold = 10, img = FALSE)

View(fr_ad)


# cHECKING nUMBER OF rOws that have NA values

row.has.na <- apply(fr_ad, 1, function(x){any(is.na(x))})
#summation of number of rows with NA values
sum(row.has.na)

#dropping of rows with NA values
fr_ad.filtered <- fr_ad[!row.has.na,]

#using the range as Band pass filter
sp <- specan(X = fr_ad.filtered, wl = 200, bp = "frange", ovlp = 90, pb = FALSE, 
             threshold = 15)

View(sp)

#Final
write.csv(sp, "C:/Users/PC/Desktop/MB2_PRESENTATION/sp.csv")

write.csv(fr_ad, "C:/Users/PC/Desktop/MB2_PRESENTATION/fr_ad.csv")

write.csv(X, "C:/Users/PC/Desktop/MB2_PRESENTATION/X.csv")
