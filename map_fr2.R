#############################
### .fr data
### Data source: Afnic
#############################

# Help
#http://stackoverflow.com/questions/17723822/administrative-regions-map-of-a-country-with-ggmap-and-ggplot2
#https://www.students.ncl.ac.uk/keith.newman/r/maps-in-r-using-gadm
#http://www.r-bloggers.com/plotting-maps-with-r/
#http://www.kevjohnson.org/making-maps-in-r/

library(data.table)
library(classInt)
#library(maptools)
library(scales)
library(ggplot2)
library(ggmap)

# Read data
filename<-"201511_OPENDATA_A-NomsDeDomaineEnPointFr.csv" #Downloaded from http://www.afnic.fr
DT<-as.data.table(read.csv2(paste0("Data/", filename), header=TRUE))

FR_adm2<-readRDS("Data/FRA_adm2.rds") #Downloaded from http://www.gadm.org/

# Clean up data
setnames(DT, "Departement.titulaire", "regioncode")

# Prepare data set
DT_count<-DT[, .(count=.N), by=regioncode] #Number of domains per region
rm(DT)
DT_count[, quota:=round((count/sum(count)),4)] #Percentage of domains per region
DT_count[, regioncode:=as.character(regioncode)]
mapping<-data.table(regioncode=FR_adm2@data$CCA_2,  #Table for mapping regions names with codes 
                    regionname=FR_adm2@data$NAME_2, #Needed as afnic works with codes
                    regionpos=FR_adm2@data$ID_2)    #And map works with position 
setkey(DT_count,  regioncode)
setkey(mapping,  regioncode)
DT_count<-DT_count[mapping, nomatch=0] #Join tables

# Prepare map
FR2<-fortify(FR_adm2)
FR2$id<-as.integer(FR2$id)
FR2<-as.data.table(FR2)
setkey(FR2, id)

setnames(DT_count, "regionpos", "id")
setkey(DT_count, id) 

plotData<-FR2[DT_count]

# Create map
p <- ggplot() +
        geom_polygon(data = plotData, aes(x = long, y = lat, group = id,
                                          fill = quota), color = NA , size = 0.25) +
        coord_map() +
        scale_fill_distiller(palette = "Greens", labels = percent,
                             breaks = pretty_breaks(n = 10)) +
        guides(fill = guide_legend(reverse = TRUE)) +
        theme_nothing(legend = TRUE) +
        labs(title = "Percentage of .fr domains per region (Nov15)", fill = "")
p

# Save map
ggsave(p, file = "fr_domains.png", width = 6, height = 4.5, type = "cairo-png") +
        coord_map()
