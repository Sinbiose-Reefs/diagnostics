
# diagnostics of reef fisheries in Brazil



# load packages

require(here)
require(openxlsx)




# load fisheries data (Freire et al. 2021)

fisheries <- read.xlsx (here ("data", "FINAL_RECONSTRUCTED_Brazil_1950_2015_CommercialEtapaII_04072021_IP_Freire.xlsx"),
                        sheet = 2)

fisheries$Sector [which(fisheries$Sector == "industrial (LS, C)")] <- "Industrial (LS, C)"

# load data of Pinheiro et al. 2018 (BR reef fish)

reef_fish <- read.csv (here ("data","brazilian-reef-fish-table-04-mar-18-website.xlsx - Database.csv"))
reef_fish<-reef_fish[which(reef_fish$Relation == "RES"),] # REef fish  (RESident fish)



# trait data (GASPAR database)

traits <- read.xlsx (here ("data", "GASPAR_Data-1.xlsx"),
                     sheet = 3)

# ajusts  
traits$Depth_max<-as.numeric(gsub (",",".",traits$Depth_max))
traits$Depth_min<-as.numeric(gsub (",",".",traits$Depth_min))
# average depth
traits$Depth_mean <- rowMeans(cbind (traits$Depth_max,
                            
                                      traits$Depth_min),
                              na.rm=T)



# fisheries ~year
require(dplyr)

# genus
genus <- strsplit(fisheries$TaxonName," ")
genus <- do.call(rbind, genus)
fisheries$Genus_match <- genus[,1]



## matching with nutrients (predictions from Hicks et al. 2019)

nutrients <- read.csv (here ("data", "Species_Nutrient_Predictions.csv"))


#  genus
nutrients$Genus <- sapply(strsplit (nutrients$species, "_"), "[",1)



# bind nutrient content to the trait dataset
traits <- cbind (traits, 
                 nutrients[match (traits$Genus,
                                  
                                  nutrients$Genus),])




# fisheries data (matching with Pinheiro et al. 2018)
# reef fish genus

table(unique(fisheries$Genus_match) %in% reef_fish$Genus )



fisheries_wtrait<-fisheries[fisheries$Genus_match %in% reef_fish$Genus,]




# match trait & fisheries
fisheries_wtrait <- cbind (fisheries_wtrait,
                           
                           traits [match (fisheries_wtrait$Genus,traits$Genus ),]
                           
                           )







table(fisheries_wtrait[,27] == fisheries_wtrait[,78])

# ---------------------------
# plotting
# catch year
catch_year <- fisheries_wtrait [,-78]%>%  # 78 is the column with a second name 'genus'
  
  group_by(Year,Sector,Region) %>% 
  
  summarize(sum_catch=sum(CatchAmount_t),
  ) 



# 

require(ggplot2)
require(ggrepel)
catch_year_plot <- ggplot (catch_year, aes (x=Year, 
                         y=sum_catch,
                         colour = Sector)) + 
  facet_wrap(~Region,scales = "free")+
  geom_line(size=1) + 
  scale_fill_viridis_d(option ="viridis", begin = 0.3,end=0.8) + 
  theme_classic() + 
  scale_colour_viridis_d(option ="viridis", begin = 0.3,end=0.8) + 
  theme (legend.position = c(0.7,0.88),
         axis.title = element_text(size=15)) + 
  ylab ("Sum of the Catch Amount (T)")



# ========================
# a pesca mudou ao longo do tempo?


# filter reef fish

fisheries_wtrait<-fisheries_wtrait[which(fisheries_wtrait$Genus %in% 
                                           reef_fish$Genus),]


# reshape
require(reshape)

year_composition <- cast (fisheries_wtrait, 
                          
                          formula = Year ~ Genus, 
      
                        value = "CatchAmount_t",
                        fun.aggregate = sum
        )



# vegan:: beta diversity between year
#  hellinger transformed dataset
require(vegan)
dist_composition <- vegdist (decostand(year_composition,'hell'),
                                method = "bray",na.rm=T)



# pcoa
require(ape)
pcoa_fish_year <- pcoa(dist_composition)
# variance explained by the first and second axes
Exp_axis1<-pcoa_fish_year$values$Eigenvalues[1]/sum(pcoa_fish_year$values$Eigenvalues)*100
Exp_axis2<-pcoa_fish_year$values$Eigenvalues[2]/sum(pcoa_fish_year$values$Eigenvalues)*100


#pcoa_fish_year <- melt (pcoa_fish_year)
pcoa_fish_year <- cbind (pcoa_fish_year$vectors,
                         year = year_composition$Year)

# dataframe with data
pcoa_fish_year<-as.data.frame(pcoa_fish_year)

# ordination (projection of beta diversity )
# help here
# https://ggplot2.tidyverse.org/reference/geom_path.html
ordination1<-ggplot(data=pcoa_fish_year,
       aes(x=Axis.1,y=Axis.2)) + 
  geom_point(aes(colour=as.numeric(year)),shape=1,size=3) + # add the point markers
   geom_path(aes(colour=as.numeric(year)),alpha=0.5)+

  geom_text(aes(label=year),
                size=2.5,vjust=-1) +
  #geom_path(aess(group=year)) +# add the site labels
  #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw() +
  theme(legend.position = "none")



# correlation of genus to the axes

correlation <-data.frame( cor (decostand(year_composition,'hell'),
                    pcoa_fish_year[,1:2],
                    method = "pearson"))

rownames(correlation) <- colnames(year_composition[,-1])
correlation$species <- rownames(correlation)

# order

correlation[order(correlation[,1],decreasing=T),][1:10,]
correlation[order(correlation[,1],decreasing=F),][1:9,]
correlation[order(correlation[,2],decreasing=T),][1:3,]



# project genus names
ordination1<-ordination1 + geom_text_repel(data = correlation[order(correlation[,2],decreasing=T),][1:3,],
                        aes (x=Axis.1*0.2,
                             y = Axis.2*0.2,
                             label = (species)),
                        size=3,fontface = "italic",
                        colour="#1363DF",
                        max.overlaps = 100) +
  geom_text_repel(data = correlation[order(correlation[,1],decreasing=F),][1:9,],
            aes (x=Axis.1*0.2,
                 y = Axis.2*0.2,
                 label = (species)),
            size=3,fontface = "italic",
            colour = "#06283D",
            max.overlaps = 100) + 
  geom_text_repel(data = correlation[order(correlation[,1],decreasing=T),][1:16,],
            aes (x=Axis.1*0.22,
                 y = Axis.2*0.22,
                 label = (species)),
            size=3,fontface = "italic",
            colour = "#47B5FF",
            max.overlaps = 100) + 
  xlab (paste ("Axis 1 (", round(Exp_axis1,2), "%)",sep="")) +
  ylab (paste ("Axis 2 (", round(Exp_axis2,2), "%)",sep=""))


## =============================================== 
# change in composition per region
# a pesca mudou ao longo do tempo?

require(reshape)
year_composition <- lapply (unique(fisheries_wtrait$Region), function (i)
  
  cast (fisheries_wtrait[which(fisheries_wtrait$Region == i),], 
        
        formula = Year ~ Genus, # Genus
        
        value = "CatchAmount_t",
        fun.aggregate = sum
  )
)



## FAMILIES
library(tidyverse)

# vegan beta diversity
require(vegan)
dist_composition_pcoa <- lapply (year_composition, function (i){
  
  
  dist_composition <- vegdist (decostand(i,'hell'),
                               method = "bray",na.rm=T)
  
  # pcoa
  pcoa_fish_year <- pcoa(dist_composition)
  ;
  pcoa_fish_year
})


# composition per region
comp_change <- bind_rows(pcoa_fish_year %>% 
                           bind_cols(region = "Brazil", Year = unique(fisheries_wtrait$Year)),
                          dist_composition_pcoa[[1]]$vectors %>% 
                           bind_cols(region = "Norte", Year = unique(fisheries_wtrait$Year)),
                         dist_composition_pcoa[[2]]$vectors %>% 
                           bind_cols(region = "NE", Year = unique(fisheries_wtrait$Year)),
                         dist_composition_pcoa[[3]]$vectors %>% 
                           bind_cols(region = "SE", Year = unique(fisheries_wtrait$Year)),
                         dist_composition_pcoa[[4]]$vectors %>% 
                           bind_cols(region = "Sul", Year = unique(fisheries_wtrait$Year))) %>% 
  data.frame() %>% dplyr::rename(change = Axis.1) %>% 
  ggplot(aes(x = Year, y = change)) +
  geom_point() +
  facet_wrap(~ region, ncol = 5) +
  theme_classic() +
  geom_smooth() +
  labs(x = "", y = "Change in catch\ncomposition (Beta)")


# arrange plots
require(gridExtra)

compostion1<-grid.arrange(ordination1,
             comp_change,
             ncol=5,nrow=6,
             layout_matrix = rbind (c (1,1,1,1,1),
                                    c (1,1,1,1,1),
                                    c (1,1,1,1,1),
                                    c (1,1,1,1,1),
                                    c (2,2,2,2,2),
                                    c (2,2,2,2,2)))

# ============================

# average trait per genus
size_genus <- tapply (traits$Size,
                      list (traits$Genus),
                      mean,na.rm=T)

# depth
depth_genus <- tapply (as.numeric(gsub (",",".",traits$Depth_mean)),
                       list (traits$Genus),
                       mean,na.rm=T)




# going deeper in depth and food chain?

nsp_choose <- 20 #  n species to choose (among the ranked spp)


# separate data per year (it will be used several times)
# using data already filtered for reef fish species

fish_year <- split (fisheries_wtrait, fisheries_wtrait$Year)




# size and depth
fish_year_size_depth <- lapply (fish_year, function (i) {
  
  
  fish_year_genus <- tapply (i$CatchAmount_t,
                             list (i$Genus),
                             sum)
  # Mean size among the most frequent nsp_choose 
  size_collect_species <- names (fish_year_genus[order(fish_year_genus,decreasing=T)][1:nsp_choose])
  size_collect_species <- size_genus[which(names (size_genus) %in% size_collect_species)]
  mean_size_collect_species<- mean(size_collect_species)
  # mean depth of the most frequent nsp
  # five more frequentt secies
  depth_collect_species <- names (fish_year_genus[order(fish_year_genus,decreasing=T)][1:nsp_choose])
  depth_collect_species <- depth_genus[which(names (depth_genus) %in% depth_collect_species)]
  mean_depth_collect_species<- mean(depth_collect_species,na.rm=T)
  
  
  # bind 
  averages_size_depth <- data.frame (size= mean_size_collect_species,
                                     depth = mean_depth_collect_species)
  ; # return
  averages_size_depth
  
  
})

# dataframe
fish_year_df <- data.frame(size = do.call(rbind,fish_year_size_depth))
fish_year_df$year <- rownames(fish_year_df)


# melt
fish_year_df <- melt (fish_year_df, id.var = "year")
levels(fish_year_df$variable)[which(levels(fish_year_df$variable) == "size.size")] <- "Size" 
levels(fish_year_df$variable)[which(levels(fish_year_df$variable) == "size.depth")] <- "Depth" 

# 
plot_size_depth<-ggplot (fish_year_df, aes (x=year, y=value,group=variable,colour=variable)) + 
  
  geom_point() + 
  
  geom_smooth(method = "gam") + 

  xlab ("Year") + theme_classic()+
  
  scale_colour_viridis_d(option="viridis", begin = 0.2,end=0.7)+
  # Add a second axis and specify its features
  # Custom the Y scales:
  scale_y_continuous(
    
    # Features of the first axis
    name = "Size (cm)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~., name="Depth (m)")
  ) + 
  
  scale_x_discrete(
  
      breaks = seq(min(fish_year_df$year), 
                 max(fish_year_df$year), by = 10)
      
      ) +
  theme(axis.text.x = element_text(angle=0,size=8),
        legend.position = c(0.8,0.1),
        legend.text = element_text(size=13),
        axis.title = element_text(size=15),
        legend.title = element_blank()) 
  




# Are fisheries getting nutritionally poorer over time?
# zinc

zinc_genus <- tapply (fisheries_wtrait$Zinc_mu,
                       list (fisheries_wtrait$Genus,
                             fisheries_wtrait$Region),
                       mean,na.rm=T)

# iron
iron_genus <- tapply (fisheries_wtrait$Iron_mu,
                      list (fisheries_wtrait$Genus,
                            fisheries_wtrait$Region),
                      mean,na.rm=T)

# omega 3
omega_genus <- tapply (fisheries_wtrait$Omega_3_mu,
                      list (fisheries_wtrait$Genus,
                            fisheries_wtrait$Region),
                      mean,na.rm=T)

# protein
protein_genus <- tapply (fisheries_wtrait$Protein_mu,
                      list (fisheries_wtrait$Genus,
                            fisheries_wtrait$Region),
                      mean,na.rm=T)

# calcium
calcium_genus <- tapply (fisheries_wtrait$Calcium_mu,
                         list (fisheries_wtrait$Genus,
                               fisheries_wtrait$Region),
                         mean,na.rm=T)

# selenium
selenium_genus <- tapply (fisheries_wtrait$Selenium_mu,
                         list (fisheries_wtrait$Genus,
                               fisheries_wtrait$Region),
                         mean,na.rm=T)

# vitA
vitA_genus <- tapply (fisheries_wtrait$Vitamin_A_mu,
                          list (fisheries_wtrait$Genus,
                                fisheries_wtrait$Region),
                          mean,na.rm=T)

# list of nutrient data
nutrient_data <- list (zinc_genus,iron_genus,omega_genus,protein_genus,
                       calcium_genus,selenium_genus , vitA_genus)
# extract data
fish_year_nutrition <- lapply (fish_year, function (i) 
  do.call(rbind, lapply (nutrient_data, function (k)  { # bind nutrient data
  
  
  fish_year_genus <- tapply (i$CatchAmount_t,
                             list (i$Genus,
                                   i$Region),
                             sum)
  
  # Mean size among the most frequent nsp_choose 
  # across regions
  nut_reg <- do.call(cbind,   # melt nutrient data per region
    
    lapply (seq (1,ncol (fish_year_genus)), function (reg) {
  
        nut_collect_species <- names (fish_year_genus[,reg])[order(fish_year_genus[,reg],decreasing=T)][1:nsp_choose]
        nut_collect_species <- k[which(names (k[,reg]) %in% nut_collect_species),reg]
        mean_nut_collect_species<- mean(nut_collect_species,na.rm=T)
        mean_nut_collect_species
  }))
  colnames(nut_reg) <- colnames(fish_year_genus)
  ; # return
  nut_reg
  
  
})))
# name rows
fish_year_nutrition<- lapply (fish_year_nutrition, function (i) {
  rownames (i)<- c("Zinc","Iron", "Omega-3", "Protein","Calcium","Selenium","Vitamin-A")
  ;
  i
})
# name year
names (fish_year_nutrition) <- names(fish_year)
# melt
fish_year_nutrition <-( do.call(rbind.data.frame, fish_year_nutrition))
fish_year_nutrition$year <- sapply (strsplit ( rownames(fish_year_nutrition), "\\."), "[",1)
fish_year_nutrition$nutrient <- sapply (strsplit ( rownames(fish_year_nutrition), "\\."), "[",2)
# melt to fit ggplot format
fish_year_nutrition<- melt(fish_year_nutrition,id.var=c("year", "nutrient"))

# plot 

plot_nut <- ggplot (fish_year_nutrition, aes (x=year, 
                                  y=value,
                                  group=variable,
                                  colour=variable)) + 
  geom_point()+
  facet_wrap(~nutrient,scales = "free_y",ncol=7)+
  geom_smooth() +
  scale_x_discrete(
    
    breaks = seq(min(fish_year_nutrition$year), 
                 max(fish_year_nutrition$year), by = 20),
    
  ) + 
  ylab ("Content of nutrients") + 
  xlab("Year")+
  theme_classic() + 
  theme(axis.text = element_text(size=5),
        axis.title = element_text(size=13),
        strip.text = element_text(face="bold")) + 
  scale_colour_viridis_d()


plot_nut

# ordination to show the nutrition content of fish genus
genus_nutrient_composition <- lapply (nutrient_data, function (i) 
  
          apply (i,1, mean,na.rm=T)
          
)
genus_nutrient_composition <- do.call(cbind,genus_nutrient_composition)
colnames(genus_nutrient_composition) <- c("Zinc","Iron", "Omega-3", "Protein","Calcium","Selenium","Vitamin-A")
# removing missing data
genus_nutrient_composition<-genus_nutrient_composition[which(rowSums(genus_nutrient_composition>0,na.rm=T)>0),]
genus_data <- rownames(genus_nutrient_composition)
# standardize values
genus_nutrient_composition<-apply (genus_nutrient_composition, 2,scale)
rownames(genus_nutrient_composition) <- genus_data

# distance matrix
dist_nut <- vegdist (genus_nutrient_composition, "euclidean")

# ordination
pcoa_nut <- pcoa(dist_nut)

# variance explained by the first and second axes
Exp_axis1<-pcoa_nut$values$Eigenvalues[1]/sum(pcoa_nut$values$Eigenvalues)*100
Exp_axis2<-pcoa_nut$values$Eigenvalues[2]/sum(pcoa_nut$values$Eigenvalues)*100

# data

#pcoa_fish_year <- melt (pcoa_fish_year)
pcoa_nut <- data.frame (pcoa_nut$vectors[,1:2],
                   sp = rownames(pcoa_nut$vectors))


# corelation to project nutrients
correlation_nut <-data.frame( cor (genus_nutrient_composition,
                                   pcoa_nut[,1:2],
                               method = "pearson"))
correlation_nut$nutrient <- rownames(correlation_nut)
# ordination
# help here
# https://ggplot2.tidyverse.org/reference/geom_path.html
ordination_nut<-ggplot(data=pcoa_nut,
                    aes(x=Axis.1,y=Axis.2)) + 
  geom_point(colour="black",alpha=0.5,stroke=1.5,
             shape=1,size=3) + # add the point markers
  #geom_path(aes(colour=as.numeric(sp)),alpha=0.5)+
  
  geom_text_repel(aes(label=sp ),size=3,vjust=1,
                  max.overlaps = 50)+#ifelse(Axis.1>1.5 |
                            #   Axis.1 < -1 |
                            #   Axis.2 >1 |
                            #   Axis.2 < -2,as.character(sp),'')),
            #size=2.5,vjust=1) +
  #geom_path(aess(group=year)) +# add the site labels
  #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw() +
  theme(legend.position = "none")




# project genus names
ordination_nut<-ordination_nut + 
  
                    geom_text_repel(data = correlation_nut,
                                           aes (x=Axis.1*2,
                                                y = Axis.2*2,
                                                label = (nutrient)),
                                           size=5,fontface = "italic",
                                           colour="#1363DF",
                                           max.overlaps = 100)  + 
  xlab (paste ("Axis 1 (", round(Exp_axis1,2), "%)",sep="")) +
  ylab (paste ("Axis 2 (", round(Exp_axis2,2), "%)",sep=""))

ordination_nut


# arrange

composition2<-grid.arrange(ordination_nut,
             plot_nut,
             ncol=5,nrow=7,
             layout_matrix = rbind (c (1,1,1,1,1),
                                    c (1,1,1,1,1),
                                    c (1,1,1,1,1),
                                    c (1,1,1,1,1),
                                    c (1,1,1,1,1),
                                    c (2,2,2,2,2),
                                    c (2,2,2,2,2)))

# ======================

# 
#source (here ("R", "function_poncho.R"))
#
#pdf("year.pdf")
#poncho (log(year_composition+1),
#        gradient = seq(1,nrow(year_composition)),
#        col = as.numeric(as.factor(fisheries_wtrait$Diet_2012[match (colnames(year_composition),
#                                                fisheries_wtrait$Genus)])),lty = 0)
#
#dev.off()
#


# ===========================
# tRAIT COMSPOTION


TL_fisheries <- tapply (fisheries_wtrait$CatchAmount_t,
        list(fisheries_wtrait$Year,
             fisheries_wtrait$Diet_2012,
             fisheries_wtrait$Region),
        sum, default = 0) 

TL_fisheries<-melt (TL_fisheries)
colnames(TL_fisheries) <- c("Year", "TrGroup", "Region", "value")

## pot trnds over time
ggplot (TL_fisheries, aes (x=Year, y=value,
                     colour = TrGroup)) + 
  facet_wrap(~Region,scales = "free")+
  geom_line(size=1) + 
  xlab ("Year") + 
  ylab ("Sum catch (thousands of tonnes)") + 
  theme_classic() + 
  scale_colour_viridis_d(option = "viridis")


## ======================================
# herbivorous over time



selected_fish_trend <- tapply (fisheries_wtrait$CatchAmount_t,
                  list(fisheries_wtrait$Year,
                       #fisheries_wtrait$Diet_2012,
                       fisheries_wtrait$Region,
                       fisheries_wtrait$Genus),
                  sum) 

selected_fish_trend<-melt (selected_fish_trend)
colnames(selected_fish_trend) <- c("Year",  "Region", "Genus","value")

# plot
ggplot (selected_fish_trend[which(selected_fish_trend$Genus %in% 
                      c("Scarus", "Sparisoma",
                        "Acanthurus", 
                        "Lutjanus",
                        "Mycteroperca",
                        "Epinephelus",
                        "Ocyurus")),],
        
        
        aes (x=Year, y=log(value),
             colour = Genus),alpha=0.5) + 
  facet_wrap(~Region,scales = "free")+
  geom_line(size=1) + 
  xlab ("Year") + 
  ylab ("Sum catch (thousands of tonnes)") + 
  theme_classic()  +
  scale_colour_viridis_d(option ="viridis", begin=0.1,end=0.9)
  



## --------------------
# trends
# species per region and year
# + current
# regional scale




agg1<-aggregate(CatchAmount_t~Region+Genus+Year,sum,data=fisheries_wtrait)

head(agg1)

# poison smooth 
poison_smooth <- function(...) {
  geom_smooth(method = "gam", 
              method.args = list(family = "negbin(1)"),...)
  # geom_smooth(method = "glm", method.args = list(family = "poisson"), ...) # glm option
}

ggplot(agg1[which(agg1$Genus %in% 
                    c("Scarus", "Sparisoma",
                     "Acanthurus", "Lutjanus",
                     "Mycteroperca",
                     "Epinephelus",
                     "Ocyurus")),],
       aes (x=Year, y=sqrt(CatchAmount_t),colour = Genus))+
  geom_point() + 
  facet_wrap(~Region,scales = "free")+
  poison_smooth(formula = y ~ s(x, bs = "cs",k=3)) + theme_classic() + 
  scale_colour_viridis_d(option = "viridis", begin=0.1,end=0.9)
  










