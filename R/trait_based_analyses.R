

# load functions and packages
source ("R/functions.R")
source ("R/packages.R")
source ("R/quality_funct_space_fromdist2.R")

# load data
load (here ("data", "fisheries_wtrait.R"))

# ----------------------------------------------------------------
# how do fisheries changed in terms of functional composition?


# trait composition

average_traits <- fisheries_wtrait %>% 
  
  
  group_by(Genus_match) %>% 
  
  summarise (Body_size = mean (Body_size,na.rm=T),  
             Trophic_level = mean (Trophic_level,na.rm=T), 
             Depth_range = mean (Depth_range,na.rm=T),
             Depth_mean = mean (Depth_mean,na.rm=T),
             Depth_max = mean (Depth_max,na.rm=T), 
             Depth_min = mean (Depth_min,na.rm=T),
             #Diel_activity = getmode (Diel_activity), 
             Size_group = getmode (Size_group), 
             Level_water = getmode (Level_water))  %>%
  
  
  dplyr::select (Genus_match, Body_size, Trophic_level, 
                 Depth_min,Depth_max,Depth_range,
                 Depth_mean,# Diel_activity, 
                 Size_group, Level_water)




# transform categorical into ranking traits
# group size
average_traits$Size_group [which(average_traits$Size_group == "sol")] <- 1
average_traits$Size_group [which(average_traits$Size_group == "pair")] <- 2
average_traits$Size_group [which(average_traits$Size_group == "smallg")] <- 3
average_traits$Size_group [which(average_traits$Size_group == "medg")] <- 4
average_traits$Size_group [which(average_traits$Size_group == "largeg")] <- 5
average_traits$Size_group <- ordered (average_traits$Size_group) # ordered


# level water
average_traits$Level_water [which(average_traits$Level_water == "bottom")] <- 3
average_traits$Level_water [which(average_traits$Level_water == "low")] <- 2
average_traits$Level_water [which(average_traits$Level_water == "high")] <- 1
average_traits$Level_water <- ordered (average_traits$Level_water) # ordered


# interaction (if missing data in gorup size, keep at least the water level)
average_traits$Interaction.Level.Group <- ifelse (is.na(as.numeric(average_traits$Level_water) * as.numeric(average_traits$Size_group)),
                                                  as.numeric(average_traits$Level_water),
                                                  as.numeric(average_traits$Level_water) * as.numeric(average_traits$Size_group))

# daily activity
#average_traits$Diel_activity <- ifelse (average_traits$Diel_activity %in% c("night", "both"),
#                                        1,0)
#average_traits<- data.frame(average_traits)
#rownames (average_traits) <- average_traits$Genus_match
#average_traits<- average_traits[,-1]


# set rownames
average_traits <- as.data.frame (average_traits)
rownames(average_traits) <- average_traits$Genus_match

# functional composition  
# organize data
organized_data <- organize.syncsa(
  
  decostand(year_composition,'hell', na.rm=T),
  average_traits[,-which(colnames(average_traits) %in% c ("Genus_match",
                                                          #"Diel_activity",
                                                          "Level_water", 
                                                          "Size_group",
                                                          "Depth_range",
                                                          "Depth_mean"))])

# functional compositionw
funct_comp <- matrix.t(organized_data$community, 
                       organized_data$traits, 
                       scale = F, 
                       ranks = F, 
                       notification = TRUE)


# correlation of overall trends and traits
data_funct_trend <- data.frame (funct_comp$matrix.T,
                                beta = pcoa_fish_year$Axis.1,
                                year = rownames(pcoa_fish_year))

data_funct_trend <- melt (data_funct_trend,id.vars = c("year","beta")) # melt

# save plot
pdf (here ("output", "composition_funct.pdf"),width=7,height=6)

ggplot (data_funct_trend, aes (x=beta, y = value)) +
  geom_point(alpha=0.5,col = "turquoise3") + 
  facet_wrap(~variable, scale = "free_y") +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs",k=4),col ="turquoise3") + 
  ylab ("Community Weighted Means of Trait Values") + 
  xlab ("First PCoA Axis") + 
  theme (strip.text.x = element_text(size = 10, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         )) + 
  geom_text_repel(data = data_funct_trend[which(data_funct_trend$year %in% 
                                                  seq(1950,2020,10)),],
                  aes (label = year),size=3)+
  theme_bw()

dev.off()




# ============================

# average trait per genus

size_genus <- tapply (traits$Body_size,
                      list (traits$Genus),
                      mean,na.rm=T)


# depth
depth_genus_max <- tapply (traits$Depth_max,
                           list (traits$Genus),
                           mean,na.rm=T)

# depth
depth_genus_min <- tapply (traits$Depth_min,
                           list (traits$Genus),
                           mean,na.rm=T)

# TL
TL_genus <- tapply (traits$Trophic_level,
                    list (traits$Genus),
                    mean,na.rm=T)


# going deeper in depth and food chain?

nsp_choose <- 15 #  n species to choose (among the ranked spp)


# separate data per year (it will be used several times)
# using data already filtered for reef fish species

fish_year <- split (fisheries_wtrait, fisheries_wtrait$Year)



# size and depth
fish_year_size_depth_TL <- lapply (fish_year, function (i) {
  
  
  fish_year_genus <- tapply (i$CatchAmount_t,
                             list (i$Genus_match),
                             sum)
  # Mean size among the most frequent nsp_choose 
  size_collect_species <- names (fish_year_genus[order(fish_year_genus,decreasing=T)][1:nsp_choose])
  size_collect_species <- size_genus[which(names (size_genus) %in% size_collect_species)]
  mean_size_collect_species<- mean(size_collect_species,na.rm=T)
  
  # max depth of the most frequent nsp
  depth_collect_species_max <- names (fish_year_genus[order(fish_year_genus,decreasing=T)][1:nsp_choose])
  depth_collect_species_max <- depth_genus_max [which(names (depth_genus_max ) %in% depth_collect_species_max)]
  max_depth_collect_species<- mean(depth_collect_species_max,na.rm=T)
  
  # min depth of the most frequent nsp
  depth_collect_species_min <- names (fish_year_genus[order(fish_year_genus,decreasing=T)][1:nsp_choose])
  depth_collect_species_min <- depth_genus_min[which(names (depth_genus_min) %in% depth_collect_species_min)]
  min_depth_collect_species<- mean(depth_collect_species_min,na.rm=T)
  
  # TL
  TL_collect_species <- names (fish_year_genus[order(fish_year_genus,decreasing=T)][1:nsp_choose])
  TL_collect_species <- TL_genus[which(names (TL_genus) %in% TL_collect_species)]
  mean_TL_collect_species<- mean(TL_collect_species,na.rm=T)
  
  
  # bind 
  averages_size_depth_TL <- data.frame (size= mean_size_collect_species,
                                        depth.max = max_depth_collect_species,
                                        depth.min = min_depth_collect_species,
                                        TL = mean_TL_collect_species)
  ; # return
  averages_size_depth_TL
  
  
})



# dataframe
fish_year_df <- data.frame(size = do.call(rbind,fish_year_size_depth_TL))
fish_year_df$year <- rownames(fish_year_df)



# melt
fish_year_df <- melt (fish_year_df, id.var = "year")
levels(fish_year_df$variable)[which(levels(fish_year_df$variable) == "size.size")] <- "Size" 
levels(fish_year_df$variable)[which(levels(fish_year_df$variable) == "size.depth.max")] <- "Max Depth" 
levels(fish_year_df$variable)[which(levels(fish_year_df$variable) == "size.depth.min")] <- "Min Depth" 
levels(fish_year_df$variable)[which(levels(fish_year_df$variable) == "size.TL")] <- "TL" 


# 
plot_size_depth<-ggplot (fish_year_df, 
                         aes (x=as.numeric(year), 
                              y=value
                         )) + 
  
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs",k=4),col ="turquoise4") + 
  facet_wrap (~variable,ncol=2,scales = "free")+
  
  geom_point(color="turquoise3") + 
  
  xlab ("Year") + ylab ("Value") +
  theme (legend.position = c(0.12,0.90),
         axis.title = element_text(size=15),
         strip.text.x = element_text(size = 14, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="turquoise3", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         ))+
  geom_text_repel(data = fish_year_df[which(fish_year_df$year %in% 
                                              seq(1950,2020,10)),],
                  aes (label = year),size=3) + 
  theme_bw()



# save plot

pdf (here ("output", "size_depth_catch_fish.pdf"),height=6,width=5)


plot_size_depth


dev.off()




# ------------------------------------------------------------




# THE TRAIT SPACE

# the regional trait space
average_traits_whole_region <- traits %>% 
  
  group_by(Genus) %>% 
  
  summarise (Body_size = mean (Body_size,na.rm=T),  
             Trophic_level = mean (Trophic_level,na.rm=T), 
             Depth_max = mean (Depth_max,na.rm=T), 
             # Diel_activity = getmode (Diel_activity), 
             Size_group = getmode (Size_group), 
             Level_water = getmode (Level_water))  %>%
  
  
  dplyr::select (Genus, Body_size, Trophic_level, Depth_max, 
                 #Diel_activity, 
                 Size_group, Level_water)  


# the first row is empty
average_traits_whole_region <- average_traits_whole_region[-1,]

# transform categorical into ranking traits
# group size
average_traits_whole_region$Size_group [which(average_traits_whole_region$Size_group == "sol")] <- 1
average_traits_whole_region$Size_group [which(average_traits_whole_region$Size_group == "pair")] <- 2
average_traits_whole_region$Size_group [which(average_traits_whole_region$Size_group == "smallg")] <- 3
average_traits_whole_region$Size_group [which(average_traits_whole_region$Size_group == "medg")] <- 4
average_traits_whole_region$Size_group [which(average_traits_whole_region$Size_group == "largeg")] <- 5
average_traits_whole_region$Size_group <- ordered (average_traits_whole_region$Size_group) # ordered

# level water
average_traits_whole_region$Level_water [which(average_traits_whole_region$Level_water == "bottom")] <- 3
average_traits_whole_region$Level_water [which(average_traits_whole_region$Level_water == "low")] <- 2
average_traits_whole_region$Level_water [which(average_traits_whole_region$Level_water == "high")] <- 1
average_traits_whole_region$Level_water <- ordered (average_traits_whole_region$Level_water) # ordered

# daily activity
#average_traits_whole_region$Diel_activity <- ifelse (average_traits_whole_region$Diel_activity %in% c("night", "both"),
#                                        1,0)

# setrownames
average_traits_whole_region<- data.frame(average_traits_whole_region)
rownames (average_traits_whole_region) <- average_traits_whole_region$Genus
average_traits_whole_region<- average_traits_whole_region[,-1]

# reef fish genera
average_traits_whole_region <- average_traits_whole_region[which(rownames(average_traits_whole_region) %in% 
                                                                   reef_fish$Genus),]

# distance matrix
# turning ranks into numerics
average_traits_whole_region$Size_group <- as.numeric(average_traits_whole_region$Size_group)
average_traits_whole_region$Level_water <- as.numeric(average_traits_whole_region$Level_water)
average_traits_whole_region$interaction_level_group <- ifelse (is.na(average_traits_whole_region$Level_water * average_traits_whole_region$Size_group),
                                                               average_traits_whole_region$Level_water,
                                                               average_traits_whole_region$Level_water * average_traits_whole_region$Size_group)

# standardize traits
std_traits_whole <-decostand (average_traits_whole_region[,-which(colnames(average_traits_whole_region) %in% c(#"Diel_activity", 
  "Size_group", 
  "Level_water"))],
  "standardize",na.rm=T)


# distance matrix
gower_matrix <- vegdist (std_traits_whole,
                         method="gower",na.rm=T)



# principal coordinate analysis
# Building the functional space based on a PCOA 
pcoa_whole<-pcoa(gower_matrix,correction = "cailliez") # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 





## ----------------------------------------------------- ##
# Testing the Quality of the functional space based on the method 
# of Maire et al. (2015) in GEB, i.e. how many axes do we need to keep to 
# faithfully represent the original Gower's distances.
# The function will test the quality of the space from 2 to n axes using 
# dudi.pco(quasieuclid(gower_matrix), scannf=F, nf=10)
## ------------------------------------------------------ ##

dev.set(dev.next())

quality<-quality_funct_space_fromdist( gower_matrix,  nbdim=10,   
                                       plot=here ("output", "quality_funct_space_I_fish.png")) 

### the minimal value corresponds to the best space to use (min SD)
axes_to_choose <- which(quality$meanSD == min(quality$meanSD)) 
### calculate and show the percentage of inertia explained by the choosen axes
(Inertia7<- (sum(pcoa_whole$values[1:axes_to_choose])) /(sum(pcoa_whole$values))) 




#barplot(pco$eig) # barplot of eigenvalues for each axis 
(Inertia2<-(pcoa_whole$values$Eigenvalues[1]+
              pcoa_whole$values$Eigenvalues[2]+
              pcoa_whole$values$Eigenvalues[3]) /(sum(pcoa_whole$values$Eigenvalues[which(pcoa_whole$values$Eigenvalues>0)]))) # percentage of inertia explained by the two first axes

## only the frst axis
(Inertia.first <- (pcoa_whole$values$Eigenvalues[1]) /(sum(pcoa_whole$values$Eigenvalues[which(pcoa_whole$values$Eigenvalues>0)])))
## only the frst axis
(Inertia.scnd <- (pcoa_whole$values$Eigenvalues[2]) /(sum(pcoa_whole$values$Eigenvalues[which(pcoa_whole$values$Eigenvalues>0)])))
## only the frst axis
(Inertia.trd <- (pcoa_whole$values$Eigenvalues[3]) /(sum(pcoa_whole$values$Eigenvalues[which(pcoa_whole$values$Eigenvalues>0)])))
Inertia.first+Inertia.scnd


## complete space
all <- data.frame (pcoa_whole$vectors[,1:axes_to_choose],
                   ext = F,
                   sp = rownames(average_traits_whole_region),
                   TL=average_traits_whole_region$Trophic_level)
a <- all [chull(all[,1:axes_to_choose], y = NULL),] # its convex hull

## combinations of PCOA coordinates to have the surface to krig
x.range <- range(all$Axis.1)
y.range <- range(all$Axis.2)
x<-seq(x.range[1], x.range[2], length.out=200)
y<-seq(y.range[1], y.range[2], length.out=200)
grd<-expand.grid(x,y)
coordinates(grd) <- ~ Var1+Var2
gridded(grd) <- TRUE

# df as spatial object
all_coord<-data.frame (all [,c("Axis.1", "Axis.2", "TL")])
coordinates (all_coord) <- ~Axis.1 + Axis.2

# krigging #> [inverse distance weighted interpolation]
dat.idw <- gstat::idw(formula=TL ~ 1, 
                      locations = all_coord, 
                      newdata = grd, idp = 4)
# plot
plot(dat.idw)
points (all[,c("Axis.1", "Axis.2")])

# crop dat.idw
library(sp)
library (raster)
p <- Polygon(a[,c("Axis.1", "Axis.2")] )
ps <- Polygons(list(p),1)
sps <- SpatialPolygons(list(ps))

## crop and mask
r2 <- crop(raster (dat.idw), extent(sps))
r3 <- mask(r2, sps)

## Check that it worked
plot(r3,add=T)
plot(sps, add=TRUE, lwd=2)

#df
df_vals <- cbind (coordinates (r3),
                  pred = values(r3))

# create directory to receive the figures
dir.create(here ("output", "figs_animation"))

# project year trait space and save

plots_year <- lapply (seq (min (fisheries_wtrait$Year), max(fisheries_wtrait$Year)), function (t) {
  
  # define year
  year <- t
  
  
  # testar para 1988
  
  # choose the dataset of one specific year
  chosen_set <- fisheries_wtrait[which(fisheries_wtrait$Year == year),]
  
  # points proportional to the catch
  catch_year_genus <- chosen_set %>%
    
    group_by(Genus_match,Year) %>% 
    
    summarize(sum_catch=sum(CatchAmount_t,na.rm=T),
              TL = mean (Trophic_level,na.rm=T)
    ) 
  
  
  #  bycatch
  bycatch_year <-sum(catch_year_genus$sum_catch,na.rm=T)*percentage_for_bycatch
  catch_year_genus<- catch_year_genus[which(catch_year_genus$sum_catch >bycatch_year),]
  
  
  # size
  all_year <- data.frame (all , 
                          catch_year_genus [match (all$sp, 
                                                   catch_year_genus$Genus_match), 
                                            c("sum_catch", "TL")])
  
  # rm NAs
  all_year<-all_year[is.na(all_year$sum_catch) != T,]
  
  #year convex hull
  a_year <- all_year [chull(all_year[,1:axes_to_choose], y = NULL),] # its convex hull
  
  # crop dat.idw
  p <- Polygon(a_year[,c("Axis.1", "Axis.2")] )
  ps <- Polygons(list(p),1)
  sps <- SpatialPolygons(list(ps))
  
  ## crop and mask
  r2 <- crop(raster (dat.idw), extent(sps))
  r3 <- mask(r2, sps)
  
  ## df with data
  df_vals <- cbind (coordinates (r3),
                    pred = values(r3))
  
  
  # plot 
  plotA <-ggplot(data.frame (df_vals)) + 
    geom_tile(aes(x, y, fill = pred)) +
    #scale_fill_viridis_c() +  
    scale_fill_gradient2(low = "blue", 
                         mid = "white",
                         high = "red",
                         midpoint=3,
                         na.value = NA)+
    scale_alpha(range = c(0.15, 0.65), guide = "none") +  
    ggtitle("Trophic level") +
    coord_quickmap()
  
  
  ## plot A (complete space)
  plotA1 <- plotA+
    geom_point(data = a, aes(x=Axis.1, 
                             y=Axis.2), 
               size=3,shape=3) + 
    theme_bw()+
    
    # polygon
    
    geom_polygon(data=a, aes (Axis.1,Axis.2),
                 alpha=0.001,
                 fill="gray",
                 colour = "black",
                 size=1,
                 linetype = 2)+ 
    theme_bw() +
    #ylim (c(-0.35,0.2))+
    #xlim (c(-0.4,0.5))+
    xlab(paste ("Axis I:", round(Inertia.first*100,2),"%"))+
    ylab(paste ("Axis II:", round(Inertia.scnd*100,2),"%")) + 
    geom_label(data = a, aes (x=Axis.1, y=Axis.2, label=(sp)),
               size=3,
               nudge_x = 0.02,
               nudge_y = 0.02)+ 
    theme (legend.position = "none")
  
  
  # complete space
  plotA2 <- plotA1 + 
       geom_point(data = all_year [is.na (all_year$sum_catch) != T,], 
               aes (x=Axis.1,
                    y=Axis.2,
                    size=sum_catch*2,
                    fill=TL,
                    colour = TL),
               alpha=0.85) +
    scale_colour_gradient2(low = "blue", 
                           mid = "white",
                           high = "red",
                           midpoint=3)+
    ggtitle (t)
  
  
  plotA2
  
  # genus with large amount of catch
  most_catched <- all_year[order(all_year$sum_catch, decreasing=T),]
  
  # plot the five most catched
  nsp_to_plot <- 10
  plotA2 <- plotA2 + geom_text_repel(data = most_catched[1:nsp_to_plot,], 
                                     aes (x=Axis.1, y=Axis.2, label=(sp)),
                                     size=3,
                                     nudge_x = 0.02,
                                     nudge_y = 0.02) 
  plotA2
  ggsave (filename = here ("output", "figs_animation", paste ("fig", t, ".png"))) 
  
})


#anime
list_img <- list.files(path = here ("output", "figs_animation"), full.names = T)

##https://cran.r-project.org/web/packages/magick/vignettes/intro.html
a_image<-image_read(list_img)
animation <-  image_animate(a_image, fps = 1)
image_write(animation, here ("output","animation_BR_fisheries.gif"))



# trait space per region
# catched_fish_set per region
catched_fish_region <- lapply (year_region_composition_filtered, function (i){
  
  # identify catched spp
  region_bycatch <- rowSums(i[,-1])*percentage_for_bycatch
  
  # filter
  region_amount <- lapply (seq (1,nrow (i)), function (k)
    
    unlist(ifelse (i[k,-1] < region_bycatch[k], 0,
                   i[k,-1]))
    
  )
  region_amount <- do.call(rbind, region_amount)
  rownames(region_amount) <- year_composition$Year
  colnames(region_amount) <- colnames(i)[-1]
  
  
  # remove species without catches
  region_amount <- region_amount[,which(colSums(region_amount)>0)]
  
  # filter year
  #amount_year <- region_amount[which(rownames(region_amount) == year),]
  #amount_year <- amount_year[which(amount_year >0)] # larger than zero
  year_composition_set <- colnames(i)[which(colnames(i) %in% colnames(region_amount))]
  catched_fish <-cbind(all, ext1=ifelse(all$sp %in% 
                                          year_composition_set,T,F))
  catched_fish <-catched_fish[which(catched_fish$ext1==T),]
  catched_fish_set <- catched_fish [chull(catched_fish, y = NULL),]
  res <- list (notbycatch = year_composition_set,
               catched_fish = catched_fish_set)
  res
})
names(catched_fish_region) <- unique(fisheries_wtrait$Region)



# region spaces
plotB <- lapply (catched_fish_region, function (i) {
  
  # crop dat.idw
  p <- Polygon(i$catched_fish[,c("Axis.1", "Axis.2")] )
  ps <- Polygons(list(p),1)
  sps <- SpatialPolygons(list(ps))
  
  ## crop and mask
  r2 <- crop(raster (dat.idw), extent(sps))
  r3 <- mask(r2, sps)
  
  ## df with data
  df_vals <- cbind (coordinates (r3),
                    pred = values(r3))
  
  # plot 
  plotA <-ggplot(data.frame (df_vals)) + 
    geom_tile(aes(x, y, fill = pred)) +
    #scale_fill_viridis_c() +  
    scale_fill_gradient2(low = "blue", 
                         mid = "white",
                         high = "red",
                         midpoint=3,
                         na.value = NA)+
    scale_alpha(range = c(0.15, 0.65), guide = "none") +  
    ggtitle("Trophic level") +
    coord_quickmap()
  
  
  ## plot A (complete space)
  plotA1 <- plotA+
    
    geom_point(data = a, aes(Axis.1,Axis.2),
               size=2,shape=3) + theme_bw()+
    geom_polygon(data=a, aes (Axis.1,Axis.2),
                 alpha=0.01,
                 fill="gray",
                 colour = "black",
                 size=0.5,
                 linetype = 2) + # complete space
    geom_polygon(data=i$catched_fish, aes (Axis.1,Axis.2),
                 alpha=0.1,
                 fill="white",
                 colour = "gray",
                 size=1,
                 linetype = 1) 
  
})


# the overall trait space
# points proportional to the catch
catch_year_genus <- fisheries_wtrait %>%
  
  group_by(Genus_match) %>% 
  
  summarize(sum_catch=sum(CatchAmount_t,na.rm=T),
            TL = mean (Trophic_level,na.rm=T)
            
  ) 

#bycatch_year <-sum(catch_year_genus$sum_catch,na.rm=T)*percentage_for_bycatch
#catch_year_genus<- catch_year_genus[which(catch_year_genus$sum_catch >bycatch_year),]

# size
all_overall <- data.frame (all , 
                           catch_year_genus [match (all$sp, 
                                                    catch_year_genus$Genus_match), 
                                             c("sum_catch", "TL")])

# Catched that is not bycatch
notbycatch<-unique(unlist(sapply (catched_fish_region,"[[","notbycatch")))
all_overall <- all_overall [which(rownames(all_overall) %in% notbycatch),]

# rm NAs
all_overall<-all_overall[is.na(all_overall$sum_catch) != T,]

#year convex hull
a_overall <- all_overall [chull(all_overall[,1:2], y = NULL),] # its convex hull

# complete space
plotA <-ggplot(data.frame (df_vals)) + 
  geom_tile(aes(x, y, fill = pred)) +
  #scale_fill_viridis_c() +  
  scale_fill_gradient2(low = "blue", 
                       mid = "white",
                       high = "red",
                       na.value = NA,
                       midpoint=3)+
  scale_alpha(range = c(0.15, 0.65), guide = "none") +  
  ggtitle("Trophic level") +
  coord_quickmap()

plotA1 <- plotA + 
  geom_point(data =a, aes(Axis.1,Axis.2), size=2,shape=3) + 
  theme_bw()+
  geom_polygon(data=a, aes (Axis.1,Axis.2),
               alpha=0.01,
               fill="gray",
               colour = "black",
               size=0.5,
               linetype = 2) +
  geom_text(data = a, aes (x=Axis.1, y=Axis.2, label=(sp)),
            size=3)+ 
  theme (legend.position = "none")

plotA2 <- plotA1+ geom_polygon(data=a_overall, aes (Axis.1,Axis.2),
                               alpha=0.3,
                               fill="white",
                               colour = "gray",
                               size=0,
                               linetype = 1) +
  # point size
  geom_point(data = all_overall [is.na (all_overall$sum_catch) != T,], 
             aes (x=Axis.1,y=Axis.2,size=sum_catch,
                  colour=TL,
                  alpha=0.3))+
  scale_colour_gradient2(low = "blue", 
                         mid = "white",
                         high = "red",
                         midpoint=3)+
  
  theme(legend.position = "none")

# genus with large amount of catch
most_catched <- all_overall[order(all_overall$sum_catch, decreasing=T),]

# plot the five most catched
nsp_to_plot <- 100
plotA2 <- plotA2 + geom_text_repel(data = most_catched[1:nsp_to_plot,], 
                                   aes (x=Axis.1, y=Axis.2, label=(sp)),
                                   size=4,
                                   max.overlaps = 100) +
  xlab ("Axis 1 (38.44%)") + 
  ylab ("Axis 2 (18.87%)")

# save
pdf (here ("output", "trait_space_fishing.pdf"),height=9,width=6)

grid.arrange(plotA2+theme (legend.position = "right"),
             plotB[[1]]+theme(axis.text = element_blank(),
                              axis.title = element_blank(),
                              legend.position = "none")+
               ggtitle("North"),
             plotB[[2]]+theme(axis.text = element_blank(),
                              axis.title = element_blank(),
                              legend.position = "none")+
               ggtitle("Northeast"),
             plotB[[3]]+theme(axis.text = element_blank(),
                              axis.title = element_blank(),
                              legend.position = "none")+
               ggtitle("Southeast"),
             plotB[[4]]+theme(axis.text = element_blank(),
                              axis.title = element_blank(),
                              legend.position = "none")+
               ggtitle("South"),
             layout_matrix = rbind (c(1,1),
                                    c(1,1),
                                    c(2,3),
                                    c(4,5)))


dev.off()


# --------------------------------
# exploring trait space over time



# change the percentage of bycatch to have all the data
#percentage_for_bycatch<-0.01





TS_time <- lapply (seq (min (fisheries_wtrait$Year), max(fisheries_wtrait$Year)), function (t) {
  
  # define year
  year <- t
  
  # choose the dataset of one specific year
  chosen_set <- fisheries_wtrait[which(fisheries_wtrait$Year == year),]
  
  # filter genus
  # points proportional to the catch
  catch_year_genus <- chosen_set %>%
    
    group_by(Genus_match,Year) %>% 
    
    summarize(sum_catch=sum(CatchAmount_t,na.rm=T),
    ) 
  
  
  # 5% threshold to consider bycatch
  bycatch_year <-sum(catch_year_genus$sum_catch,na.rm=T)*percentage_for_bycatch
  catch_year_genus<- catch_year_genus[which(catch_year_genus$sum_catch >bycatch_year),]
  
  
  # size
  all_year <- data.frame (all , 
                          catch_year_genus [match (all$sp, 
                                                   catch_year_genus$Genus_match), 
                                            "sum_catch"])
  
  # rm NAs
  all_year<-all_year[is.na(all_year$sum_catch) != T,]
  
  
  #year convex hull
  a_year <- all_year [chull(all_year[,1:2], y = NULL),] # its convex hull
  
  # quantifying reduction in functional space
  # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
  chull.poly.complete <- Polygon(a_overall[,1:2], hole=F)
  chull.area.complete <- chull.poly.complete@area
  
  
  
  # year trait space
  chull.poly.year <- Polygon(a_year[,1:2], hole=F)
  chull.area.year <- chull.poly.year@area
  
  # ratio
  df_res <- data.frame (completeTS = chull.area.complete,
                        yearTS =chull.area.year,
                        ratioTS=chull.area.year/chull.area.complete,
                        year = t)
  
})


# plot 
TS_time_df <- do.call (rbind, TS_time)

# BR trend
BR_trend_TS<- ggplot (TS_time_df, aes (x=year,y= ratioTS)) + 
  geom_point(col="turquoise4") + 
  geom_smooth(col="turquoise3") + 
  ylab ("Year trait space area / Complete trait space area") + 
  theme_classic()



# ---------------------------------------------------------------
# per region




TS_time_region <- lapply (unique(fisheries_wtrait$Region), function (r) # each region
  
  lapply (seq (min (fisheries_wtrait$Year), max(fisheries_wtrait$Year)), function (t) { # each year
    
    # define year
    year <- t
    
    # choose the dataset of one specific year
    chosen_set <- fisheries_wtrait[which(fisheries_wtrait$Region == r & fisheries_wtrait$Year == year),]
    
    # filter genus
    # points proportional to the catch
    catch_year_genus <- chosen_set %>%
      
      group_by(Genus_match,Year) %>% 
      
      summarize(sum_catch=sum(CatchAmount_t,na.rm=T),
      ) 
    
    
    # 5% threshold to consider bycatch
    bycatch_year <-sum(catch_year_genus$sum_catch,na.rm=T)*percentage_for_bycatch
    catch_year_genus<- catch_year_genus[which(catch_year_genus$sum_catch >bycatch_year),]
    
    
    # size
    all_year <- data.frame (all , 
                            catch_year_genus [match (all$sp, 
                                                     catch_year_genus$Genus_match), 
                                              "sum_catch"])
    
    # rm NAs
    all_year<-all_year[is.na(all_year$sum_catch) != T,]
    
    
    #year convex hull
    a_year <- all_year [chull(all_year[,1:2], y = NULL),] # its convex hull
    
    # quantifying reduction in functional space
    # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
    chull.poly.complete <- Polygon(a_overall[,1:2], hole=F)
    chull.area.complete <- chull.poly.complete@area
    
    
    
    # year trait space
    chull.poly.year <- Polygon(a_year[,1:2], hole=F)
    chull.area.year <- chull.poly.year@area
    
    # ratio
    df_res <- data.frame (completeTS = chull.area.complete,
                          yearTS =chull.area.year,
                          ratioTS=chull.area.year/chull.area.complete,
                          year = t,
                          region = r)
    
  }) # close year
) # close region

# plot 
TS_time_region_df <- lapply (TS_time_region, function (r) 
  
  do.call (rbind, r)
)

# melt the list once again  
TS_time_region_df<- do.call(rbind, TS_time_region_df)
# discreticing year
TS_time_region_df<-TS_time_region_df %>% 
  mutate(year_binned = cut_number(year, n=6))


# BR trend

region_trend_TS<-ggplot (TS_time_region_df, aes (x=year,y= ratioTS)) + 
  geom_point(col="turquoise4") + 
  geom_smooth(col="turquoise3") + 
  ylab ("Year trait space area / Complete trait space area") + 
  theme_classic() + 
  facet_wrap(~region)


grid.arrange (
  BR_trend_TS + theme (axis.title = element_blank()),
  region_trend_TS+ theme (axis.title = element_blank()),
  nrow=2, ncol=2,
  layout_matrix = rbind (c(1,1),
                         c(2,2),
                         c(2,2),
                         c(2,2))
)


# boxplot 

region_trend_TS_discrete<-ggplot (TS_time_region_df, aes (x=year_binned,y= ratioTS)) + 
  geom_boxplot()+
  ylab ("Year trait space area / Complete trait space area") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle=45))+
  facet_wrap(~region)
region_trend_TS_discrete




# -----------------------------


# variation in diet over time


TL_fisheries <- tapply (fisheries_wtrait$CatchAmount_t,
                        list(fisheries_wtrait$Year,
                             fisheries_wtrait$Diet,
                             fisheries_wtrait$Region),
                        sum, default = 0) 

TL_fisheries<-melt (TL_fisheries)
colnames(TL_fisheries) <- c("Year", "TrGroup", "Region", "value")
# change region names

TL_fisheries$Region <- recode_factor(TL_fisheries$Region, 
                                     "Sul" = "South", 
                                     "SE" = "Southeast",
                                     "NE" = "Northeast", 
                                     "Norte" = "North")

# order diet according to TL
TL_fisheries$TrGroup <- factor(TL_fisheries$TrGroup,
                               levels = c("fc", "im", "is", "om", "hm", "hd"))

TL_fisheries$TrGroup <- recode_factor(TL_fisheries$TrGroup, 
                                     "fc" = "PC", 
                                     "im" = "IM",
                                     "is" = "IS", 
                                     "om" = "OM",
                                     "hm" = "HM",
                                     "hd" = "HD")

## pot trnds over time
trophic_level_trends <- ggplot (TL_fisheries, aes (x=Year, y=value,
                                                   colour = TrGroup)) + 
  facet_wrap(~Region,scales = "free")+
  geom_line(size=1) + 
  xlab ("Year") + 
  ylab ("Sum catch (thousands of tonnes)") + 
  scale_colour_viridis_d(option = "viridis") + 
  theme (strip.text = element_text(face="bold"),
         strip.text.x = element_text(size = 10, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         )) 

pdf (here ("output", "TL_trends.pdf"),height=6,width=6)
trophic_level_trends+theme_classic()
dev.off()


## ======================================


# trends for selected genera



selected_fish_trend <- tapply (fisheries_wtrait$CatchAmount_t,
                               list(fisheries_wtrait$Year,
                                    #fisheries_wtrait$Diet_2012,
                                    fisheries_wtrait$Region,
                                    fisheries_wtrait$Genus_match),
                               sum) 

selected_fish_trend<-melt (selected_fish_trend)
colnames(selected_fish_trend) <- c("Year",  "Region", "Genus_match","value")

# plot
sel_spp_trends <- ggplot (selected_fish_trend[which(selected_fish_trend$Genus_match %in% 
                                                      c("Scarus", 
                                                        "Sparisoma",
                                                        "Balistes", 
                                                        "Lutjanus",
                                                        "Mycteroperca",
                                                        "Epinephelus",
                                                        "Ocyurus")),],
                          
                          
                          aes (x=Year, y=log(value),
                               colour = Genus_match),alpha=0.5) + 
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




agg1<-aggregate(CatchAmount_t~Region+Genus_match+Year,sum,data=fisheries_wtrait)
# change region names
#agg1$Region[which(agg1$Region == "Sul")] <- "South"
#agg1$Region[which(agg1$Region == "SE")] <- "Southeast"
#agg1$Region[which(agg1$Region == "NE")] <- "Northeast"
#agg1$Region[which(agg1$Region == "Norte")] <- "North"


# plot
trend_sel_sp_region <- ggplot(agg1[which(agg1$Genus_match %in% 
                                           c("Scarus", 
                                             "Sparisoma",
                                             "Balistes",
                                             "Mycteroperca",
                                             "Epinephelus")),],
                              aes (x=Year, y=sqrt(CatchAmount_t),colour = Genus_match))+
  geom_point() + 
  
  facet_wrap(~Region,scales = "free")+
  
  poison_smooth(formula = y ~ s(x, bs = "cs",k=4)) + theme_classic() + 
  scale_colour_viridis_d(option = "viridis", begin=0,end=1, name = "Genus") + 
  theme (strip.text = element_text(face="bold"),
         strip.text.x = element_text(size = 10, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         ))

trend_sel_sp_region



# lutjanids



trend_sel_sp_region_lutjanus <- ggplot(agg1[which(agg1$Genus_match %in% 
                                                    c("Lutjanus",
                                                      "Ocyurus")),],
                                       aes (x=Year, y=sqrt(CatchAmount_t),colour = Genus_match))+
  geom_point() + 
  facet_wrap(~Region,scales = "free")+
  poison_smooth(formula = y ~ s(x, bs = "cs",k=4)) + theme_classic() + 
  scale_colour_viridis_d(option = "viridis", begin=0.1,end=0.7, name= "") + 
  theme (strip.text = element_text(face="bold"),
         strip.text.x = element_text(size = 10, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         ))

trend_sel_sp_region_lutjanus


## cartilaginous fish

trend_sel_sp_region_cart <- ggplot(agg1[which(agg1$Genus_match %in% 
                                                c("Carcharhinus", "Ginglymostoma")),],
                                   aes (x=Year, y=sqrt(CatchAmount_t),colour = Genus_match))+
  geom_point() + 
  facet_wrap(~Region,scales = "free")+
  poison_smooth(formula = y ~ s(x, bs = "cs",k=4)) + theme_classic() + 
  scale_colour_viridis_d(option = "viridis", begin=0,end=0.7, name = "") + 
  theme (strip.text = element_text(face="bold"),
         strip.text.x = element_text(size = 10, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         ))

# save
# scarus et al
pdf (here ("output", "sel_spp_trends_scarus.pdf"),height=5,width=6)

trend_sel_sp_region+theme (axis.title.x = element_blank(), 
                           axis.text = element_text(size=7), 
                           legend.key.width = unit(0.1,"cm"),
                           legend.key.height = unit(0.5,"cm"),
                           legend.title = element_blank(),
                           legend.text = element_text(size=8)) + 
  ylab ("Catch amount (tonnes, squared-root)")

dev.off()              

# lutjanus ocyurus
pdf (here ("output", "sel_spp_trends_lutjanus.pdf"),height=5,width=6)

trend_sel_sp_region_lutjanus + theme (axis.title.x = element_blank(), 
                                      axis.text = element_text(size=7),
                                      legend.key.width = unit(0.1,"cm"),
                                      legend.key.height = unit(0.5,"cm"),
                                      legend.title = element_blank(),
                                      legend.text = element_text(size=8))+ 
  ylab ("Catch amount (tonnes, squared-root)")

dev.off()

# sharks
pdf (here ("output", "sel_spp_trends_shark.pdf"),height=5,width=6)

trend_sel_sp_region_cart+theme (axis.title.x = element_blank(), 
                                axis.text = element_text(size=7),
                                legend.key.width = unit(0.1,"cm"),
                                legend.key.height = unit(0.5,"cm"),
                                legend.title = element_blank(),
                                legend.text = element_text(size=8))+ 
  ylab ("Catch amount (tonnes, squared-root)")

dev.off()




## correlations to project trait values into the ordination
correlations <- cor (data.matrix(data.frame (average_traits_whole_region[,-which(colnames(average_traits_whole_region) %in% c(#"Diel_activity", 
  "Size_group", 
  "Level_water"))],
  pcoa_whole$vectors[,1:3])),
  use = "complete.obs")
correlations<-correlations [-which(rownames(correlations) %in% c("Axis.1","Axis.2","Axis.3")),
                            c("Axis.1","Axis.2")]# interesting correlations

# show traits in the trait space


#plotA2_cor <- plotA + geom_segment(aes(x = 0, y = 0, 
#                                    xend = correlations[1,1]*0.2, 
#                                    yend = correlations[1,2]*0.2),size = 1,
#                                color="#2B7A0B",alpha=0.08,
#                                arrow = arrow(length = unit(.35, "cm")))  + 
#  ## annotate
#  annotate(geom="text",x=correlations[1,1]*0.22,
#           y=correlations[1,2]*0.30,label="Total length",
#           color="#2B7A0B",alpha=0.5) +
#  
#  geom_segment(aes(x = 0, y = 0, 
#                   xend = correlations[2,1]*0.2, 
#                   yend = correlations[2,2]*0.2),size = 1,
#               color="#2B7A0B",alpha=0.08,
#               arrow = arrow(length = unit(.35, "cm"))) + 
#  annotate(geom="text",x=correlations[2,1]*0.27,
#           y=correlations[2,2]*0.22,label="Trophic level",
#           color="#2B7A0B",alpha=0.5) +
#  
#  geom_segment(aes(x = 0, y = 0, 
#                   xend = correlations[3,1]*0.2, 
#                   yend = correlations[3,2]*0.2),size = 1,
#               color="#2B7A0B",alpha=0.08,
#               arrow = arrow(length = unit(.35, "cm"))) + 
#  annotate(geom="text",x=correlations[3,1]*0.37,
#           y=correlations[3,2]*0.2,label="Max depth",
#           color="#2B7A0B",alpha=0.5) +
#  
#  geom_segment(aes(x = 0, y = 0, 
#                   xend = correlations[4,1]*0.2, 
#                   yend = correlations[4,2]*0.2),size = 1,
#               color="#2B7A0B",alpha=0.1,
#               arrow = arrow(length = unit(.35, "cm"))) + 
#  annotate(geom="text",x=correlations[4,1]*0.25,
#           y=correlations[4,2]*0.23,label="Level water : Group size",
#           color="#2B7A0B",alpha=0.5)  
#