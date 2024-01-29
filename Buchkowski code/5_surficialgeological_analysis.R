# Surficial geology ----

# Load in the geological data

geology <- read_sf("C:/Users/rober/Documents/Moose_Model_Original_Data/Geology/Surficial/Regional_Surficial_Geology.shp")

# DESCRIPTION:
# The regional surficial geology map layer provides data on the types of material and landforms found at the surface of the earth. It was derived from 1:50 000, 1:250 000 and 1:1 000 000 scale surficial geology maps, aerial photograph interpretation and variable amounts of field verification. The regional surfical geology map of the Island of Newfoundland was compiled by Liverman and Taylor (1990a). The compiled paper version was prepared at a scale of 1:500 000 (Liverman and Taylor, 1990b). The regional surficial geology map of Labrador was compiled by Klassen et al. (1992, Map n. 1814A) at a 1:1 000 000 scale. The map presented in the Geoscience Atlas, is based on the attributes in the 'Genetic1Ma' field. These are attributes as defined in Klassen et al. (1992) for the Labrador map. The surficial geology attributes in the 'Genetic250' field, for the Island portion of the map, were translated to the equivalent attribute in the 'Genetic1Ma' field.&lt;/SPAN&gt;&lt;/P&gt;&lt;P&gt;&lt;SPAN&gt;For more information, see this layer's Help file on the Geoscience Atlas (http://geoatlas.gov.nl.ca/).&lt;/SPAN&gt;&lt;/P&gt;&lt;/DIV&gt;&lt;/DIV&gt;&lt;/DIV&gt;</idAbs><idCredit>See the Help file for credit information

# Plot the geology
png("Plots/surfacegeology.png", width = 12, height = 10, units = "in", res = 600)
geology %>% ggplot() + geom_sf(aes(fill = GENETIC1MA), size = 0.1, color = "white") + theme_classic()
dev.off()

# First add a unique key:
geology = geology %>% mutate(UIDsg = 1:dim(geology)[1])

# Save a geology database that doesn't have spatial
geology %>% tibble() %>% select(-geometry) %>%
  write_rds("Data/surfacegeology_aspatial.rds")

# Link these geological features to the forestry plots:

# Load in the forestry shapefile example to get CRS
aa = st_read("C:/Users/rober/Documents/GitHubRepos/MooseEcon/Data/forest_inventory/inventory_spatial/forestry1.shp", query = 'SELECT * FROM "forestry1" WHERE FID = 1')

crs(aa)

# Need to transform to the same coordinate system
rm(aa)

# Get rid of the extra columns for now
geology = geology %>% select(UIDsg)

# Transform CRS to UTM for the surficial geology data:
geology <- st_transform(geology,"+proj=utm +zone=21 +datum=NAD27 +units=m +no_defs")

# Write a function that takes in the geology data and each section of the forestry data and returns the matches:

matching_function <- function(bb, geologyt){
  # Transform CRS:
  bb <- st_transform(bb,crs(geologyt))
  
  # Find the centroids of each polygon in the forestry data set
  cc <- st_centroid(bb)
  
  # Determine which point is inside which polygon
  dd <- st_join(cc, geologyt)
  
  # Check the NAs
  dd2 = dd %>% filter(is.na(UIDsg))
  
  # Remove the NA dimensions
  dd2 = dd2[!is.na(st_dimension(dd2)),]
  
  # Find the ~2000 empty points:
  bb2 <- bb %>% filter(UID %in% dd2$UID)
  
  geologyt1 <- st_make_valid(geologyt)
  geologyt2 <- st_crop(geologyt1, st_bbox(bb2))
  
  # Find the matches for geology
  dd3 <- st_join(bb2, geologyt2)
  
  # If there are multiple, pick the one that is the biggest overlap
  dd3 <- dd3 %>% mutate(AREA = st_area(dd3$geometry))
  
  dd4 <- data.frame(dd3) %>% select(-geometry)
  
  dd4 <- tibble(dd4) %>% 
    arrange(-AREA) %>%
    group_by(UID) %>% 
    slice(c(1))
  
  unique(table(dd4$UID))
  
  # Find the still remaining empty points:
  dd5 <- dd4 %>% select(-AREA) %>% filter(is.na(UIDsg))
  bb2 <- bb %>% filter(UID %in% dd5$UID)
  
  geologyt2 <- st_crop(geologyt1, st_bbox(bb2))
  
  # This loop finds the closest 4 polygons using a box and then determines which is actually closest using the st_distance function. This is significantly faster than checking all distances between the forestry polygons and the geology polygons.
  for(i in 1:dim(bb2)[1]){
    keepgoing = T
    dist_to_try = 2000
    
    while(keepgoing){
      geologyt3 <- st_crop(geologyt2, 
                           st_bbox(st_buffer(bb2[i,], 
                                             dist = dist_to_try)))
      if(dim(geologyt3)[1] > 4) break
      dist_to_try = dist_to_try + 100
    }
    
    tmp1 <- st_distance(geologyt3,bb2[i,])
    
    geologyt3_mod = data.frame(geologyt3) %>% select(UIDsg)
    
    dd5[i,"UIDsg"] = geologyt3_mod[which.min(tmp1),"UIDsg"]
    print(paste0("Done ", i, " of ", dim(bb2)[1]))
  }
  
  # Merge these new IDs into the file:
  dd = dd %>% 
    left_join(
      dd4 %>% select(-AREA) %>% rename(UIDsg2 = UIDsg)
    ) %>% 
    left_join(
      dd5 %>% rename(UIDsg3 = UIDsg)
    )
  
  # Make a flag for how the data were calculated:
  # 1 = point match, 2 = polygon overlap, 3 = nearest polygon
  
  ddf = dd %>%
    mutate(flagUIDsg = 
             ifelse(!is.na(UIDsg), 1,
                    ifelse(!is.na(UIDsg2), 2,3))
    ) %>%
    mutate(UIDsg =
             ifelse(!is.na(UIDsg), UIDsg,
                    ifelse(!is.na(UIDsg2), UIDsg2,UIDsg3))
    )
  
  # Save the results
  ddf = tibble(ddf) %>% select(UID, UIDsg,flagUIDsg)
  
  return(ddf)
}

# Load in each slice of the forestry data and match it to a watershed
# .....(1)......
bbin = st_read("C:/Users/rober/Documents/GitHubRepos/MooseEcon/Data/forest_inventory/inventory_spatial/forestry1.shp")

ee <- matching_function(bb = bbin, geologyt = geology)

# Temporarily save the resulting data
ee %>% write_rds("Data/surfacegeology_foreach_UID.rds")

# .....(2)......
bb = st_read("C:/Users/rober/Documents/GitHubRepos/MooseEcon/Data/forest_inventory/inventory_spatial/forestry2.shp")

# Transform CRS:
bb <- st_transform(bb,crs(geologyt))

# Find the centroids of each polygon in the forestry data set
cc <- st_centroid(bb)

# Determine which point is inside which polygon
dd <- st_join(cc, geologyt)

# Check the NAs
dd2 = dd %>% filter(is.na(UIDsg))

# Remove the NA dimensions
dd2 = dd2[!is.na(st_dimension(dd2)),]

# Find the ~2000 empty points:
bb2 <- bb %>% filter(UID %in% dd2$UID)

geologyt1 <- st_make_valid(geologyt)
geologyt2 <- st_crop(geologyt1, st_bbox(bb2))

# Find the matches for geology
dd3 <- st_join(bb2, geologyt2)

# If there are multiple, pick the one that is the biggest overlap
dd3 <- dd3 %>% mutate(AREA = st_area(dd3$geometry))

dd4 <- data.frame(dd3) %>% select(-geometry)

dd4 <- tibble(dd4) %>% 
  arrange(-AREA) %>%
  group_by(UID) %>% 
  slice(c(1))

unique(table(dd4$UID))

# Find the still remaining empty points:
dd5 <- dd4 %>% select(-AREA) %>% filter(is.na(UIDsg))
bb2 <- bb %>% filter(UID %in% dd5$UID)

geologyt2 <- st_crop(geologyt1, st_bbox(bb2))

# This loop finds the closest 4 polygons using a box and then determines which is actually closest using the st_distance function. This is significantly faster than checking all distances between the forestry polygons and the geology polygons.
for(i in 1:dim(bb2)[1]){
  keepgoing = T
  dist_to_try = 2000
  
  while(keepgoing){
    geologyt3 <- st_crop(geologyt2, 
                         st_bbox(st_buffer(bb2[i,], 
                                           dist = dist_to_try)))
    if(dim(geologyt3)[1] > 4) break
    dist_to_try = dist_to_try + 100
  }
  
  tmp1 <- st_distance(geologyt3,bb2[i,])
  
  geologyt3_mod = data.frame(geologyt3) %>% select(UIDsg)
  
  dd5[i,"UIDsg"] = geologyt3_mod[which.min(tmp1),"UIDsg"]
  print(paste0("Done ", i, " of ", dim(bb2)[1]))
}

# Merge these new IDs into the file:
dd = dd %>% 
  left_join(
    dd4 %>% select(-AREA) %>% rename(UIDsg2 = UIDsg)
  ) %>% 
  left_join(
    dd5 %>% rename(UIDsg3 = UIDsg)
  )

# Make a flag for how the data were calculated:
# 1 = point match, 2 = polygon overlap, 3 = nearest polygon

ddf = dd %>%
  mutate(flagUIDsg = 
           ifelse(!is.na(UIDsg), 1,
                  ifelse(!is.na(UIDsg2), 2,3))
  ) %>%
  mutate(UIDsg =
           ifelse(!is.na(UIDsg), UIDsg,
                  ifelse(!is.na(UIDsg2), UIDsg2,UIDsg3))
  )
# Save the results
ee = ee %>% 
  bind_rows(
    tibble(ddf) %>% select(UID, UIDsg,flagUIDsg)
  )

# Temporarily save the resulting data
ee %>% write_rds("Data/surfacegeology_foreach_UID.rds")

# Clean up
rm(bb,cc,dd, bb2, dd2, dd3, dd4, dd5, ddf, geologyt2, geologyt2_mod, tmp1)
gc()

# .....(3)......
bb = st_read("C:/Users/rober/Documents/GitHubRepos/MooseEcon/Data/forest_inventory/inventory_spatial/forestry3.shp")

# Transform CRS:
bb <- st_transform(bb,crs(geologyt))

# Find the centroids of each polygon in the forestry data set
cc <- st_centroid(bb)

# Determine which point is inside which polygon
dd <- st_join(cc, geologyt)

# Check the NAs
dd2 = dd %>% filter(is.na(UIDsg))

# Remove the NA dimensions
dd2 = dd2[!is.na(st_dimension(dd2)),]

# Find the ~2000 empty points:
bb2 <- bb %>% filter(UID %in% dd2$UID)

geologyt1 <- st_make_valid(geologyt)
geologyt2 <- st_crop(geologyt1, st_bbox(bb2))

# Find the matches for geology
dd3 <- st_join(bb2, geologyt2)

# If there are multiple, pick the one that is the biggest overlap
dd3 <- dd3 %>% mutate(AREA = st_area(dd3$geometry))

dd4 <- data.frame(dd3) %>% select(-geometry)

dd4 <- tibble(dd4) %>% 
  arrange(-AREA) %>%
  group_by(UID) %>% 
  slice(c(1))

unique(table(dd4$UID))

# Find the still remaining empty points:
dd5 <- dd4 %>% select(-AREA) %>% filter(is.na(UIDsg))
bb2 <- bb %>% filter(UID %in% dd5$UID)

geologyt2 <- st_crop(geologyt1, st_bbox(bb2))

# This loop finds the closest 4 polygons using a box and then determines which is actually closest using the st_distance function. This is significantly faster than checking all distances between the forestry polygons and the geology polygons.
for(i in 1:dim(bb2)[1]){
  keepgoing = T
  dist_to_try = 2000
  
  while(keepgoing){
    geologyt3 <- st_crop(geologyt2, 
                         st_bbox(st_buffer(bb2[i,], 
                                           dist = dist_to_try)))
    if(dim(geologyt3)[1] > 4) break
    dist_to_try = dist_to_try + 100
  }
  
  tmp1 <- st_distance(geologyt3,bb2[i,])
  
  geologyt3_mod = data.frame(geologyt3) %>% select(UIDsg)
  
  dd5[i,"UIDsg"] = geologyt3_mod[which.min(tmp1),"UIDsg"]
  print(paste0("Done ", i, " of ", dim(bb2)[1]))
}
# Merge these new IDs into the file:
dd = dd %>% 
  left_join(
    dd4 %>% select(-AREA) %>% rename(UIDsg2 = UIDsg)
  ) %>% 
  left_join(
    dd5 %>% rename(UIDsg3 = UIDsg)
  )

# Make a flag for how the data were calculated:
# 1 = point match, 2 = polygon overlap, 3 = nearest polygon

ddf = dd %>%
  mutate(flagUIDsg = 
           ifelse(!is.na(UIDsg), 1,
                  ifelse(!is.na(UIDsg2), 2,3))
  ) %>%
  mutate(UIDsg =
           ifelse(!is.na(UIDsg), UIDsg,
                  ifelse(!is.na(UIDsg2), UIDsg2,UIDsg3))
  )
# Save the results
ee = ee %>% 
  bind_rows(
    tibble(ddf) %>% select(UID, UIDsg,flagUIDsg)
  )

# Clean up
rm(bb,cc,dd, bb2, dd2, dd3, dd4, dd5, ddf, geologyt2, geologyt2_mod, tmp1)
gc()
# Inspect the result
dim(ee)

# Confirm that no UIDsg values are NA, the ones that exist are probably missing geometries
ee %>% filter(is.na(UIDsg))
ee %>% filter(is.na(UID))

# Save the resulting data
ee %>% write_rds("Data/surfacegeology_foreach_UID_old.rds")

# Look at the duplicate matches (An error that needs to be fixed) ----
ee <- read_rds("Data/surfacegeology_foreach_UID_old.rds") # Load in the data

# Find the repeats
repeats <- tibble(ee) %>%
  group_by(UID) %>%
  summarize(N = n()) %>%
  filter(N > 1)

tibble(ee) %>% 
  filter(UID %in% repeats$UID)


# Load in the full forestry data set and then find the repeated UIDs
forestry <- read_sf("Data/forest_inventory/inventory_spatial/forestry1.shp") %>%
  bind_rows(
    read_sf("Data/forest_inventory/inventory_spatial/forestry2.shp")
  ) %>%
  bind_rows(
    read_sf("Data/forest_inventory/inventory_spatial/forestry3.shp")
  ) %>% 
  filter(UID %in% repeats$UID)

forestry <- st_transform(forestry, crs(geology))

ggplot(geology) + geom_sf() + geom_sf(data = forestry, color = "red")

# Try a new join:
res <- st_join(forestry, geology) 

res <- res %>% mutate(AREA = st_area(res$geometry))

# All the areas are the same
tibble(res) %>% select(-geometry) %>%
  group_by(UID) %>%
  summarize(sd = sd(AREA)) %>%
  summarize(max(sd))

geology %>% filter(UIDsg %in% unique(res$UIDsg)) %>% ggplot() + geom_sf(aes(color = UIDsg)) + scale_color_viridis_c()

# What is happening: There are multiple geology polygons stacked in this location, so they are all getting counted together.

# Pick the geology polygon that is largest and so more likely represents the regional features

sizegeology <- geology %>% filter(UIDsg %in% unique(res$UIDsg))

fixeddups <- tibble(res) %>%
  select(-geometry, - AREA) %>%
  left_join(
    tibble(sizegeology) %>%
      select(UIDsg) %>%
      mutate(AREA = st_area(sizegeology))
  ) %>% 
  arrange(-AREA) %>%
  group_by(UID) %>% 
  slice(c(1)) %>%
  mutate(flagUIDsg = 4) %>% # Assign a new flag for this data set
  select(-AREA)

# Remove the replicated cases, add in the corrected ones, and write to file
ee %>%
  filter(!(UID %in% fixeddups$UID)) %>%
  bind_rows(
    fixeddups
  ) %>% 
  write_rds("Data/surfacegeology_foreach_UID.rds")
