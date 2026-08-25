# replay pb-16S-nf classification data from an export of level_7 in qiime viewer
# using taxonomy_barplot_vsearch.qzv
# pb-16S-nf_barplot2heatmap.R

library("tidyr")
library("zoo")
library("pheatmap")
library("RColorBrewer")

level_7 <- read.csv("level-7.csv",
                    row.names=1)
# pivot
# remove leading strings ?__ in row_names
data <- as.data.frame(t(level_7[,1:ncol(level_7)-1]))

# convert to numeric
data <- mutate_all(data, function(x) as.numeric(as.character(x)))

# copy rownames to new data.frame
taxonomy <- data.frame(Taxon = gsub("\\.(.)?__",";",rownames(data), perl=TRUE))
taxonomy$Taxon <- gsub("d__","",taxonomy$Taxon)
 
# expand rownames to 7 columns
taxlevels <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
taxonomy <- separate(data = taxonomy, 
                 col = Taxon, 
                 into = taxlevels,
                 sep = ";",
                 fill = "right")

# replace empty by NA
taxonomy <- taxonomy %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

# propagate last value to the right in all rows
taxonomy <- as.data.frame(t(apply(taxonomy, 1, zoo::na.locf0)))
colnames(taxonomy) <- taxlevels

# merge both data.frames into one
#new.data <- cbind(taxonomy, data)

# plot heatmap with bi-clustering
sel.pal <- colorRampPalette(c("blue","white","red"))(256)
clustmet="average"
scaledir="row"
rdist="euclidean"
cdist="euclidean"
phdata <- log(as.matrix(data)+1,10)
row.names(phdata) <- taxonomy$species

hm.parameters <- list(phdata,
                      color = sel.pal,
                      fontsize = 8,
                      cellwidth = 2, 
                      cellheight = 2,
                      treeheight_row = 100,
                      kmeans_k = NA,
                      show_rownames = TRUE,
                      show_colnames = TRUE,
                      main = "VSearch classification",
                      scale = scaledir,
                      clustering_method = clustmet,
                      cluster_rows = TRUE,
                      cluster_cols = TRUE,
                      clustering_distance_rows = rdist,
                      clustering_distance_cols = cdist,
                      legend = TRUE)
do.call("pheatmap", hm.parameters)
