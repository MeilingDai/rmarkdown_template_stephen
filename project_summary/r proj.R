library("tidyverse")
read_csv("data/field_horses_metadata.csv")
field_horses_metadata <- read_csv("data/field_horses_metadata.csv")
field_horses_metadata

read_csv("data/field_horses_raw_counts.csv")
field_horses_raw_counts <- read_csv("data/field_horses_raw_counts.csv")
field_horses_raw_counts

field_horses_raw_counts_modified <- read_csv("data/field_horses_raw_counts_modified.csv")
field_horses_raw_counts_modified


field_horses_metadata_modified <- read_csv("data/field_horses_metadata_modified.csv")
field_horses_metadata_modified


tidy_field_horses_raw_counts <-  field_horses_raw_counts_modified %>%  
  gather(key = horse_id, value = counts, -gene) 
tidy_field_horses_raw_counts

full_data <- full_join(tidy_field_horses_raw_counts, field_horses_metadata_modified)
full_data 
write.csv(full_data,"data/field_horses_full_data.csv")

full_data %>%  ggplot (aes(x = horse_id,
                           y = counts, 
                           color = infection, 
                           group = horse_id))+
  geom_boxplot() + 
  geom_jitter()+
  scale_y_log10()+
  scale_color_manual(values = c(mock = "grey", infected = "red")) +
  theme_bw() +
  labs(title = "microRNA raw counts of field horses")

field_horses_raw_counts <- read_csv("data/field_horses_raw_counts.csv")
field_horses_raw_counts


#use geom_boxplot and geom_jitter to plot microRNA raw counts of field horses
full_data %>%  ggplot (aes(x = horse_id,
                           y = counts, 
                           color = infection, 
                           group = horse_id))+
  geom_boxplot() + 
  geom_jitter()+
  scale_y_log10()+
  scale_color_manual(values = c(mock = "grey", infected = "red")) +
  theme_bw() +
  labs(title = "microRNA raw counts of field horses")


#use geom_boxplot and geom_jitter to plot microRNA raw counts of field horses with raw counts > 10,000.
aw_count_more_than_10000 <- full_data %>% 
  filter(counts > 10000)

raw_count_more_than_10000 %>% ggplot(aes(x = horse_id,
                                         y = counts, 
                                         color = infection, 
                                         group = horse_id))+
  geom_boxplot() + 
  geom_jitter()+
  scale_y_log10()+
  scale_color_manual(values = c(mock = "grey", infected = "red")) +
  theme_bw() +
  labs(title = "microRNA raw counts more than 1000 of 6 horses")


#PCA analysis 
field_horses_raw_counts_PCA <- field_horses_raw_counts %>% 
  column_to_rownames("gene") %>% 
  scale()

# Peform the PCA on the already scaled heat_stress data
microRNA_PCA <- prcomp(field_horses_raw_counts_PCA)

# Check the summary
summary(microRNA_PCA)
plot(microRNA_PCA)





data_1 <- field_horses_raw_counts %>% 
  filter(gene > 0) %>% 
  column_to_rownames("gene") 
data_1



# Transpose the rows and columns
data_2 <- t(data_1) 
data_2
head(data_2)



# Remove first column from data

data_without_gene <- field_horses_raw_counts[,-(1)]
data_without_gene 

# Store GeneID as rownames
rownames(data_without_gene) <- field_horses_raw_counts$gene





View(data_without_gene)
head(data_without_gene)






