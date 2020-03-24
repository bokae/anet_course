## Industry clusters and ERGMs

## solution for TASK 1

# packages
library(dplyr)
library(EconGeo)


# import patent data
pat_df <- data.frame(read.csv('hun-patents.csv', header = TRUE, sep=';'))


# aggregate data for 2010 and 2005
pat_df2010 <- pat_df %>%
  filter(prio_year >= 2006 & prio_year <= 2010) %>%
  select(-prio_year) %>%
  group_by(reg_code, tech) %>%
  summarize(nr_patents = sum(nr_patents))

pat_df2005 <- pat_df %>%
  filter(prio_year >= 2001 & prio_year <= 2005) %>%
  select(-prio_year) %>%
  group_by(reg_code, tech) %>%
  summarize(nr_patents = sum(nr_patents))


# region-technology matrix for 2010 and 2005
mat2010 <- get.matrix(data.frame(pat_df2010))
mat2005 <- get.matrix(data.frame(pat_df2005))


# compute RCA for every technology in every region for 2010 and 2005
rca_mat2010 <- RCA(mat2010)
rca_mat2005 <- RCA(mat2005)


# filter the matrix and construct a data.frame
BK_clusters2010 <- data.frame(rca_mat2010['HU331',])
BK_clusters2010$tech <- rownames(BK_clusters2010)

BK_clusters2005 <- data.frame(rca_mat2005['HU331',])
BK_clusters2005$tech <- rownames(BK_clusters2005)


# issue!
nrow(BK_clusters2005) # 250
nrow(BK_clusters2010) # 314


# merge
BK_clusters <- merge(BK_clusters2010, BK_clusters2005, by='tech', all=TRUE)


# format
colnames(BK_clusters) <- c('tech','rca2010', 'rca2005')


# filter
stable_clusters <- filter(BK_clusters, rca2010 >1 & rca2005>1)
print(stable_clusters)


