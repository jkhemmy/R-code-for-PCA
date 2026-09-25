#packages
library(dplyr)
library(ggplot2)


#import master table from environment --> import dataset

#select variables for PCA

pca_data <- MasterTable %>%
  select(`Av Cu WL`,
         `Av Zn WL`,
         `Av Fe WL`)


# View the data
pca_data


# check the structure of the PCA dataset
str(pca_data)

#standardise elemental data

# Cu, Zn and Fe occur at different concentration ranges.
# Each variable was therefore standardised before PCA so
# that each element contributed equally to the analysis.

pca_scaled <- scale(pca_data)


# View the standardised data
View(pca_scaled)

#perform PCA

pca <- prcomp(
  pca_scaled,
  center = FALSE,
  scale. = FALSE
)

# Summary of variance explained by each principal component
summary(pca)


# PCA loadings
pca$rotation

#extract PCA scores
pca_scores <- as.data.frame(pca$x)


# Add sample metadata from the MasterTable
pca_scores$Sample <- MasterTable$Sample
pca_scores$Cohort <- MasterTable$Cohort
pca_scores$Prep <- MasterTable$Prep
pca_scores$Sex <- MasterTable$Sex
pca_scores$Operator <- MasterTable$Operator


# View PCA scores and associated metadata
head(pca_scores)

#calculate variance & extract percentage of variance for axis labels

pc1_var <- round(
  summary(pca)$importance[2, 1] * 100,
  2
)

pc2_var <- round(
  summary(pca)$importance[2, 2] * 100,
  2
)


pc1_var
pc2_var

#PCA plot by tissue prep

pca_prep <- ggplot(
  pca_scores,
  aes(
    x = PC1,
    y = PC2,
    colour = Prep
  )
) +
  geom_point(size = 3) +
  stat_ellipse(
    aes(group = Prep),
    level = 0.95,
    linewidth = 0.8
  ) +
  theme_classic() +
  labs(
    x = paste0("PC1 (", pc1_var, "%)"),
    y = paste0("PC2 (", pc2_var, "%)"),
    colour = "Preparation"
  )


# Display plot
pca_prep

#PCA plot by operator

pca_operator <- ggplot(
  pca_scores,
  aes(
    x = PC1,
    y = PC2,
    colour = Operator
  )
) +
  geom_point(size = 3) +
  stat_ellipse(
    aes(group = Operator),
    level = 0.95,
    linewidth = 0.8
  ) +
  theme_classic() +
  labs(
    x = paste0("PC1 (", pc1_var, "%)"),
    y = paste0("PC2 (", pc2_var, "%)"),
    colour = "Operator"
  )


# Display plot
pca_operator

#PCA plot by sex

pca_sex <- ggplot(
  pca_scores,
  aes(
    x = PC1,
    y = PC2,
    colour = Sex
  )
) +
  geom_point(size = 3) +
  stat_ellipse(
    aes(group = Sex),
    level = 0.95,
    linewidth = 0.8
  ) +
  theme_classic() +
  labs(
    x = paste0("PC1 (", pc1_var, "%)"),
    y = paste0("PC2 (", pc2_var, "%)"),
    colour = "Sex"
  )


# Display plot
pca_sex

#PCA plot by cohort

pca_cohort <- ggplot(
  pca_scores,
  aes(
    x = PC1,
    y = PC2,
    colour = Cohort
  )
) +
  geom_point(size = 3) +
  stat_ellipse(
    aes(group = Cohort),
    level = 0.95,
    linewidth = 0.8
  ) +
  theme_classic() +
  labs(
    x = paste0("PC1 (", pc1_var, "%)"),
    y = paste0("PC2 (", pc2_var, "%)"),
    colour = "Cohort"
  )


# Display plot
pca_cohort
