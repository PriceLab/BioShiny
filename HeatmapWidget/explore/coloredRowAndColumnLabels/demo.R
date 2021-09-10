library(iheatmapr)
library(datasets)
library(reshape2)

Indometh_matrix <- acast(Indometh, Subject ~ time, value.var = "conc")
Indometh_matrix <- Indometh_matrix[as.character(1:6),]
rownames(Indometh_matrix) <- paste("Patient",rownames(Indometh_matrix))
Indometh_patient_cor <- cor(t(Indometh_matrix))

patient_max_conc <- apply(Indometh_matrix,1,max)
patient_min_conc <- apply(Indometh_matrix,1,min)
patient_groups <- c("A","A","B","A","B","A") # Arbitrary groups

main_heatmap(Indometh_patient_cor,name = "Correlation") %>%
  add_col_clustering() %>%
  add_row_clustering(k = 3) %>%
  add_row_title("Patients") %>%
  add_col_title("Patients") %>%
  add_row_annotation(data.frame("Max" = patient_max_conc,
                                "Min" = patient_min_conc,
                                "Groups" = patient_groups)) %>%
  add_main_heatmap(Indometh_matrix,
                   name = "Indometacin<br>Concentration") %>%
  add_col_labels() %>%
  add_col_title("Time") %>%
  add_col_summary() %>%
  add_row_annotation(data.frame("Groups" = patient_groups))
