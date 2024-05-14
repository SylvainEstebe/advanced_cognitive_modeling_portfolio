pacman::p_load(tidyverse,
               here,
               posterior,
               cmdstanr,
               brms, tidybayes)


# load real data
df_real <- read.csv("AlienData.csv")
idxKeep = rep(NA,nrow(df_real))
for (i in 1:nrow(df_real)){
  idxKeep[i] = !grepl("pt",df_real$stimulus[i])
}
df_real = df_real[idxKeep,]


# transform the empirical data
for (i in 1:nrow(df_real)){
  df_real$response[i] = ifelse(df_real$response[i] %in% c(4, 3), 1, 0)
}

# transform name of jpg
  for(i in 1:nrow(df_real)){
    text = df_real$stimulus[i]
    text2 = str_replace_all(text, "\\.(pt|png|bmp|jpe?g)$", "")
    df_real$stimulus[i] = str_replace_all(text2, "pt", "")
    df_real$stimulus[i] = str_replace_all(text2, " ", "")
  }

# put the number in a matrix
A = matrix(nrow = nrow(df_real), ncol = 5)
number = as.integer(df_real$stimulus[2])
as.integer(df_real$stimulus[2])
as.numeric(strsplit(as.character(df_real$stimulus[2]),"")[[1]])
go_to_matrix = as.numeric(strsplit(as.character(number),"")[[1]])

for(i in 1:nrow(df_real)){
  go_to_matrix = as.numeric(strsplit(as.character(df_real$stimulus[i]),"")[[1]])
  for(j in 1:length(go_to_matrix)){
  A[i,j] = go_to_matrix[j]
  }
}
# create data frame for data xp 
data_xp <-  tibble (
  trial = df_real$trial,
  feature = A,
  decision = df_real$response,
  true_category = df_real$dangerous,
  subject = df_real$subject,
  session = df_real$session,
  condition = df_real$condition
)



