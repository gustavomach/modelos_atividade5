# install.packages("reshape2")

library(dplyr)
library(ggplot2)
require(corrplot)
library(reshape2)

dados <-
  read.csv(
    "DADOS_CLARO.csv",
    stringsAsFactors = TRUE,
    sep = ';',
    dec = ','
  )

## target analysis --> IND_CHURN

# ggplot(dados, aes(x = IND_CHURN)) +
#   geom_bar() +
#   geom_text(
#     aes(label = scales::percent(round((..count..) / sum(..count..), 2
#     )),
#     y = ((..count..) / sum(..count..))),
#     stat = "count",
#     vjust = -.25,
#     size = 7
#   ) +
#   ylab("count")

sum(is.na(dados$IND_CHURN))
# we have target information for all observations
# our dataset is highly imbalanced

colnames(dados)

# converting some columns to categorical
# converting all columsn that have date information to categorical
# cliente, plano, DDD_, etc., columns are factor as well


to_factor_cols <-
  c(
    "NO_STATUS_DESDE", # we could use it to calculate the number of days that the client is in a status
    "DAT_ATIVACAO",
    "DAT_INICIO_CARENCIA",
    "DAT_FIM_CARENCIA",
    "data_fatura_mes6",
    "data_fatura_mes7",
    "data_fatura_mes8",
    "data_fatura_mes9",
    "data_fatura_mes10",
    "data_recarga_mes5",
    "data_recarga_mes6",
    "data_recarga_mes7",
    "data_recarga_mes8",
    "data_recarga_mes9",
    "data_recarga_mes10",
    "COD_CPF",
    "CEP",
    "CLIENTE",
    "CLIENTE.1",
    "CLIENTE.2",
    "CLIENTE.3",
    "CLIENTE.4",
    "CLIENTE.5",
    "PLANO",
    # "DDD_CLIENTE_mes5",
    "DDD_CLIENTE_mes6",
    "DDD_CLIENTE_mes7",
    "DDD_CLIENTE_mes8",
    "DDD_CLIENTE_mes9",
    "DDD_CLIENTE_mes10"
  )

dados[to_factor_cols] <- lapply(dados[to_factor_cols], factor)
# sapply(dados, class)

## EDA

nrow(dados)
ncol(dados)

colnames(dados)

# checking if those columns that have the same name followed by .{1-9} are the same

# dados[1:20, c("CLIENTE", "CLIENTE.1", "CLIENTE.2", "CLIENTE.3", "CLIENTE.4", "CLIENTE.5")]
sum(dados$CLIENTE != dados$CLIENTE.1)
sum(dados$CLIENTE != dados$CLIENTE.2)
sum(dados$CLIENTE != dados$CLIENTE.3)
sum(dados$CLIENTE != dados$CLIENTE.4)
sum(dados$CLIENTE != dados$CLIENTE.5)
# So, those columns are the same, we can remove from 1 to 5

# checking if we have duplicated clients id (column CLIENTE) and if we have duplicated CPFs

dup_cliente <- dados %>% 
  group_by(CLIENTE) %>% 
  filter(n()>1)
nrow(dup_cliente)

dup_cpf <- dados %>% 
  group_by(COD_CPF) %>% 
  arrange(COD_CPF) %>% 
  filter(n()>1)
nrow(dup_cpf)

# so, we have duplicated CPFs, but not duplicated client ID. We will use the CLIENTE column as our identifier ad remove the cpf one and believe that those duplicated are honest mistakes
# specially since the rest of the information of those duplicated cpfs rows seems to be completly different

# the id colum doesn't give any information, it jsut exist because of the way that the data was created / exported
# we will remove the CEP variabel as well, because we will not use it to do any geospatial analysis or get more information from other datasource at the moment

cols_remove <- c("id", "CEP", "CLIENTE.1", "CLIENTE.2", "CLIENTE.3", "CLIENTE.4", "CLIENTE.5", "COD_CPF")
dados <- dados[,-which(names(dados) %in% cols_remove)]

# get total of na values by column
na_count <-
  setNames(nm = c('NA_Percent', 'Column'), stack(round(colSums(is.na(
    dados
  )) / nrow(dados), 2)))

na_count <- na_count[order(na_count$NA_Percent, decreasing = TRUE), ]

# na_count[1:10,]

# because we have a large number of columns we can be more aggressive with the removal / exclusion of columns
na_tresh <- 0.7
remove_na_cols <-
  unique(na_count[na_count["NA_Percent"] >= na_tresh, ]$Column)
remove_na_cols

# removing columns based on null values percentage
dados_removed_na_cols <-
  dados[,-which(names(dados) %in% remove_na_cols)]

# ncol(dados)
# ncol(dados_removed_na_cols)
n_cols_remoed = ncol(dados) - ncol(dados_removed_na_cols)
print(paste0("Removemos ", n_cols_remoed, " colunas por terem mais que ", na_tresh*100 , "% de valores nulos"))

# get categorical and numerical variables after removing columns with high % o NA

cols_categ <-
  names(which(sapply(dados_removed_na_cols, class) == "factor"))
length(cols_categ)

cols_numer <-
  names(which(sapply(dados_removed_na_cols, class) != "factor"))
length(cols_numer)

# checking if the columns make sense, or we have to set some as categorical or numerical
cols_categ

cols_numer

# check if there is any column with a single value
summary(dados_removed_na_cols[, cols_numer])
# doesnt seem to be the cause for the numerical variables

apply(dados_removed_na_cols[, cols_categ], 2, function(x) length(unique(x)))
# doesnt seem to be the cause for the categorical variables

# Correlation analysis
# ploting corr matrix

M <- cor(dados_removed_na_cols[, cols_numer],
         use = 'complete.obs')

M[upper.tri(M)] <- 0
# corrplot(M,
#          method = 'number',
#          diag = T,
#          number.cex = 0.8)

corr_tresh <- 0.8
high_corr <- subset(melt(M), value > corr_tresh)
high_corr <- high_corr[order(high_corr$value, decreasing = TRUE), ]
high_corr <- high_corr[high_corr$Var1 != high_corr$Var2,]
high_corr

cols_high_corr <- high_corr$Var1

dados_removed_high_corr <- dados_removed_na_cols[,-which(names(dados_removed_na_cols) %in% cols_high_corr)]

ncol(dados_removed_high_corr)


# cols after removing na and removing high correlation and adding categorical values
cols_final <- c(
  cols_categ, colnames(dados_removed_high_corr)
  )
length(cols_final)
cols_final

data_preproc <- dados[, which(names(dados) %in% cols_final)]
# data_preproc <- dados[, cols_final]
# data_preproc <- subset(dados, cols_final)
  
colnames(data_preproc)
ncol(data_preproc)

###############

# sep into churn and not churn datasets so that we can make train test split and do some sampling technique to deal with the imbalance
data_churn <- data_preproc[data_preproc$IND_CHURN == "S", ]
data_not_churn <- data_preproc[data_preproc$IND_CHURN != "S", ]

(nrow(data_churn) + nrow(data_not_churn)) == nrow(data_preproc)

# separating between train and test
set.seed(42)
size_train <- 0.8
oversample_factor <- 2

# because the target is imbalanced, we must use some technique to deal with that
randomTrainValIndexes_churn <- sample(1:nrow(data_churn), size=size_train*nrow(data_churn))

# train/val and test of the churn
data_churn_train_val <- data_churn[randomTrainValIndexes_churn, ]
data_churn_test  <- data_churn[-randomTrainValIndexes_churn, ] 

# train/val and test of the the not_churn data
randomTrainValIndexes_not_churn <- sample(1:nrow(data_not_churn), size=oversample_factor*nrow(data_churn_train_val))
data_not_churn_train_val <- data_not_churn[randomTrainValIndexes_not_churn, ]
data_not_churn_test <- data_not_churn[-randomTrainValIndexes_not_churn, ]

randomTestIndexes_not_churn <- sample(1:nrow(data_not_churn_test), size=oversample_factor*nrow(data_churn_test))

data_not_churn_test <- data_not_churn_test[randomTestIndexes_not_churn, ]

X_train_val <- rbind(data_churn_train_val, data_not_churn_train_val)
X_test <- rbind(data_churn_test, data_not_churn_test)


X_train_val <- X_train_val[sample(nrow(X_train_val)), ]
X_test <- X_test[sample(nrow(X_test)), ]


# analyzing the train data 

summary(X_train_val)

