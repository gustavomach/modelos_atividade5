# install.packages("reshape2")

library(dplyr)
library(ggplot2)
require(corrplot)
library(reshape2)

dados <-
  read.csv(
    "DADOS_CLARO.csv",
    # stringsAsFactors = TRUE,
    sep = ';',
    dec = ','
  )
# dados <- dados[:, 2:]
dados <- dados[,-which(names(dados) == "id")]

## EDA

nrow(dados)
ncol(dados)

colnames(dados)

target <- "IND_CHURN"

# target analysis
# ggplot(dados, aes(x=IND_CHURN)) +
#   geom_bar(aes(y = (..count..)/sum(..count..))) +
#   scale_y_continuous(labels=scales::percent) +
#   ylab("percent")

# (ggplot(dados, aes(x=IND_CHURN)) +
#   geom_bar() +
#   geom_text(
#     aes(label=after_stat(round(count / sum(count) * 100, 1))),
#     stat='count',
#     nudge_y=0.125,
#     size = 5,
#     format_string='({:.1f}%)'
#   )
# )

ggplot(dados, aes(x = IND_CHURN)) +
  geom_bar() +
  geom_text(
    aes(label = scales::percent(round((..count..) / sum(..count..), 2
    )),
    y = ((..count..) / sum(..count..))),
    stat = "count",
    vjust = -.25,
    size = 7
  ) +
  ylab("count")

sum(is.na(dados$IND_CHURN))
# we have target information for all observations

# get total of na values by column
na_count <-
  setNames(nm = c('NA_Percent', 'Column'), stack(round(colSums(is.na(
    dados
  )) / nrow(dados), 2)))

na_count <- na_count[order(na_count$NA_Percent, decreasing = TRUE), ]
na_count[1:10,]

na_tresh <- 0.7
remove_na_cols <-
  unique(na_count[na_count["NA_Percent"] >= na_tresh, ]$Column)
remove_na_cols

# removing columns based on null values percentage
dados_removed_na_cols <-
  dados[,-which(names(dados) %in% remove_na_cols)]

ncol(dados)
ncol(dados_removed_na_cols)
n_cols_remoed = ncol(dados) - ncol(dados_removed_na_cols)
print(paste0("Removemos ", n_cols_remoed, " colunas com por terem mais que ", na_tresh*100 , "% de valores nulos"))

# get categorical and numerical variables
cols_categ <-
  names(which(sapply(dados_removed_na_cols, class) == "factor"))
cols_numer <-
  names(which(sapply(dados_removed_na_cols, class) != "factor"))

# Correlation analysis
# ploting corr matrix
M <- cor(dados_removed_na_cols[, cols_numer],
         use = 'complete.obs')

M[upper.tri(M)] <- 0
corrplot(M,
         method = 'number',
         diag = T,
         number.cex = 0.8)

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

# data_preproc <- dados[, which(names(dados) %in% cols_final)]
data_preproc <- dados[, cols_final]



###############

# removing rows with na in the target
data_preproc <- dados[~is.na(dados$IND_CHURN), ]

nrow(dados)
nrow(data_preproc)

# changing churn value to 0: if not churn and 1 if churn
data_preproc[data_preproc$IND_CHURN == "S", target] <- 1
data_preproc[data_preproc$IND_CHURN == "N", target] <- 0








