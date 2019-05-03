library(readtext)
library(qdap)
library(slam)
library(stringr)
library(tm)
library(SnowballC)
library(SparkR)
args<-commandArgs(TRUE)



df <- read.table("C:/Users/Sushrut/Downloads/MovieSummaries/plot_summaries.txt",header=FALSE,sep='\t',stringsAsFactors = FALSE,quote = "")
df<-df[1:2000,]

mylist <-c()
for(i in 1:nrow(df)){
  mylist<-append(mylist,df[i,2])
}
doc.list<-as.list(mylist)

#df1 <- as.DataFrame("testfile.txt")
#df_title <- data.frame(doc_id = df[,1], text = df[,2])
#doc.list<- as.list(levels(df_title$text))
#print(doc.list[2])
N.docs <- length(doc.list)
for(i in 1:N.docs){
  names(doc.list) <- paste0("doc", df$V1)
}
#names(doc.list) <- paste0("doc", c(1:N.docs))
#names(doc.list) <- paste0("doc", c(1:N.docs))

query <- args[1]
#-----------
#Creating Corpus
my.docs <- VectorSource(c(doc.list, query))
my.docs$Names <- c(names(doc.list), "query")

my.corpus <- Corpus(my.docs)

#------------------------

my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus <- tm_map(my.corpus, stemDocument)
my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, content_transformer(tolower))
my.corpus <- tm_map(my.corpus, stripWhitespace)


#-------------------------------
#Creating Vector Space model
term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
colnames(term.doc.matrix.stm) <- c(names(doc.list), "query")
term.doc.matrix <- as.matrix(term.doc.matrix.stm)

#Function to  define tfidf weights which are defined to be 0 if tf=0
#We consider whenever a term does not occur in a specific document, or when it appears in every document, its weight is zero


get.tf.idf.weights <- function(tf.vec) {
  # Computes tfidf weights from term frequency vector
  n.docs <- length(tf.vec)
  doc.frequency <- length(tf.vec[tf.vec > 0])
  weights <- rep(0, length(tf.vec))
  weights[tf.vec > 0] <- (1 + log2(tf.vec[tf.vec > 0])) * log2(n.docs/doc.frequency)
  return(weights)
}

#-------------------------------------------------------------------------
#Using apply, we run the tfidf weighting function on every row of the term document matrix
#The document frequency is easily derived from each row by the counting the non-zero entries (not including the query).

tfidf.matrix <- t(apply(term.doc.matrix, 1,
                        FUN = function(row) {get.tf.idf.weights(row)}))
colnames(tfidf.matrix) <- colnames(term.doc.matrix)
#tfidf.matrix[0:3, ]

#------------------------------------------------------------------------------
#We may furthermore normalize each column vector in our tfidf matrix so that its norm is one
tfidf.matrix <- scale(tfidf.matrix, center = FALSE,
                      scale = sqrt(colSums(tfidf.matrix^2)))
#tfidf.matrix[0:3, ]

#---------------------------------------------------------------------------------
#Split the Query vector and tfidf matrix

query.vector <- tfidf.matrix[, (N.docs + 1)]
tfidf.matrix <- tfidf.matrix[, 1:N.docs]

#----------------------------------------
#Finding Cosine similarities
#These are simple dot products as our vectors have been normalized to unit length.

doc.scores <- t(query.vector) %*% tfidf.matrix

#----------------------------------------------
#With scores in hand, rank the documents by their cosine similarities with the query vector.

results.df <- data.frame(doc = names(doc.list), score = t(doc.scores),
                         text = unlist(doc.list))
results.df <- results.df[order(results.df$score, decreasing = TRUE), ]
results<- results.df[,c(1,2)]
print(results[1:10,], row.names = FALSE, right = FALSE, digits = 2)