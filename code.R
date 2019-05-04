library(readtext)
library(qdap)
library(slam)
library(stringr)
library(tm)
library(SnowballC)
library(data.table)
library(wordcloud)
library(RColorBrewer)
options(warn=-1)

df <- read.table("https://www.utdallas.edu/~sxp175331/CS6301.004_Project2/plot_summaries.txt",header=FALSE,sep='\t',stringsAsFactors = FALSE,quote = "",col.names = c("doc","text"))
movie <- as.data.frame(fread("https://www.utdallas.edu/~sxp175331/CS6301.004_Project2/movie.metadata.tsv", header=FALSE,quote = "", col.names=c("doc", "path", "movie", "date", "col5", "col6", "col7", "col8", "col9")))
movie <- data.frame(movie$doc, movie$movie)
colnames(movie) <- c("doc", "movie")
df<-df[1:1000,]

doc.list<-as.vector(df$text)

N.docs <- length(doc.list)
names(doc.list) <- as.vector(df$doc)
create_wordCloud<-function(term.doc.matrix)
{
  v <- sort(rowSums(term.doc.matrix),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  set.seed(22)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}
preprocessing<-function(my.corpus){
  my.corpus <- tm_map(my.corpus, removePunctuation)
  my.corpus <- tm_map(my.corpus, stemDocument)
  my.corpus <- tm_map(my.corpus, removeNumbers)
  my.corpus <- tm_map(my.corpus, content_transformer(tolower))
  my.corpus <- tm_map(my.corpus, stripWhitespace)
  my.corpus <- tm_map(my.corpus, removeWords, stopwords("english"))
}

search <- function(){
  while(TRUE){
    query <- readline(prompt="Input query (or press q to terminate the program): ")
    if (query == "q"){
      return("Terminating")
    }
    #-----------
    #Creating Corpus
    my.docs <- VectorSource(c(doc.list, query))
    my.docs$Names <- c(names(doc.list), "query")
    
    my.corpus <- Corpus(my.docs)
    
    #------------------------
    #Preprocessing
    preprocessing(my.corpus)
    
    #-------------------------------
    #Creating Vector Space model
    term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
    colnames(term.doc.matrix.stm) <- c(names(doc.list), "query")
    term.doc.matrix <- as.matrix(term.doc.matrix.stm)
    
    
    create_wordCloud(term.doc.matrix)
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
    
    #------------------------------------------------------------------------------
    #We may furthermore normalize each column vector in our tfidf matrix so that its norm is one
    tfidf.matrix <- scale(tfidf.matrix, center = FALSE,
                          scale = sqrt(colSums(tfidf.matrix^2)))
    
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
    merged_result <- merge(results.df, movie)
    merged_result <- merged_result[order(merged_result$score, decreasing = TRUE), ]
    if(merged_result[1,2]!=0){
      results <- merged_result[1:10,c(1,2,4)]
      print(results,row.names = FALSE, qright = FALSE, digits = 2) 
    }
    else{
      print("Query Term not in the documents")
    }
    
  }
}

search()