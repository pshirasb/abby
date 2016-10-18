library("RPostgreSQL")
library("RSQLite")
library("quanteda")


setMethod("dbIsValid", "PostgreSQLConnection", function(dbObj, ...) {
  isValid <- tryCatch({dbGetInfo(dbObj)},
                      error = function(e) NULL)
  !is.null(isValid)  
})

connect = function(dbtype = "postgresql") {

    if(dbtype == "postgresql") {
        # PostgreSQL
        dbname = "skdb"
        host = "swiftkey-db.crx9oopuneks.us-east-1.rds.amazonaws.com"
        port = 5432
        user = "guest"
        pass = "guest"


        # Connect to database
        try({
        drv = NULL
        con = NULL
        drv = dbDriver("PostgreSQL")
        con = dbConnect(drv,
                        dbname = dbname,
                        host   = host,
                        port   = port,
                        user   = user,
                        password = pass)
       })
        
        if(!is.null(drv) & !is.null(con)){
            return(con)
        }

        # revert back to sqlite
        warning("Postgres connection failed. Falling back to Sqlite")
        
    } 

    # SQLite
    drv = dbDriver("SQLite")
    con = dbConnect(drv, "skdb.sqlite")
    
    return(con)
}

getConnection = function() {
    drv      = dbDriver("PostgreSQL")
    con_list = dbListConnections(drv)
    if(length(con_list) > 0) {
        if(checkConnection(con_list[[1]])){
            print("Re-using old connection")
            return(con_list[[1]])
        } else {
            # expired connection, disconnect
            print("invalid connect, disconnecting...")
            disconnect(con_list[[1]])
        }
    }
    print("creating new connection")
    return(connect())
    
}

checkConnection = function(con) {

    # No argument
    if(missing(con)) {
        return(FALSE)
    }

    # Is null
    if(is.null(con)) {
        return(FALSE)
    }

    # Is a valid connection
    if(!dbIsValid(con)) {
        return(FALSE)
    }

    # Is connection still alive
    try({
        res = NULL
        res = dbGetQuery(con, "select * from unigrams limit 1;")
    }, silent = TRUE)
    if(is.null(res)){
        return(FALSE);
    }    


    return(TRUE)

}

disconnect = function(con) {
    try({
        if(dbIsValid(con)) {
            dbDisconnect(con)
        }
    })
}

# Given two words, predict the next
predictWord = function(txt, num = 25, con) {

    # Check the connection first
    if(missing(con)){
        con = getConnection()
    }


    # seperate the last three words from the term + cleaning
    #term = paste(tail(unlist(strsplit(term, " ")),3), collapse = " ")
    txt  = tolower(txt)
    term = tail(unlist(tokenize(
                  x             = txt,
                  removePunct   = T,
                  removeNumbers = T,
                  removeHyphens = T,
                  removeTwitter = T,
                  concatenator  = " "
                  )),
                n = 2
                )    
    wCount  = length(term)
    unigram = term[wCount]
    bigram  = paste(term, collapse = " ")

# NOTE: use this query for probabilities instead of frequnecy
#select term, word, (freq::float) / coalesce((select freq from bigrams where term = 'what' and word = 'thatz'), 1) as prob from trigrams where term = 'what the' order by prob DESC limit 10;

#select term, word, (freq::float) / (select sum(freq) from trigrams where term = 'what the') as prob from trigrams where term = 'what the' order by prob DESC limit 10;

    # Escape quotes for sql queries
    escape = function(x) { gsub("'","''",x) }

    # Query the frequencies
    res   = data.frame() 
    
    # Trigram table
    if(wCount == 2) {
        query = "SELECT term, word, CAST(freq as float) /"
        query = paste0(query, " (SELECT SUM(freq) FROM trigrams WHERE ")
        query = paste0(query, "term = '", escape(bigram), "') as prob ")
        query = paste0(query, "FROM trigrams WHERE term = '")
        query = paste0(query, escape(bigram), "' ORDER BY prob DESC LIMIT ")
        query = paste0(query, num, ";")
        tmp   = dbGetQuery(con, query)
        res   = tmp
    }


    # Bigram table
    if(nrow(res) < num) {
        query = "SELECT term, word, (freq::float) /"
        query = paste0(query, " (SELECT SUM(freq) FROM bigrams WHERE ")
        query = paste0(query, "term = '", escape(unigram), "') as prob ")
        query = paste0(query, "FROM bigrams WHERE term = '")
        query = paste0(query, escape(unigram), "' ORDER BY prob DESC LIMIT ")
        query = paste0(query, num, ";")
        tmp   = dbGetQuery(con, query)
        if(nrow(res)) {
            res = rbind(res, tmp)
        } else {
            res = tmp
        }
    }

    # Unigram table
    if(nrow(res) < num) {
        query = "SELECT word, (freq::float) /"
        query = paste0(query, "(SELECT SUM(freq) FROM unigrams) as prob ")
        query = paste0(query, "FROM unigrams ORDER BY prob DESC LIMIT ")
        query = paste0(query, num, ";")
        tmp   = dbGetQuery(con, query)
        tmp   = cbind(data.frame(term = ""), tmp)

        if(nrow(res)) {
            res = rbind(res, tmp)
        } else {
            res = tmp
        }
   }

    # make "word" column a factor
    if(nrow(res)){
        res = aggregate(prob ~ word, res, mean)
        res = head(res, num)
        res = res[order(res$prob, decreasing = T),] 
        res$word = factor(res$word, levels = res$word)
        rownames(res) = seq_along(res$word)
    }
    
    return(res)
}   

mumble = function(txt, con, n = 10, random = T) {
    
    for(i in 1:n) {
        res = predictWord(txt, con)
           
        if(random) {
            nextWord = sample(res$word, size = 1, prob = res$prob)
            txt = paste(txt,nextWord)
        } else {
            txt = paste(txt, head(res$word,1))
        }
    }
    return(txt)
}

