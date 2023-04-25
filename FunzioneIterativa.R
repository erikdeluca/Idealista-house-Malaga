


# parametri in input

consumer_key <- "xop6jrf8yhw02a514zwhxrhlplvgn4ip"
consumer_secret <- "upiSxn5JmYpy"

#Use basic authentication
secret <- jsonlite::base64_enc(paste(consumer_key, consumer_secret, sep = ":"))
req <- httr::POST("https://api.idealista.com/oauth/token",
                  httr::add_headers(
                    #"Authorization" = paste("Basic", gsub("n", "", secret)),
                    "Authorization" = paste("Basic", secret, sep = " "),
                    "Content-Type" = "application/x-www-form-urlencoded;charset=utf-8"
                  ),
                  body = "grant_type=client_credentials"
)

token <- paste("Bearer", httr::content(req)$access_token)


#url user parameters
x = '36.711376'
y = '-4.428667'
maxItems = '500'
distance = '1000'
type = 'homes'
op = 'sale'
minprice = '100001'
maxprice = '700000'
minsize = '70'
maxsize = '160'


#url fixed parameters
site = 'https://api.idealista.com/3.5/es/search?'
loc = 'center='
country = '&country=es'
maxitems = '&maxItems=50'
pages = '&numPage=1'
dist = '&distance='
property = '&propertyType='
operation = '&operation='
pricefrom = '&minPrice='
priceto = '&maxPrice='
misize = '&minSize='
masize = '&maxSize='
chalet = '&chalet=0'


pagina = 10

for(z in 1:pagina)
{
  # Imposto i scaglioni di prezzo per ogni giro
  minpriceFrat = as.character(as.numeric(minprice) + (z-1)/pagina * (as.numeric(maxprice) - as.numeric(minprice)))
  maxpriceFrat = as.character(as.numeric(minprice) + (z)/pagina * (as.numeric(maxprice) - as.numeric(minprice)))
  # prepara l'url
  url <- paste(site, loc, x, ',', y, country, maxitems, pages, dist, distance,
               property, type, operation, op, pricefrom, minpriceFrat, priceto, maxpriceFrat,
               misize, minsize, masize, maxsize, sep = "")
  
  # invia la richiesta a idealista
  res <- httr::POST(url, httr::add_headers("Authorization" = token))
  
  # extract the JSON content
  cont_raw <- httr::content(res) 
  
  
  # Va a cercare l'item con più colonne
  indexColMax = sapply(1:length(cont_raw[[1]]), function(x) cont_raw[[1]][[x]] %>% names() %>% length) %>% which.max
  colNames = cont_raw[[1]][[indexColMax]] %>% names()
  # Creo una matrice vuota dove imagazzinare i valori
  m = matrix(NA, nrow = length(cont_raw[[1]]), ncol = length(colNames))
  colnames(m) = colNames
  for(r in 1:length(cont_raw[[1]]))
  {
    for(c in 1:length(cont_raw[[1]][[r]]))
    {
      # nel caso l'elemento della lista sia una sotto lista o df vado a spacchettarlo aggiungendo colonne
      if(length(cont_raw[[1]][[r]][[c]])>1)
      {
        # non si può fare in un unico caso
        for(i in 1:length(cont_raw[[1]][[r]][[c]]))
        {
          # se la colonna della sottolista non è già stata aggiunta lo faccio
          if(!names(cont_raw[[1]][[r]][[c]])[i] %in% colNames)
          {
            colNames = c(colNames, names(cont_raw[[1]][[r]][[c]])[i])
            m = cbind(m, rep(NA,length(cont_raw[[1]]))) # aggiunta della colonna
            colnames(m) = colNames
          }
        }
        # inserisco i dati della sottolista
        for(k in 1:length(cont_raw[[1]][[r]][[c]]))
          m[r,names(cont_raw[[1]][[r]][[c]])[k]] = cont_raw[[1]][[r]][[c]][[k]]
      }else{
        tryCatch(
          {
            m[r,names(cont_raw[[1]][[r]][c])] = ifelse(length(cont_raw[[1]][[r]][[c]][[1]])>1,
                                                   cont_raw[[1]][[r]][[c]][[1]][[1]],
                                                   cont_raw[[1]][[r]][[c]][[1]])
            },
          error = function(e) print(e, z, r, c))
      }
    }
  }
  data = m %>% data.frame() %>% tibble()
  
  # debug
  print(c(z,minpriceFrat,maxpriceFrat,data %>% dim))
  
  # merge database
  if(z == 1)
  {
    d = data
  }else
  {
    data[setdiff(names(d), names(data))] <- NA
    d[setdiff(names(d), names(data))] <- NA
    d = bind_rows(d, data)
  }
  
}


data = d %>% tibble




# diversi luoghi




# parametri in input

consumer_key <- "xop6jrf8yhw02a514zwhxrhlplvgn4ip"
consumer_secret <- "upiSxn5JmYpy"

# nuove credenziali

consumer_key = "hpsdfxg76unhb6tgwe38qukjzacwyr3b"
consumer_secret = "Ch7SVGGp4Vp4"

#Use basic authentication
secret <- jsonlite::base64_enc(paste(consumer_key, consumer_secret, sep = ":"))
req <- httr::POST("https://api.idealista.com/oauth/token",
                  httr::add_headers(
                    #"Authorization" = paste("Basic", gsub("n", "", secret)),
                    "Authorization" = paste("Basic", secret, sep = " "),
                    "Content-Type" = "application/x-www-form-urlencoded;charset=utf-8"
                  ),
                  body = "grant_type=client_credentials"
)

token <- paste("Bearer", httr::content(req)$access_token)


#url user parameters
x = '36.71145256718129'
y = '-4.4288958904720355'
# x = '45.643170'
# y = '13.790524'
maxItems = '10000'
distance = '10000'
type = 'homes'
op = 'sale'
minprice = '30001'
maxprice = '200000000'
minsize = '30'
maxsize = '10000'


#url fixed parameters
site = 'https://api.idealista.com/3.5/es/search?'
# site = 'https://api.idealista.com/3.5/it/search?'
loc = 'center='
country = '&country=es'
# country = '&country=it'
maxitems = '&maxItems=50'
pages = '&numPage='
dist = '&distance='
property = '&propertyType='
operation = '&operation='
pricefrom = '&minPrice='
priceto = '&maxPrice='
misize = '&minSize='
masize = '&maxSize='
chalet = '&chalet=0'


pagina = 100

for(z in 1:pagina)
{
  print(z)
  
  # prepara l'url
  url <- paste(site, loc, x, ',', y, country, maxitems, pages, z, dist, distance,
               property, type, operation, op, pricefrom, minprice, priceto, maxprice,
               misize, minsize, masize, maxsize, sep = "")
  
  # invia la richiesta a idealista
  res <- httr::POST(url, httr::add_headers("Authorization" = token))
  
  #extract the JSON content
  cont_raw <- httr::content(res) 
  
  
  # Va a cercare l'item con più colonne
  indexColMax = sapply(1:length(cont_raw[[1]]), function(x) cont_raw[[1]][[x]] %>% names() %>% length) %>% which.max
  colNames = cont_raw[[1]][[indexColMax]] %>% names()
  # Creo una matrice vuota dove imagazzinare i valori
  m = matrix(NA, nrow = length(cont_raw[[1]]), ncol = length(colNames))
  colnames(m) = colNames
  for(r in 1:length(cont_raw[[1]]))
  {
    for(c in 1:length(cont_raw[[1]][[r]]))
    {
      # nel caso l'elemento della lista sia una sotto lista o df vado a spacchettarlo aggiungendo colonne
      if(length(cont_raw[[1]][[r]][[c]])>1)
      {
        # non si può fare in un unico caso
        for(i in 1:length(cont_raw[[1]][[r]][[c]]))
        {
          # se la colonna della sottolista non è già stata aggiunta lo faccio
          if(is.null(names(cont_raw[[1]][[r]][[c]])))
          {
            cont_raw[[1]][[r]][[c]] = cont_raw[[1]][[r]][[c]][[1]] 
          }
          if(!names(cont_raw[[1]][[r]][[c]])[i] %in% colNames)
          {
            colNames = c(colNames, names(cont_raw[[1]][[r]][[c]])[i])
            m = cbind(m, rep(NA,length(cont_raw[[1]]))) # aggiunta della colonna
            colnames(m) = colNames
          }
        }
        # inserisco i dati della sottolista
        for(k in 1:length(cont_raw[[1]][[r]][[c]]))
          m[r,names(cont_raw[[1]][[r]][[c]])[k]] = cont_raw[[1]][[r]][[c]][[k]]
      }else{
        tryCatch(
          {
            m[r,names(cont_raw[[1]][[r]][c])] = ifelse(length(cont_raw[[1]][[r]][[c]][[1]])>1,
                                                       cont_raw[[1]][[r]][[c]][[1]][[1]],
                                                       cont_raw[[1]][[r]][[c]][[1]])
          },
          error = function(e) print(e, z, r, c))
      }
    }
  }
  data = m %>% data.frame() %>% tibble()
  
  # debug
  print(c(z,minprice,maxprice,data %>% dim))
  
  # merge database
  if(z == 1)
  {
    d = data
  }else
  {
    data[setdiff(names(d), names(data))] <- NA
    d[setdiff(names(d), names(data))] <- NA
    d = bind_rows(d, data)
  }
  
  Sys.sleep(1)
  
}

OLDdata = data
OLDdata1 = data
OLDdata2 = data

newData = rbind.data.frame(OLDdata,OLDdata1,OLDdata2)
newData = newData[!duplicated(newData),]

# numeric and factor
newData[,46:49]
colnames(newData[,10:17])
indexNumeric = c(4,5,6,9,19,20,23,30,)
indexFactor = c(7,8,10,11,12,14,15,16,17,18,21,25:29,31,33:42)
newData = newData %>% mutate_at(indexFactor,as.factor)
newData = newData %>% mutate_at(indexNumeric,as.numeric)

dataFactor = newData %>% na.roughfix()




data = d %>% tibble
