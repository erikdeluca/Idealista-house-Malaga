colNames = cont_raw[[1]][[indexColMax]] %>% names()
m = matrix(NA, nrow = length(cont_raw[[1]]), ncol = length(colNames))
colnames(m) = colNames
for(r in 1:length(cont_raw[[1]]))
{
  for(c in 1:length(cont_raw[[1]][[r]]))
  # for(c in 1:30)
  {
    print(c)
    if(length(cont_raw[[1]][[r]][[c]])>1)
    {
      for(i in 1:length(cont_raw[[1]][[r]][[c]]))
      {
        if(!names(cont_raw[[1]][[r]][[c]])[i] %in% colNames)
        {
          colNames = c(colNames, names(cont_raw[[1]][[r]][[c]])[i])
          m = cbind(m, rep(NA,length(cont_raw[[1]])))
          colnames(m) = colNames
        }
      }
      for(k in 1:length(cont_raw[[1]][[r]][[c]]))
        m[r,names(cont_raw[[1]][[r]][[c]])[k]] = cont_raw[[1]][[r]][[c]][[k]]
    }else{
      m[r,names(cont_raw[[1]][[r]][c])] = cont_raw[[1]][[r]][[c]][[1]]
    }
  }
}