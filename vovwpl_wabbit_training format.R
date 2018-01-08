df2$repeater[df2$repeater=="t"]=1
df2$repeater[df2$repeater=="f"]=0
df2$repeater=as.numeric(df2$repeater)
isclassification <- T
movetolast <- function(data, move) {
  data[c(setdiff(names(data), move), move)]
}

movetolast(df2, "repeater")
x="repeater"
df2=df2[c(x, setdiff(names(df2), x))]
head(df2)
str(df2)
labelName=''
isclassification <- T
outcomeName <- 'repeater'
weightName <- ''
objDF <- df2
predictors <- names(objDF)[!names(objDF) %in% c(outcomeName, weightName)]
finalVWfileName <- 'vw1.txt'
labelName=""
########################################################################################

# https://github.com/JohnLangford/vowpal_wabbit/wiki/Input-format
# [Label] [Importance [Tag]]|Namespace Features |Namespace Features ... |Namespace Features

# LABELS & IMPORTANCE
if (is.null(labelName)) {
  outcomeName <- 'ignoreme'
  objDF[,outcomeName] <- "0 |"
} else {
  if (isclassification) {
    # everything should be -1,1 for classification
    objDF[,outcomeName] <- ifelse(objDF[,outcomeName]>0,1,-1)
  }
  
  if (weightName != '')
    objDF[,outcomeName] <- paste(objDF[,outcomeName], objDF[,weightName], "|")
  else
    objDF[,outcomeName] <- paste(objDF[,outcomeName], "|")
}


# Pairing column names with data... adding 1 blank character before each variable
for (i in predictors) {
  objDF[,i] <- ifelse(objDF[,i]==1,paste0(' ',i),
                      ifelse(objDF[,i]==0,'',paste0(' ', i,':', objDF[,i])))
}

# reorder columns so that label (outcome) is first followed by predictors     
objDF <- objDF[c(outcomeName, predictors)]

write.table(objDF, finalVWfileName, sep="", quote = F, row.names = F,  col.names = F)

#######################################################################