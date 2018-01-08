
str(test2$repeater)

id="id"
predictors <- names(test2)[!names(test2) %in% id]
finalVWfileName="testvw.vw"
########################################################################################

# https://github.com/JohnLangford/vowpal_wabbit/wiki/Input-format
# [Label] [Importance [Tag]]|Namespace Features |Namespace Features ... |Namespace Features

# LABELS & IMPORTANCE

 
test2[,id] <-  paste("'",test2[,id],"|",sep="")
head(test2[,id] )
for (i in predictors) {
  test2[,i] <- ifelse(test2[,i]==1,paste0(' ',i),
                      ifelse(test2[,i]==0,'',paste0(' ', i,':', test2[,i])))
}

head(test2)

# Pairing column names with data... adding 1 blank character before each variable


# reorder columns so that label (outcome) is first followed by predictors     

write.table(test2,file="vwtest1.txt" , sep="", quote = F, row.names = F,  col.names = F)

#######################################################################


