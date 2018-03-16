# ISCN data exploration app functions/setup code #

if(file.exists('./temp/ISCN3.RData')){
  load('./temp/ISCN3.RData')
}else{
  ISCN3 <- processData_ISCN3(verbose=TRUE)
  save(ISCN3, file='./temp/ISCN3.RData')
}

sample.df <- ISCN3$sample
measure.df <- ISCN3$measure
field.df <- ISCN3$field
allmeasures <- unique(measure.df$var)


duplicated <- unique(measure.df$var[duplicated(measure.df$var)])
#paste(duplicated, collapse = "|")
#paste(duplicated, sep = ",", collapse = "',' ") # generated for javascript conditional statement in conditionalPanel

emptykey <- read.csv("EmptyKey.csv")
allvars.df <- emptykey[,c(2,3,5,6)]
var.df <- allvars.df[allvars.df$var %in% allmeasures,] # taking the variables from empty that are actually in ISCN3
namedVars <- setNames(as.character(var.df$var), as.character(var.df$longVar))

colnames(allvars.df) <-
  c(
    "Internal Variable Name",
    "Description",
    "Internal Dataframe",
    "Value type"
  )

######

# just for comparing variables from different sources - not needed for app
keyvars <- emptykey$var[emptykey$dataframe == "sample"]
datumtypes.df <- read.csv("datumtypes.csv")
rowtext <-  str_trim(as.character(datumtypes.df$Rowtext))

compare_lists <- function(list1, list2){
  only1 <- setdiff(list1, list2)
  only2 <- setdiff(list2, list1)
  both <- intersect(list1, list2)
  cat(length(only1), "elements only in", deparse(substitute(list1)), ":", only1, 
             "\n \n", length(only2), "elements only in", deparse(substitute(list2)), ":", only2, 
             "\n \n", length(both), "Intersecting elements: ", both)
}

#compare_lists(allmeasures, rowtext)
#compare_lists(allmeasures, keyvars)





