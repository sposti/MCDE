##carga Bibliotecas

library(readr)
library(knitr)
library(sqldf)


##Definición Importacion desde >>Git
url_archivo <- "https://github.com/sposti/MCDE/blob/main/titanic.zip?raw=TRUE"
archivo <- "titanic.zip"
download.file(url_archivo, archivo, mode = "wb")

##Definicion sub carrpeta extraccion zip
outDir<-"./titanic/"
unzip(archivo,exdir=outDir)

##Ver contenido extraido
archivos <- as.data.frame(list.files(outDir))
colnames(archivos)[1]  <- "col"  
archivos <- sqldf("select col as objeto, case when instr(col, '.') >0 then 'archivo' else 'directorio' end as tipo, 0 as Imported, 0 as Q from archivos order by tipo, objeto")


##importacion Train
df_train <- read.table(paste0(outDir,"train.csv"),
                       header = T,
                       sep = ",",
                       encoding = "UTF8")
##importacion Test
df_test <- read.table(paste0(outDir,"test.csv"),
                      header = T,
                      sep = ",",
                      encoding = "UTF8")


str(df_train)

df_train$Survived <- as.factor(df_train$Survived)
df_train$Pclass <- as.factor(df_train$Pclass)
df_train$Pclass <- factor(df_train$Pclass, levels = c("1", "2","3"))
df_train$Sex <- as.factor(df_train$Sex)

df_train <- sqldf("select *, case when SibSp = 0 and Parch= 0 then '1' else '0' end as ViajaSolo, SibSp+Parch as family_headcount from df_train")


##edades promedio adultos x clase sin NA viajan solos
edades_promedio_viajan_solos <-sqldf("select Pclass, avg(age) as Age from df_train where Age is not NULL and ViajaSolo = 1 group by Pclass")

df_train <- sqldf(c("UPDATE df_train
                      SET Age = (select Age from edades_promedio_viajan_solos)
                      WHERE EXISTS (SELECT 1
                                    FROM edades_promedio_viajan_solos
                                    WHERE edades_promedio_viajan_solos.Pclass = df_train.Pclass
                      AND df_train.Age is null and df_train.viajasolo = 1 
                                    )"
                    , "select * from main.df_train"
)
)

rm("edades_promedio_viajan_solos")

##edad promerdio "Masters" señorito (niños menores)
edad_promedio_masters <- sqldf("select Pclass, avg(Age) Age from df_train where Name like '%Master%' and Age is not null group by Pclass")


df_train <- sqldf(c("UPDATE df_train
                      SET Age = (select Age from edad_promedio_masters)
                      WHERE EXISTS (SELECT 1
                                    FROM edad_promedio_masters
                                    where edad_promedio_masters.Pclass = df_train.Pclass
                                    
                      AND df_train.Age is null and df_train.Name like '%Master%'
                                    )"
                    , "select * from main.df_train"
)
)


rm("edad_promedio_masters")

##edad promerdio "Mrs" señora 
edad_promedio_mrs <- sqldf("select Pclass, avg(Age) Age from df_train where Name like '%Mrs.%' and Age is not null group by Pclass")

df_train <- sqldf(c("UPDATE df_train
                      SET Age = (select Age from edad_promedio_mrs)
                      WHERE EXISTS (SELECT 1
                                    FROM edad_promedio_mrs
                                    where edad_promedio_mrs.Pclass = df_train.Pclass
                                    
                      AND df_train.Age is null and df_train.Name like '%Mrs%'
                                    )"
                    , "select * from main.df_train"
)
)

rm("edad_promedio_mrs")

##edad promerdio "Mr" 
edad_promedio_mr <- sqldf("select Pclass, avg(Age) Age from df_train where Name like '%Mr.%' and Age is not null group by Pclass")

df_train <- sqldf(c("UPDATE df_train
                      SET Age = (select Age from edad_promedio_mr)
                      WHERE EXISTS (SELECT 1
                                    FROM edad_promedio_mr
                                    where edad_promedio_mr.Pclass = df_train.Pclass
                                    
                      AND df_train.Age is null and df_train.Name like '%Mr.%'
                                    )"
                    , "select * from main.df_train"
)
)


rm("edad_promedio_mr")

##edad promerdio "Miss" Headcount = 1
edad_promedio_miss <- sqldf("select Pclass, avg(Age) Age from df_train where Name like '%Miss.%' and Age is not null and family_headcount = 1 group by Pclass")

df_train <- sqldf(c("UPDATE df_train
                      SET Age = (select Age from edad_promedio_miss)
                      WHERE EXISTS (SELECT 1
                                    FROM edad_promedio_miss
                                    where edad_promedio_miss.Pclass = df_train.Pclass
                                    
                      AND df_train.Age is null and df_train.Name like '%Miss.%' and family_headcount = 1
                                    )"
                    , "select * from main.df_train"
)
)


rm("edad_promedio_miss")

##edad promerdio "Miss" Headcount > 1 (menores)
edad_promedio_miss1 <- sqldf("select Pclass, avg(Age) Age from df_train where Name like '%Miss.%' and Age is not null and family_headcount > 1 group by Pclass")

df_train <- sqldf(c("UPDATE df_train
                      SET Age = (select Age from edad_promedio_miss1)
                      WHERE EXISTS (SELECT 1
                                    FROM edad_promedio_miss1
                                    where edad_promedio_miss1.Pclass = df_train.Pclass
                                    
                      AND df_train.Age is null and df_train.Name like '%Miss.%' and family_headcount > 1
                                    )"
                    , "select * from main.df_train"
)
)


rm("edad_promedio_miss1")


df_train <- sqldf("select   *
                            , case  when Age <=12 then 'niños'
                                    when Age <=16 then 'adolescentes'
                                    when Age <60 then 'adultos'
                                    else 'mayores'
                                    end as rango_age
                    from df_train ")

str(df_train)
df_train$rango_age <- as.factor(df_train$rango_age) 
df_train$rango_age <- factor(df_train$rango_age, levels = c("niños", "adolescentes", "adultos", "mayores")) 
df_train$ViajaSolo <- as.factor(df_train$ViajaSolo) 
