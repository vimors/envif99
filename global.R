library(sf)
library(leaflet)
library(dplyr)
library(foreign)
library(readxl)
library(data.table)
library(magrittr)
library(foreign)



# Creamos una lista con el nombre de los estados y su numero indice:
opciones_estado <- c(
  "Ciudad de MÃ©xico"= "09",
  "EdoMex" = "15"  )   

# Abrimos el shape de municipios, que en mi caso tengo guardado en la carpeta muni_2015gw 
mun <- st_read("data/muni_2015gw.shp", quiet = T)

cat_municipio <- read_excel("data/cat_municipio2.xls")
cat_municipio_unique = cat_municipio[!duplicated(cat_municipio$MUN), ]


#### load data ####
 envif <- foreign::read.dbf("data/bases/ENVIF.DBF") %>% setDT()
 fup <- foreign::read.dbf("data/bases/FUP_T.DBF") %>% setDT()
 mhogar <- foreign::read.dbf("data/bases/MHOGAR.DBF") %>% setDT()
 no_ent <- foreign::read.dbf("data/bases/NO_ENT_T.DBF") %>% setDT()
 trh <- foreign::read.dbf("data/bases/TRH_T.DBF") %>% setDT()

 setnames(mhogar, old = "FACTOR", new = "FACTOR_MHOGAR")


 ids <- names(envif[, (names(envif) %in% names(mhogar)), with = FALSE][,1:10])

 complete_table <-
   merge(envif,
         mhogar[,.SD,.SDcols = c(ids, "SEXO", "EDAD", "SABE_LEER", "FACTOR_MHOGAR")],
         all.x = T,
         by = ids)

 complete_table[,
                quinq := ifelse((EDAD == 98 |  EDAD == 99), yes = NaN,
                                no = cut(as.numeric(EDAD),
                                         breaks = seq(0,100, by = 5),
                                        right = T))]


#### Parametros reales,
#### Tomar una muestra del 25% mas
#### Estimar parametros (puntuales y de intervalos 95% conf) del total de los datos

##### Funcion ####


pob_est <-  function(poblacion, confianza, tam_muestra, semilla){
  #poblacion es la variable factor de expansion
  tot_pop = sum(poblacion)
  set.seed(semilla)
  N <- length(poblacion)
  n <- round(tam_muestra * N, 0)

  muestra <- sample(poblacion, size = n)
  est_pob <- round(N * mean(muestra),0)
  var_est <- (N^2)*(1-(n/N))*(var(muestra)/n)

  ci_total <- est_pob + c(-1,1) * qnorm((1-(confianza/2))) * sqrt(var_est)

  outputs <- list(pob_total = tot_pop,
                  pob_estimada = est_pob,
                  lb = round(ci_total[1],0),
                  ub = round(ci_total[2],0),
                  num_registros = N)
  return(outputs)
}


#### 1. Total poblacion ####
# Se usa la base MHOGAR - Residentes del hogar
a = sum(complete_table$FACTOR_MHOGAR)
a
total_pob <- pob_est(poblacion = complete_table$FACTOR_MHOGAR,
                     confianza = 0.05,
                     tam_muestra = 0.25,
                     semilla = 1)


total_pob_2 <- pob_est(poblacion = complete_table[,
                                                  unique(.SD),
                                                  .SDcols = c(ids, "FACTOR")]$FACTOR,
                       confianza = 0.05,
                       tam_muestra = 0.25,
                       semilla = 1)


#### 2. Total poblacion por sexo ####
m_pob <- pob_est(poblacion = complete_table[SEXO == 1]$FACTOR_MHOGAR,
                 confianza = 0.05,
                 tam_muestra = 0.25,
                 semilla = 1)

f_pob <- pob_est(poblacion = complete_table[SEXO == 2]$FACTOR_MHOGAR,
                 confianza = 0.05,
                 tam_muestra = 0.25,
                 semilla = 1)

df <- data.frame(
  group = c("Masculino", "Femenino"),
  value = c(m_pob$pob_total, f_pob$pob_total)
)



#### 3. Total poblacion por edad ####
edad_pob <- complete_table[,pob_est(poblacion = FACTOR_MHOGAR ,
                                    confianza = 0.05,
                                    tam_muestra = 0.25 ,
                                    semilla = 1),
                           by = c("quinq", "SEXO")] %>% .[order(quinq),]
#                           by = "quinq"] %>% .[order(quinq),]

#### 4. Total por actos violentos ####
# en la encuesta se detallan 4 tipos de violencia:
# - Maltrato Emocional
# - Intimidacion
# - Abuso Fisico
# - Abuso Sexual

violencia_names <- grep(pattern = "P[3-6]_[1]",
                        x = names(complete_table), value = T)
violencia_names
# Encontramos la poblacion que contesta no presentar signos de violencia en sus hogares

usuarios_sin_violencia <-
  lapply(violencia_names,
         function(x, DT) {
           grep(pattern = "[1]$", x = DT[, .SD, .SDcols = x] %>% unlist)
         },
         DT = complete_table
  )

names(usuarios_sin_violencia) <- c("Maltrato Emocional", "Intimidacion",
                                   "Abuso Fisico", "Abuso Sexual")

todos_sin_violencia <- Reduce(f = intersect, usuarios_sin_violencia)


# Se utiliza el complemento de la poblacion anterior

total_con_violencia <-
  pob_est(poblacion = complete_table[!todos_sin_violencia, FACTOR_MHOGAR],
          confianza = 0.05,
          tam_muestra = 0.25,
          semilla = 1)


total_sin_violencia <-
  pob_est(poblacion = complete_table[todos_sin_violencia, FACTOR_MHOGAR],
          confianza = 0.05,
          tam_muestra = 0.25,
          semilla = 1)


tipo_violencia <-
  lapply(usuarios_sin_violencia,
         function(x,DT){
           pob_est(DT[!x,]$FACTOR_MHOGAR,
                   confianza = 0.05,
                   tam_muestra = 0.25,
                   semilla = 1)
         },
         DT = complete_table
  )

names(tipo_violencia) <- c("Maltrato Emocional", "Intimidacion",
                           "Abuso Fisico", "Abuso Sexual")

#### 5. Total por actos violentos por sexo ####

total_con_violencia_sexo <-
  complete_table[!todos_sin_violencia,
                 pob_est(poblacion = FACTOR_MHOGAR ,
                         confianza = 0.05,
                         tam_muestra = 0.25 ,
                         semilla = 1),
                 by = "SEXO"]


#### 6. Total por actos violentos por edad ####

total_con_violencia_edad <-
  complete_table[!todos_sin_violencia,
                 pob_est(poblacion = FACTOR_MHOGAR ,
                         confianza = 0.05,
                         tam_muestra = 0.25 ,
                         semilla = 1),
                 by = "quinq"] %>% .[order(quinq),]


#### 7. Total por actos violentos por sexo  y edad ####

total_con_violencia_edad_sexo <-
  complete_table[!todos_sin_violencia,
                 pob_est(poblacion = FACTOR_MHOGAR ,
                         confianza = 0.05,
                         tam_muestra = 0.25 ,
                         semilla = 1),
                 by = c("quinq", "SEXO")] %>% .[order(quinq),]
#####Añadido manuel
total_con_violencia_estrato <-
  complete_table[!todos_sin_violencia,
                 pob_est(poblacion = FACTOR_MHOGAR ,
                         confianza = 0.05,
                         tam_muestra = 0.25 ,
                         semilla = 1),
                 by = c("ESTRATO")] %>% .[order(ESTRATO),]

print("pasa1")
#### 8. y 9. Total por actos violentos por municipio y delegacion ####
total_con_violencia_municipio <-
  complete_table[!todos_sin_violencia,
                 pob_est(poblacion = FACTOR_MHOGAR ,
                         confianza = 0.05,
                         tam_muestra = 0.25 ,
                         semilla = 1),
                 by = c("ENT", "MUNICIPIO")] %>% .[order(ENT, MUNICIPIO),]




setnames(cat_municipio_unique, old = "ENTIDAD", new = "ENT")
setnames(cat_municipio_unique, old = "MUN", new = "MUNICIPIO")

total_con_violencia_df <- select(filter(total_con_violencia_municipio, ENT == "09"),c("ENT", "MUNICIPIO", "pob_total"))
total_con_violencia_df_names <-
  merge(total_con_violencia_df,
        cat_municipio_unique[,c("ENT", "MUNICIPIO", "NOM_MUN")],
        all.x = T,
        by = c("ENT", "MUNICIPIO"))


tipo_violencia_municipio <-
  lapply(usuarios_sin_violencia,
         function(x,DT){
           DT[!x,
              pob_est(poblacion =  FACTOR_MHOGAR,
                      confianza = 0.05,
                      tam_muestra = 0.25,
                      semilla = 1),
              by = c("ENT", "MUNICIPIO")] %>%
             .[order(ENT,MUNICIPIO),]
         },
         DT = complete_table
  )

names(tipo_violencia_municipio) <- c("Maltrato Emocional", "Intimidacion",
                                          "Abuso Fisico", "Abuso Sexual")
#### 10. y 11. Total por actos violentos por municipio y delegacion por sexo####
total_con_violencia_municipio_sexo <-
  complete_table[!todos_sin_violencia,
                 pob_est(poblacion = FACTOR_MHOGAR ,
                         confianza = 0.05,
                         tam_muestra = 0.25 ,
                         semilla = 1),
                 by = c("ENT", "MUNICIPIO", "SEXO")] %>% .[order(ENT, MUNICIPIO),]

#Añadido Manuel

total_con_violencia_municipio_quinq <-
  complete_table[!todos_sin_violencia,
                 pob_est(poblacion = FACTOR_MHOGAR ,
                         confianza = 0.05,
                         tam_muestra = 0.25 ,
                         semilla = 1),
                 by = c("ENT", "MUNICIPIO", "quinq")] %>% .[order(ENT, MUNICIPIO,quinq),]


#### 12. Total por actos violentos por tipo de acto violento por sexo ####

tipo_violencia_sexo <- lapply(usuarios_sin_violencia,
                              function(x,DT){
                                DT[!x,
                                   pob_est(poblacion =  FACTOR_MHOGAR,
                                           confianza = 0.05,
                                           tam_muestra = 0.25,
                                           semilla = 1),
                                   by = "SEXO"]
                              },
                              DT = complete_table
)

#### 13. Total por actos violentos por tipo de acto violento por edad ####

tipo_violencia_edad <- lapply(usuarios_sin_violencia,
                              function(x,DT){
                                DT[!x,
                                   pob_est(poblacion =  FACTOR_MHOGAR,
                                           confianza = 0.05,
                                           tam_muestra = 0.25,
                                           semilla = 1),
                                   by = c("quinq")] %>%
                                  .[order(quinq),]
                              },
                              DT = complete_table
)


#### 14. Total por actos violentos por tipo de acto violento por edad y sexo####

tipo_violencia_sexo_edad <- lapply(usuarios_sin_violencia,
                                   function(x,DT){
                                     DT[!x,
                                        pob_est(poblacion =  FACTOR_MHOGAR,
                                                confianza = 0.05,
                                                tam_muestra = 0.25,
                                                semilla = 1),
                                        by = c("quinq", "SEXO")] %>%
                                       .[order(quinq),]
                                   },
                                   DT = complete_table
)

#### 15. y 19. Total por actos violentos por saber leer y escribir####

total_con_violencia_sabe_leer <-
  complete_table[!todos_sin_violencia,
                 pob_est(poblacion = FACTOR_MHOGAR ,
                         confianza = 0.05,
                         tam_muestra = 0.25 ,
                         semilla = 1),
                 by = c("SABE_LEER")] %>% .[order(SABE_LEER)]



total_sin_violencia_sabe_leer <-
  complete_table[todos_sin_violencia,
                 pob_est(poblacion = FACTOR_MHOGAR ,
                         confianza = 0.05,
                         tam_muestra = 0.25 ,
                         semilla = 1),
                 by = c("SABE_LEER")] %>% .[order(SABE_LEER)]

#### 16. y 20.Total por actos violentos por saber leer y escribir por sexo ####

total_con_violencia_sabe_leer_sexo <-
  complete_table[!todos_sin_violencia,
                 pob_est(poblacion = FACTOR_MHOGAR ,
                         confianza = 0.05,
                         tam_muestra = 0.25 ,
                         semilla = 1),
                 by = c("SABE_LEER", "SEXO")] %>% .[order(SABE_LEER)]

#### 17. 21 Total por actos violentos por tipo de acto violento y saber leer / escribir ####

tipo_violencia_leer <-
  lapply(usuarios_sin_violencia,
         function(x,DT){
           DT[!x,
              pob_est(poblacion =  FACTOR_MHOGAR,
                      confianza = 0.05,
                      tam_muestra = 0.25,
                      semilla = 1),
              by = c("SABE_LEER")] %>%
             .[order(SABE_LEER),]
         },
         DT = complete_table
  )

names(tipo_violencia_leer) <- c("Maltrato Emocional", "Intimidacion",
                                "Abuso Fisico", "Abuso Sexual")

tipo_violencia_leer_municipio <-
  lapply(usuarios_sin_violencia,
         function(x,DT){
           DT[!x,
              pob_est(poblacion =  FACTOR_MHOGAR,
                      confianza = 0.05,
                      tam_muestra = 0.25,
                      semilla = 1),
              by = c("ENT", "MUNICIPIO", "SABE_LEER")] %>%
             .[order(ENT,MUNICIPIO, SABE_LEER),]
         },
         DT = complete_table
  )


#### 18. Total por actos violentos por tipo de acto violento y saber leer / escribir Y sexo ####

tipo_violencia_leer_sexo <-
  lapply(usuarios_sin_violencia,
         function(x,DT){
           DT[!x,
              pob_est(poblacion =  FACTOR_MHOGAR,
                      confianza = 0.05,
                      tam_muestra = 0.25,
                      semilla = 1),
              by = c("SABE_LEER", "SEXO")] %>%
             .[order(SABE_LEER, SEXO),]
         },
         DT = complete_table
  )

names(tipo_violencia_leer_sexo) <- c("Maltrato Emocional", "Intimidacion",
                                     "Abuso Fisico", "Abuso Sexual")

click_id = 0


