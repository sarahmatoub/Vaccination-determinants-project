# Ce fichier contient les fonctions que j'ai utilisé pour extraire les données du Niger
#chargement des packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr",
               "tidyverse")

# Etape 1 : extraction des données du Niger
##### fonction pour ajouter la colonne hhid (identifiant menage unique pour chaque pays) #####
add_hhid = function(data, code_pays){
  data = data %>% mutate(hhid = paste0(code_pays, 
                                       data$grappe, 
                                       "00", data$menage))%>% select(hhid, 
                                                                     grappe, menage, everything()) 
  return(data)
}
add_hhid(data = data_17_me_ner, code_pays = "ner") # ok

# rajouter la colonne hhid 
# base de données sur l'éducation
data_02_men_ner = add_hhid(data = vars_me_s2, code_pays = "ner")
# base de données age, statut martial
data_01_men_ner = add_hhid(data = vars_me_s1, code_pays = "ner")
# base de données sur l'epargne 
data_06_men_ner = add_hhid(data = vars_me_s6, code_pays = "ner")
# base de données logement (menage connecté à un réseau électrique)
data_11_men_ner = add_hhid(data = vars_me_s11, code_pays = "ner")

# section 4 emploi
data_04_men_ner = add_hhid(vars_me_s4, code_pays = "ner")
# base de données sur les entreprises non agricoles
data_10_1_men_ner = add_hhid(vars_me_s10_1, code_pays = "ner")
data_10_2_men_ner = add_hhid(vars_me_s10_2, code_pays = "ner")
data_10_men_ner = left_join(data_10_1_men_ner, data_10_2_men_ner, by = "hhid") %>% select(-grappe.y, -menage.y) %>% rename(menage = menage.x, grappe = grappe.x)
# base de données sur les actifs
data_12_men_ner = add_hhid(vars_me_s12, code_pays = "ner")
# base de données sur l'élevage 
data_17_men_ner = add_hhid(data = vars_me_s17, code_pays = "ner")
# base de données sur la pauvreté
data_20_men_ner = add_hhid(data = vars_me_s20, code_pays = "ner")

########## fonctions individus ##########
#### fonction pour déterminer le genre de l'individu ####
attribgender = function(individ, id){
  gender_info = data_01_men_ner %>%
    filter(hhid == id, id_code == individ) %>%
    pull(sexe)
  
  if (length(gender_info) > 0) {
    return(gender_info == 1)  # Retourne TRUE si (1:homme)
  } else {
    return(NA)  
  }
}

#### fonction pour vérifier si un individu est allé à l'école ####
attribschool = function(individ, id){
  # id : identifiant menage
  # individ : identifiant de l'individu
  school = with(data_02_men_ner, data_02_men_ner[hhid == id & id_code == individ, "a_etudie"])
  return(ifelse(length(school)>0, school==1, NA))
}
attribschool(individ = 4, id = "ner1001") # ok c'est false
attribschool(individ = 2, id = "ner1002") # ok c'est true

#filtrer les données pour un ménage et un individu donné 
#extraire la colonne de l'éducation (niveau actuel), et vérifier si la valeur indique que l'individu est allé à l'école ou pas

#### Fonction pour déterminer si un individu est marié #####
attribmarried = function(individ, id){
  married = with(data_01_men_ner, data_01_men_ner[hhid == id & id_code == individ, "statut_martial"])
  return(ifelse(length(married)>0, married%in%c(2,3), NA)) # 2 :marié monogame, 3 :marié polygame
}
# menage = 1, ind = 4, NA, on retourne FALSE
# menage = 1, ind = 1, 2, on retourne TRUE
attribmarried(individ = 4, id = "ner1001") # ok
attribmarried(individ = 2, id = "ner1001") # ok

#### Fonction pour vérifier si un individu dans un menage donné possède un compte bancaire ####

attribbank = function(individ, id){
  bank = with(data_06_men_ner, data_06_men_ner[hhid == id & id_code == individ, "compte_possession"])
  return(ifelse(length(bank)>0, bank == 1, NA))
}

# vérification
vars_me_s6 %>% select(menage, id_code, compte_possession) %>% filter(compte_possession == 1)
#vars_me_s6 %>% filter(menage == 4, id_code == 1) %>% select(menage, id_code, compte_possession)
attribbank(individ = 1, id = "ner1001") # ok, c'est bien false puisque compte_possession = 0

##### fonction qui détermine si un individu a des économies ####
attribsavings = function(individ, id){
  savings = with(data_06_men_ner, data_06_men_ner[hhid == id & id_code == individ, "compte_epargne"]) 
  return(ifelse(length(savings)>0, savings == 1, NA))
}
# obtenir les codes hhid et id de ceux qui ont un compte epargne
data_06_men_ner %>% filter(compte_epargne == 1) %>% select(hhid, id_code)
attribsavings(individ = 1, id = "ner2004") # ok c'est bien true

data_06_men_ner %>% filter(compte_epargne == 2) %>% select(hhid, id_code) # ceux qui n'ont pas de compte epargne
attribsavings(individ = 1, id = "ner40011") # ok c'est false parfait!

##### fonction détermine si un individu a un emploi ####
# emploi de manière générale
attribjob <- function(individ, id) {
  # Extraction des données pour l'individu spécifié
  job_info <- data_04_men_ner %>%
    filter(hhid == id, id_code == individ)
  
  # Vérifie s'il y a des données pour l'individu
  if (nrow(job_info) == 0) {
    return(NA)  # Aucune donnée pour l'individu
  }
  
  # Détermine si l'individu a un emploi
  has_job <- any(job_info$travail_champs == 1 | 
                   job_info$travail_salarie == 1 | 
                   job_info$travail_remun_com == 1)
  
  return(has_job)
}
attribjob(1, "ner1001") # ok NA
attribjob(2, "ner3005") # ok true
attribjob(6, "ner92009") # ok false

# fonction pour savoir si l'indiv a un business (travaille dans les champs)
attribbusiness <- function(individ, id) {
  # Filtrer les données pour l'individu et le ménage spécifiques
  business_info <- data_04_men_ner %>%
    filter(hhid == id, id_code == individ) %>%
    summarise(
      has_business = any(travail_remun_com == 1, na.rm = TRUE)  # Verifier si l'individu a travaillé pour son propre compte
    ) %>%
    pull(has_business)
  
  # Retourner TRUE si l'individu a une entreprise, FALSE sinon, et NA si aucune donnée n'est disponible
  if (is.na(business_info) || length(business_info) == 0) {
    return(NA)  # Aucune info disponible
  } else {
    return(business_info)
  }
}

# élevage si c'est une activité principale ou secondaire
attribfarming <- function(individ, id, type_elevage, is_principal = TRUE) {
  job_data <- data_04_men_ner %>%
    filter(hhid == id, id_code == individ, code_activite == type_elevage)
  
  if (nrow(job_data) == 0) {
    return(NA)  # Aucune donnée pour cet individu
  }
  
  if (is_principal) {
    has_job <- any(job_data$travail_salarie == 1, na.rm = TRUE)
  } else {
    has_job <- any(job_data$travail_sec == 1, na.rm = TRUE)
  }
  
  return(has_job)
}
# type_elvage 32 caprin, 33 ovins
attribfarming(1, "ner1001", 32) #ok
attribfarming(2, "ner3005", 32) # ok c'est true




# Fonction pour vérifier si un individu a accès à Internet de n'importe quelle manière spécifiée
attribinternet = function(individ, id) {
  # Extraire les données pour l'individu spécifique dans le ménage spécifique
  internet_access = data_01_men_ner %>% 
    filter(hhid == id, id_code == individ) %>%
    select(internet_tel:internet_ecole)
  
  if(nrow(internet_access) > 0) {
    # Vérifie si au moins une des manières d'accès à internet est vraie (1=oui, 0=non)
    return(any(internet_access == 1, na.rm = TRUE))
  } else {
    # Retourne NA s'il a rien trouvé pour cet individu
    return(NA)
  }
}
# test
attribinternet(1, "ner1001") # c'est bien false
attribinternet(2, "ner1002") # c'est bien true

# fonction pour savoir si un individu au sein d'un menage a un telephone
attribphone = function(individ, id){
  phone = with(data_01_men_ner, data_01_men_ner[hhid == id & id_code == individ, "a_telephone"])
  return(ifelse(length(phone) > 0, phone == 1, NA))
}

# test
attribphone(1, "ner1001") # ok c'est false
attribphone(2, "ner1002") # ok c'est true

########## fonctions ménage ##########
####### fonction pour vérifier si le ménage possède un mouton ou une chèvre #####
has_specie <- function(id, species_codes) {
  return(with(data_17_men_ner, any(hhid == id & code_anim %in% c(species_codes) & menage_possede == 1)))
}

has_specie(id = "ner1001", species_codes = 2)
has_specie(id = "ner1001", species_codes = 3)

####### fonction qui détermine si le ménage est connecté à un réseau électrique ##
attrin_reauelect = function(id){
  reseau_elct = with(data_11_men_ner, data_11_men_ner[hhid == id, "reseau_elect_men"])
  return(ifelse(length(reseau_elct) > 0, reseau_elct %in% c(1,2,3), NA))
}

###### fonction qui détermine si le menage est connecté à un réseau de tel ######
attrib_reseaum = function(id){
  phone = with(data_11_men_ner, data_11_men_ner[hhid == id, "reseau_mobile_men"])
  return(ifelse(length(phone)>0, phone == 1, NA))
}
# verification
data_11_men_ner %>% filter(reseau_mobile_men == 1) %>% select(hhid, reseau_mobile_men) #ceux qui captent le reseau mobile, y en a que 2!!

attrib_reseaum(id = "ner224006") #ok!
attrib_reseaum(id = "ner1001") #c'est bien false, ok!

###### fonction qui détermine si le menage est connecté à un réseau électrique (s11q43) ######
attrib_reseau_elect = function(id){
  reseau_elct = with(data_11_men_ner, data_11_men_ner[hhid == id, "reseau_elect_men"])
  return(ifelse(length(reseau_elct)>0, reseau_elct %in% c(1, 2, 3), NA)) # 1 : connecté, 2 : connecté chez un voisin, 3 : directement au poteau
}
data_11_men_ner %>% filter(reseau_elect_men %in% c(1, 2, 3)) %>% select(hhid) #ceux qui sont connectés
attrib_reseau_elect(id = "ner2005") # ok
data_11_men_ner %>% filter(reseau_elect_men == 4) %>% select(hhid) # ceux qui sont pas connectés
attrib_reseau_elect(id = "ner1040012") # ok!

#### fonction qui determine si le menage possede une entreprise non agricole

attrib_entreprise_nonag = function(id){
  entreprise_non_ag = with(data_10_men_ner, data_10_men_ner[hhid == id, "entreprise_nonag_pres"])
  return(ifelse(length(entreprise_non_ag) > 0, entreprise_non_ag == 1, NA))
}
# vérification
data_10_men_ner %>% filter(entreprise_nonag_pres == 1) %>% select(hhid) #ceux qui posèdent une entreprise non agricole
attrib_entreprise_nonag(id = "ner1003") # ok c'est bien true
data_10_men_ner %>% filter(entreprise_nonag_pres == 0) %>% select(hhid)
attrib_entreprise_nonag(id = "ner1001") # ok c'est bien false

###### fonction pour verifier si le menage possede l'un des articles (tv, etc) #####
possede_article <- function(menage, id_articles) {
  # Utiliser ifelse pour vérifier l'existence de l'identifiant du ménage
  possede <- ifelse(menage %in% data_12_men_ner$hhid,
                    nrow(data_12_men_ner %>% filter(hhid == menage,
                                                    id_article %in% id_articles,
                                                    possede_article == 1)) > 0, NA)
  
  return(possede)
}

##### fonction pour calculer le capital immobilier du ménage (wealth) #####
wealth_men = function(id){
  wealth_data = data_12_men_ner %>% filter(hhid == id, id_article %in% c(44, 45)) %>% select(val_fcfa)
  rev_men = sum(wealth_data$val_fcfa, na.rm = TRUE)
  return(rev_men)
}
# verification 
# data_12_men_ner %>% filter(id_article %in% c(44, 45)) %>% select(hhid, id_article ,val_fcfa)

wealth_men(id = "ner1001") # ok 50000
wealth_men(id = "ner1004") # ok 40000

# #### fonction pour calculer le nombre de moutons et chèvres ####
count_animals <- function(menage, spiece_code) {
  animal_count <- data_17_men_ner %>%
    filter(hhid == menage, code_anim == spiece_code) %>%
    summarise(total = sum(taille_troup_men, na.rm = TRUE)) %>%
    pull(total)
  return(animal_count)
}

count_animals(menage = "ner1001", 2) # il n'a pas de moutons
count_animals(menage = "ner1001", 3)



