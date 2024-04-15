# Ce fichier contient l'etraction de la base de données des ménages qui 
# possèdent des moutons et des chèvres (unité d'observation : le ménage, une ligne par ménage)
# Objectif : faire un script pour automatiser l'extraction des données pour tous les pays
# francophones : Niger, Benin, Burkina Faso, Côte d'Ivoire, Mali, Sénégal et Togo

library(dplyr)
library(tidyverse)

###### questionnaires ménage (concernent plusieurs individus)##########
# section 0 : identification du ménage
data_00_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s00_me_ner2018.csv",
                          header = TRUE, sep = ",")
# table des correspondances pour les régions et departements
table_region = data.frame(region = 1:8, 
                          region_nom = c("agadez", "diffa", "dosso", "maradi", "tahoua", "tillaberi", "zinder", "niamey"))

table_departement = read.csv("original data/ECVMA Niger/2018-2019/Data/departements_niger.csv", 
                             header = TRUE, sep = ";")

# left_join fusionne les donnees des tables de correspondance des regions/depart avec le dataframe de gauche en utilisant les colonnes region/departement
vars_me_id = data_00_me_ner %>% select(grappe,
                                        menage,
                                        s00q01,
                                        s00q02) %>% rename(region = s00q01,
                                                           departement = s00q02) %>% left_join(table_region,
                                                                                               by = "region") %>% left_join(table_departement, 
                                                                                                                            by = "departement")

  


# section 1 : caractéristiques sociodémographiques des membres du ménage
data_01_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s01_me_ner2018.csv", 
                          header=TRUE, sep = ",")

# table de correspondance religion
table_religion = data.frame(religion = 1:5, 
                            religion_menage = c("musulman", "chretien", "animiste", "autre religion", "sans religion"))

table_marrie = data.frame(statut_martial = 1:7,
                          martial_status = c("celibataire", "marie monogame",
                                             "marie polygame", "union libre", "veuf",
                                             "divorce", "separe")
  
)
vars_me_s1 = data_01_me_ner %>% select(grappe,
                                        menage, 
                                        s01q00a, 
                                        s01q01,
                                        s01q02,
                                        s01q03a:s01q03c,
                                        s01q04a,
                                        s01q07,
                                        s01q14,
                                        s01q36,
                                       s01q39__1:s01q39__5) %>% rename(id_code = s01q00a, 
                                                           sexe = s01q01,
                                                           lien_parente = s01q02,
                                                           naissance_jour = s01q03a,
                                                           naissance_mois = s01q03b,
                                                           naissance_annee = s01q03c,
                                                           age_annee = s01q04a,
                                                           statut_martial = s01q07,
                                                           religion = s01q14,
                                                           a_telephone = s01q36,
                                                           internet_tel = s01q39__1,
                                                           internet_bureau = s01q39__2,
                                                           internet_cafe = s01q39__3,
                                                           internet_domicile = s01q39__4,
                                                           internet_ecole = s01q39__5) %>% left_join(table_religion, 
                                                                                            by = "religion") %>% left_join(table_marrie,
                                                                                                                           by = "statut_martial")

# section 10 : entreprises non agricoles
data_10_1_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s10_1_me_ner2018.csv", 
                            header=TRUE, sep = ",")

vars_me_s10_1 = data_10_1_me_ner %>% select(grappe, 
                                             menage, 
                                             s10q11) %>% rename(entreprise_nonag_pres = s10q11)
# entreprise_nonag : présence d'entreprise non agricole (réponse positive à au moins une des questions 2 à 10)
data_10_2_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s10_2_me_ner2018.csv", 
                          header=TRUE, sep = ",")

vars_me_s10_2 = data_10_2_me_ner %>% select(grappe, 
                                             menage, 
                                             s10q22) %>% rename(entreprise_nonag_benef = s10q22)
# section 11 : logement
data_11_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s11_me_ner2018.csv", 
                          header=TRUE, sep = ",")
vars_me_s11 = data_11_me_ner %>% select(grappe, 
                                         menage, 
                                         s11q34,
                                         s11q43,
                                         s11q46) %>% rename(reseau_elect_men = s11q34,
                                                            reseau_mobile_men = s11q43,
                                                            internet_men = s11q46
                                                            )
# section 12 : actifs du ménage
data_12_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s12_me_ner2018.csv", 
                          header=TRUE, sep = ",")

data_article_12 = read.csv("original data/ECVMA Niger/2018-2019/Data/articles_s12_me_ner.csv",
                        header = TRUE, sep = ";")
# artciles qui nous intéressent
articles_interessants <- c(28, 29, 30, 20, 34, 35, 19, 44, 45)
data_article_filtre = data_article_12 %>% filter(id_article %in% articles_interessants)

vars_me_s12 = data_12_me_ner %>% select(grappe, 
                                         menage, 
                                         s12q01, 
                                         s12q02:s12q06,
                                         s12q08,
                                         s12q09) %>% rename(id_article = s12q01,
                                                            possede_article = s12q02, 
                                                            nb_article = s12q03,
                                                            possede_article_men = s12q04, # le bien appartient à un membre du ménage 
                                                            etat_article = s12q06,
                                                            val_fcfa = s12q08, # valeur d'acquisition de l'article
                                                            prix_revente = s12q09) %>% filter(id_article 
                                                                                          %in% articles_interessants, possede_article_men == 1) %>% left_join(data_article_filtre, 
                                                                                                            by = "id_article") # jointure pour ajouter le nom de l'article


# section 14 : shocks
data_14_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s14_me_ner2018.csv", 
                          header=TRUE, sep = ",")
vars_me_s14 = data_14_me_ner %>% select(grappe, 
                                         menage,
                                         s14q00,
                                         s14q01, 
                                         s14q02, 
                                         s14q04d, 
                                         s14q05__1:s14q05__26) %>% rename(id_code = s14q00,
                                                                          choc_nature = s14q01,
                                                                          choc_men = s14q02,
                                                                          choc_me_conseq = s14q04d, # conséquence sur le stock de produits alimentaires
                                                                          ) %>% rename_with(
                                                                            .fn = ~c(
                                                                              "choc_str_epargne",
                                                                              "choc_str_aide_parents_amis",
                                                                              "choc_str_aide_gouvernement",
                                                                              "choc_str_aide_religieuses_ONG",
                                                                              "choc_str_marier_enfants",
                                                                              "choc_str_changement_consommation",
                                                                              "choc_str_achat_aliments_moins_chers",
                                                                              "choc_str_membres_actifs_emplois_supp",
                                                                              "choc_str_membres_adultes_emplois",
                                                                              "choc_str_enfants_travailler",
                                                                              "choc_str_enfants_descolarises",
                                                                              "choc_str_migration",
                                                                              "choc_str_reduction_depenses_sante_educ",
                                                                              "choc_str_obtention_credit",
                                                                              "choc_str_vente_actifs_agricoles",
                                                                              "choc_str_vente_biens_durables",
                                                                              "choc_str_vente_terrain_immeubles",
                                                                              "choc_str_location_terres",
                                                                              "choc_str_vente_stock_vivres",
                                                                              "choc_str_activites_peche",
                                                                              "choc_str_vente_betail",
                                                                              "choc_str_confiage_enfants",
                                                                              "choc_str_activites_spirituelles",
                                                                              "choc_str_culture_contre_saison",
                                                                              "choc_str_autre_strategie",
                                                                              "choc_str_aucune_strategie"
                                                                            ),
                                                                            .cols = starts_with("s14q05__")) %>% filter(choc_nature == 108)
                                         
# 2 : effectif cheptel diminué
# 108 : taux élevé de maladie des animaux

# section 16A : agriculture partie A plantations
data_16a_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s16a_me_ner2018.csv", 
                          header=TRUE, sep = ",")
vars_me_s16a = data_16a_me_ner %>% select(grappe, 
                                           menage,
                                           s16aq04,
                                           s16Aq00) %>% rename(cultive_terre_men = s16Aq00,
                                                               id_code = s16aq04)

# section 17 : elevage
data_17_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s17_me_ner2018.csv", 
                          header=TRUE, sep = ",")

vars_me_s17 = data_17_me_ner %>% select(grappe, 
                                         menage, 
                                         s17q02,
                                         s17q03,
                                         s17q05:s17q13, 
                                         s17q16, 
                                         s17q19:s17q21,
                                         s17q28,
                                         s17q37,
                                         s17q39,
                                         s17q40,
                                         s17q49,
                                         s17q53,
                                         s17q56,
                                         s17q58:s17q61) %>% rename(code_anim = s17q02,
                                                            menage_possede = s17q03,
                                                            taille_troup = s17q05, # combien d'especes au total
                                                            taille_troup_men = s17q06, # combien d'especes qui appartiennent au menage
                                                            prop_anim1 = s17q07a1,
                                                            autre_prop_anim1 = s17q07a4, # variable logique : 1=oui, 2=non
                                                            prop_anim2 = s17q07b1,
                                                            autre_prop_anim2 = s17q07b4,
                                                            prop_anim3 = s17q07c1,
                                                            autre_prop_anim3 = s17q07c4,
                                                            prop_anim4 = s17q07d1,
                                                            autre_prop_anim4 = s17q07d4,
                                                            prop_anim5 = s17q07e1,
                                                            nb_esp_prop1 = s17q07a2,
                                                            nb_esp_prop2 = s17q07b2,
                                                            nb_esp_prop3 = s17q07c2,
                                                            nb_esp_prop4 = s17q07d2,
                                                            nb_esp_prop5 = s17q07e2,
                                                            decide_vente1 = s17q07a3,
                                                            decide_vente2 = s17q07b3,
                                                            decide_vente3 = s17q07c3,
                                                            decide_vente4 = s17q07d3,
                                                            decide_vente5 = s17q07e3,
                                                            qte_achat_esp = s17q08,
                                                            val_achat_esp = s17q09,
                                                            nb_esp_vendu = s17q10,
                                                            decide_vente_esp = s17q11,
                                                            prc_rev_vente_esp = s17q12, #pourcentage de revenu issu de la vente des animaux
                                                            rev_brut_vente_esp = s17q13,
                                                            prod_viande = s17q16, # abattage des animaux
                                                            vente_viande = s17q19, # vente d'une partie de la viande abattue
                                                            prc_rev_vente_viande = s17q20,
                                                            montant_vente_viande = s17q21,
                                                            prod_lait = s17q28, #exploitation les animaux pour production de lait
                                                            prod_lait_tr = s17q37, # transformation d'une partie de la production de lait
                                                            vente_prod_laitiers = s17q39,
                                                            rev_vente_prlait = s17q40,
                                                            nourriture_anim = s17q49, #achat d'aliments pour nourriture d'animaux
                                                            paiement_anim = s17q53, # paiement pour abreuvement des animaux
                                                            vaccine = s17q56, # variable réponse : avez-vous fait vacciné votre troupeau durant les 12 derniers mois?
                                                            deparasit_troup = s17q58,
                                                            deparasit_troup_prx = s17q59,
                                                            soins_troup = s17q60,
                                                            prix_soins_troup = s17q61)
                                                            #  %>% filter((code_anim == 2 |
                                                            #                 code_anim == 3 ) & menage_possede == 1)
# NB : j'ai retiré la colonne s17q00 car elle avait les mêmes valeurs que la colonne s17q03 ( all(vars_me_s17$s17q00 == vars_me_s17$s17q03))

# section 20 : pauvreté
data_20_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s20_me_ner2018.csv", 
                          header=TRUE, sep = ",")
vars_me_s20 = data_20_me_ner %>% select(grappe,
                                         menage,
                                         s20q02:s20q11) %>% rename(
                                           percep_rev_men = s20q02, # estimation niveau de vie revenu menage 
                                           percep_rev_voisins = s20q03,
                                           percep_rev_capitale = s20q04,
                                           percep_rev_classement = s20q05, # classement sur échelle de pauvereté
                                           percep_rev_mttmin = s20q06, # montant minimal niveau de vie descent
                                           diff_dep_loyer = s20q07, # difficultées à payer le loyer
                                           diff_dep_ecl = s20q08, # diff depenses eclerage
                                           diff_dep_maladie = s20q09,
                                           diff_dep_transport = s20q10,
                                           diff_sco_enfts = s20q11
                                         )


#### questionnaires individuels (chaque ligne correspond à un individu) #####

# section 2 : education
data_02_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s02_me_ner2018.csv", 
                          header=TRUE, sep = ",")
vars_me_s2 = data_02_me_ner %>% select(grappe, 
                                       menage, 
                                       s01q00a,
                                       s02q03,
                                       s02q29
                                       ) %>% rename(id_code = s01q00a,
                                                    a_etudie = s02q03,
                                                    niveau_etude_max = s02q29) %>% mutate(niveau_etude_max = 
                                                                                            case_when(
                                                                                              niveau_etude_max == 1 ~ "maternelle",
                                                                                              niveau_etude_max == 2 ~ "primaire",
                                                                                              niveau_etude_max == 3 ~ "secondaire 1 generale",
                                                                                              niveau_etude_max == 4 ~ "secondaire 1 technique",
                                                                                              niveau_etude_max == 5 ~ "secondaire 2 generale",
                                                                                              niveau_etude_max == 6 ~ "secondaire 2 technique",
                                                                                              niveau_etude_max == 7 ~ "post secondaire",
                                                                                              niveau_etude_max == 8 ~ "sup",
                                                                                              TRUE ~ as.character(niveau_etude_max) # pour les non definis
                                                                                            ))

# section 4 : emploi
data_04_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s04_me_ner2018.csv", 
                          header=TRUE, sep = ",")
vars_me_s4 = data_04_me_ner %>% select(grappe,
                                       menage,
                                       s01q00a,
                                       s04q06:s04q08,
                                       s04q11,
                                       s04q16,
                                       s04q30d, # code d'activité de l'entreprise (q30), 32 : élevage caprins, 33 : élevage ovins
                                       s04q32,
                                       s04q43, # salaire pour cet emploi pendant la periode de temps consideree
                                       s04q50,
                                       s04q57,
                                       s04q58) %>% rename(id_code = s01q00a,
                                                          travail_champs = s04q06,
                                                          travail_remun_com = s04q07,
                                                          travail_salarie = s04q08,
                                                          travail_non_exerce = s04q11,
                                                          source_rev_prin = s04q16,
                                                          code_activite = s04q30d,
                                                          duree_emploi = s04q32,
                                                          salaire_emploi = s04q43,
                                                          travail_sec = s04q50, #emploi secondaire
                                                          cat_sociopro = s04q57,
                                                          salaire_emploi2 = s04q58
                                                          ) %>% filter(code_activite %in% c(32, 33))

# section 6 : épargne
data_06_me_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s06_me_ner2018.csv", 
                          header=TRUE, sep = ",")
vars_me_s6 = data_06_me_ner %>% select(grappe,
                                       menage,
                                       s01q00a,
                                       s06q01__1,
                                       s06q02,
                                       s06q03,
                                       s06q06,
                                       s06q06_autre,
                                       s06q11,
                                       s06q14) %>% rename(id_code = s01q00a,
                                                          compte_possession = s06q01__1, #compte en banque classique
                                                          compte_epargne = s06q02,
                                                          demande_credit = s06q03,
                                                          obtention_credit = s06q06, #raison pourlaquelle il n'a pas obtenu de crédit
                                                          utilisation_credit = s06q11,
                                                          montant_dernier_credit = s06q14)


######### questionnaires commuautaires (concernent plusieurs ménages)##########

# section 1 : caracteristiques générales du quartier/ village
# chargement data section1
data_01_co_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s01_co_ner2018.csv", 
                          header=TRUE, sep = ",")
# selection des variables section1
vars_co_s1 = data_01_co_ner %>% select(grappe, 
                                       s01q05:s01q11, 
                                       s01q13,
                                       s01q14__4) %>% rename(distance_ville_pr = s01q05, #en km
                                                          voie_acces_pr = s01q06, # principale voie d'acces au village/quartier
                                                          voie_acces_autre = s01q06_autre,
                                                          transp_dispo = s01q07, #moyen de transport en commun motorisé
                                                          transp_moto = s01q08__1, 
                                                          transp_voiture = s01q08__2, #taxi/voiture
                                                          transp_train = s01q08__3,
                                                          transp_pirogue = s01q08__4,
                                                          transp_present = s01q08__5,
                                                          moy_transp_pr = s01q09,
                                                          moy_transp_pr_freq = s01q10,
                                                          moy_transp_pr_freq_autre = s01q10_autre,
                                                          reseau_elect = s01q11, #disponibilité de reseau de distribution electrique
                                                          reseau_mobile = s01q13,
                                                          presence_cs = s01q14__4 # comité de santé
                                                          ) 
# section 2 : existence et accessibilité aux services sociaux
# chargement donnees section2
data_02_co_ner = read.csv("original data/ECVMA Niger/2018-2019/Data/s02_co_ner2018.csv",
                          header = TRUE, sep = ",")
# selection variables section2
vars_co_s2 = data_02_co_ner %>% select(grappe,
                                       s02q01__5:s02q01__7,
                                       s02q01__12, 
                                       s02q01__18,
                                       s02q01__20, 
                                       s02q02:s02q04__10) %>% rename(service_hopital = s02q01__5,
                                                              service_autre_sante = s02q01__6,
                                                              service_cabinet = s02q01__7,
                                                              service_banque = s02q01__12, #banque / institution micro finance
                                                              service_cea = s02q01__18, # centre d'encadrement agricole
                                                              service_bab = s02q01__20, # banque d'aliments pour le bétail
                                                              service_transport = s02q02, #principal moyen de transport
                                                              service_transport_autre = s02q02_autre,
                                                              service_temps_moy = s02q03, 
                                                              service_pb_aucun = s02q04__0,
                                                              service_pb_eloignmnt = s02q04__1,
                                                              service_pb_insf = s02q04__2, #personnel insuffisant
                                                              service_pb_qualif = s02q04__3, #personnel peu qualifié
                                                              service_pb_att = s02q04__4, # longue attente
                                                              service_pb_acc = s02q04__5, # mauvais accueil
                                                              service_pb_lieu = s02q04__6, # insalubrité lieux
                                                              service_pb_acces = s02q04__7, 
                                                              service_pb_inf = s02q04__8, # infrastructure en mauvais état
                                                              service_pb_prx = s02q04__9, # service cher
                                                              service_pb_corr = s02q04__10 # corruption
                                                              )


####### fichiers supplementaires ########                                                                                                                                                       
# poids des grappes 
poids = read.csv("original data/ECVMA Niger/2018-2019/Data/ehcvm_ponderations_ner2018.csv", 
                 header = TRUE, sep = ",")
poids = poids %>% rename(poids_grappe = hhweight) #inutile je pense

# coordonnées gps 
gps_grappe = read.csv("original data/ECVMA Niger/2018-2019/Data/grappe_gps_ner2018.csv", 
                      header = TRUE, sep = ",")

# welware : bien etre ds menages
welfware = read.csv("original data/ECVMA Niger/2018-2019/Data/ehcvm_welfare_ner2018.csv",
                    header = TRUE, sep = ",")

# infos sur le menage 
ehcvm_menage = read.csv("original data/ECVMA Niger/2018-2019/Data/ehcvm_menage_ner2018.csv",
                        header = TRUE, sep = ",")

# pauvrete individus 
ehcvm_ind = read.csv("original data/ECVMA Niger/2018-2019/Data/ehcvm_individu_ner2018.csv",
                     header = TRUE, sep = ",")

# conso niveau menage 
ehcvm_cons = read.csv("original data/ECVMA Niger/2018-2019/Data/ehcvm_conso_ner2018.csv",
                      header = TRUE, sep = ",")

######### création de tableaux avec les infos qu'on veut #######
source("source/functions.R")

###### data household head ########
# infos sur l'age, statut martial, religion, tel, internet
chef_menages = data_01_men_ner %>% filter(lien_parente == 1) %>% select(hhid, 
                                                                        grappe, 
                                                                        id_code, 
                                                                        sexe, 
                                                                        age_annee, 
                                                                        martial_status, 
                                                                        religion_menage, 
                                                                        a_telephone,
                                                                        starts_with("internet_")) %>% rename(head_sexe = sexe,
                                                                                                    head_age = age_annee,
                                                                                                    head_martial_status = martial_status,
                                                                                                    head_religion = religion_menage) %>% mutate(head_sexe = case_when(
                                                                                                      head_sexe == 1 ~ "homme", TRUE ~ "femmme"
                                                                                                    ),
                                                                                                    head_phone = as.logical(a_telephone == 1),
                                                                                                    head_internet = as.logical(internet_tel == 1 | 
                                                                                                                                 internet_bureau == 1 | 
                                                                                                                                 internet_cafe == 1 | 
                                                                                                                                 internet_domicile == 1 | 
                                                                                                                                 internet_ecole == 1))

# infos sur l'education 
chef_menage2 = chef_menages %>% left_join(data_02_men_ner, by = c("hhid", "id_code")) %>% select(hhid, id_code, menage, grappe.x, starts_with("head_"), a_etudie, niveau_etude_max)
chef_menage2 = chef_menage2 %>% rename(grappe = grappe.x, head_education = a_etudie, head_educlevel_max = niveau_etude_max) %>% mutate(head_education = case_when(head_education == 1 ~"TRUE", TRUE ~ "FALSE"))

# infos sur l'emploi
chef_menage4 = chef_menage2 %>%
  left_join(data_04_men_ner, by = c("hhid", "id_code")) %>%
  mutate(
    head_travail_champs = as.logical(travail_champs == 1),
    head_travail_remun_com = as.logical(travail_remun_com == 1),
    head_travail_salarie = as.logical(travail_salarie == 1),
    head_travail_non_exerce = as.logical(travail_non_exerce == 1),
    head_source_rev_prin = case_when(
      source_rev_prin == 1 ~ "Pension",
      source_rev_prin == 2 ~ "Loyers",
      source_rev_prin == 3 ~ "Bourse/Transfert",
      source_rev_prin == 4 ~ "Epargne",
      source_rev_prin == 5 ~ "Recoltes",
      source_rev_prin == 6 ~ "Dons de nourriture",
      source_rev_prin == 7 ~ "Soutient familial",
      source_rev_prin == 8 ~ "Mendicité",
      source_rev_prin == 9 ~ "Autre",
      TRUE ~ "NA"
    ),
    head_bovin_farming = code_activite == 32,
    head_caprin_farming = code_activite == 33,
    head_salaire_emploi = salaire_emploi,
    head_salaire_emploi2 = salaire_emploi2
  ) %>%
  select(
    hhid, id_code, grappe.x, menage.x, -grappe.y, -menage.y, starts_with("head_") 
  ) %>% rename(grappe = grappe.x, menage = menage.x)

# infos sur l'épargne
chef_menage6 = chef_menage4 %>% left_join(data_06_men_ner, by = c("hhid", "id_code")) %>%
  mutate(head_bank = as.logical(compte_possession),
         head_credit = as.logical(compte_epargne),
         head_credit_demand = as.logical(demande_credit),
         head_credit_obtention = as.logical(obtention_credit),
         head_credit_last = montant_dernier_credit,
         head_credit_util = case_when(
           utilisation_credit == 1 ~ "Education",
           utilisation_credit == 2 ~ "Sante",
           utilisation_credit == 3 ~ "Equipement menage",
           utilisation_credit == 4 ~ "Aqcuisition terrain",
           utilisation_credit == 5 ~ "Entreprise",
           utilisation_credit == 6 ~ "Equipement matieres premieres",
           utilisation_credit == 7 ~ "Intrants agricoles",
           utilisation_credit == 8 ~ "Consommation menage",
           utilisation_credit == 9 ~ "Evenements",
           utilisation_credit == 10 ~ "Chocs",
           TRUE ~ "NA"
         )
         ) %>% select(hhid, id_code, grappe.x, menage.x, -grappe.y, -menage.y, starts_with("head_")) %>%
  rename(grappe = grappe.x, menage = menage.x)

##### infos sur les communautés (plusieurs ménages) ####
# base de données infos de base sur les ménages
data_00_men_ner = add_hhid(data = vars_me_id, code_pays = "ner")
data_menages = vars_co_s1 %>% left_join(vars_co_s2, by = "grappe") %>% distinct(grappe, .keep_all = TRUE) 
data_menages1 = data_menages %>% 
  mutate(transp_dispo = as.logical(transp_dispo == 1),
         transp_moto = as.logical(transp_moto == 1),
         transp_voiture = as.logical(transp_voiture == 1),
         transp_pirogue = as.logical(transp_pirogue == 1),
         transp_train = as.logical(transp_train == 1),
         transp_present = as.logical(transp_present == 1),
         moy_transp_pr = case_when(
           moy_transp_pr == 1 ~ "Moto/Tricyle",
           moy_transp_pr == 2 ~ "Taxi/Car",
           moy_transp_pr == 3 ~ "Train",
           moy_transp_pr == 4 ~ "Pirogue",
           TRUE ~ "NA"
         ),
         reseau_elect = as.logical(reseau_elect == 1),
         reseau_mobile = as.logical(reseau_mobile == 1),
         presence_cs = as.logical(presence_cs == 1),
         moy_transp_pr_freq = case_when(
         moy_transp_pr_freq == 1 ~"Plusieurs fois par jour",
         moy_transp_pr_freq == 2 ~"Une fois par jour",
         moy_transp_pr_freq == 3 ~"Plusieurs fois par semaine",
         moy_transp_pr_freq == 4 ~"Une fois par semaine",
         moy_transp_pr_freq == 5 ~"Plusieurs fois tous 15j",
         moy_transp_pr_freq == 6 ~"Plusieurs fois par mois",
         moy_transp_pr_freq == 7 ~"Autre",
         TRUE ~"NA"
         ),
         voie_acces_pr = case_when(
           voie_acces_pr == 1 ~ "Route goudronnee",
           voie_acces_pr == 2 ~ "Route en laterite",
           voie_acces_pr == 3 ~ "Piste",
           voie_acces_pr == 4 ~ "Voie maritime",
           voie_acces_pr == 5 ~ "Voie ferree",
           voie_acces_pr == 6 ~"Autre",
           TRUE ~ "NA"),
         service_hopital = as.logical(service_hopital == 1),
         service_autre_sante = as.logical(service_autre_sante == 1),
         service_cabinet = as.logical(service_cabinet == 1),
         service_banque = as.logical(service_banque == 1),
         service_cea = as.logical(service_cea == 1),
         service_bab = as.logical(service_bab == 1),
         service_pb_aucun = as.logical(service_pb_aucun == 1),
         service_pb_eloignmnt = as.logical(service_pb_eloignmnt == 1),
         service_pb_insf = as.logical(service_pb_insf == 1),
         service_pb_qualif = as.logical(service_pb_qualif == 1),
         service_pb_att = as.logical(service_pb_att == 1),
         service_pb_acc = as.logical(service_pb_acc == 1),
         service_pb_lieu = as.logical(service_pb_lieu == 1),
         service_pb_acces = as.logical(service_pb_acces == 1),
         service_pb_inf = as.logical(service_pb_inf == 1),
         service_pb_prx = as.logical(service_pb_prx == 1),
         service_pb_corr = as.logical(service_pb_corr == 1),
         service_transport = case_when(
           service_transport == 1 ~ "Pieds",
           service_transport == 2 ~ "Velo/Bicylette",
           service_transport == 3 ~ "Moto/Tricycle",
           service_transport == 4 ~ "Voiture/Car",
           service_transport == 5 ~ "Charette",
           service_transport == 6 ~ "Animaux",
           service_transport == 7 ~ "Pirogue",
           service_transport == 8 ~ "Autre",
           TRUE ~ "NA"
         )
  )

# fusionner avec les infos sur la région/ département
data_menages2 = data_00_men_ner %>% left_join(data_menages1, by = "grappe") %>% 
  select(hhid:region, region_nom, departement, departement_nom, everything()) 

##### infos sur les menages ####### 
# section 10 (entreprises non agricoles), 
#actifs du menage,(s12), logement (11), chocs et stratégies de survie (14), pauvreté subjective (20)

# base de données sur les actifs du ménage
data_actifs = data_12_men_ner %>% filter(possede_article == 1) %>%
  select(hhid:id_article, nom_article, everything()) %>%
  mutate(etat_article = case_when(etat_article == 1 ~ "Neuf",
                                  etat_article == 2 ~ "Occasion",
                                  TRUE ~ "NA")) %>% 
  mutate(household_tv = any(id_article == 20),
         household_transp = any(id_article %in% c(28, 29, 30)),
         household_radio = any(id_article == 19),
         household_tel_port = any(id_article == 35),
         household_tel_fixe = any(id_article == 34)
  ) %>% select(-possede_article_men, -id_article, -possede_article, -nom_article) %>% distinct(hhid, .keep_all = TRUE)

# base de données sur le logement du ménage
data_logement = data_11_men_ner %>% 
  mutate(reseau_elect_men = as.logical(reseau_elect_men %in% c(1,2,3)),
         reseau_mobile_men = as.logical(reseau_mobile_men == 1),
         internet_men = as.logical(internet_men == 1)
         )

# base de données sur les chocks
data_14_men_ner = add_hhid(vars_me_s14, code_pays = "ner") 

data_chocs <- data_14_men_ner %>%
  mutate(choc_men = as.logical(choc_men),
    across(
      starts_with("choc_str_"),
      ~ as.logical(.x %in% c(1,3))  
    ),
    choc_men = as.logical(choc_men), 
    choc_me_conseq = case_when(
      choc_me_conseq == 1 ~ "Augmente",
      choc_me_conseq == 2 ~ "Diminue",
      choc_me_conseq == 3 ~ "Inchange",
      choc_me_conseq == 4 ~ "Non concerne",
      TRUE ~ "NA"
      
    )
  )

# base de donnée sur la pauvreté
data_pauv = data_20_men_ner %>% 
  mutate(percep_rev_men = case_when(
    percep_rev_men == 1 ~ "Bien",
    percep_rev_men == 2 ~ "Assez bien",
    percep_rev_men == 3 ~ "Passablement",
    percep_rev_men == 4 ~ "Difficilement",
    percep_rev_men == 5 ~ "Ne sait pas",
    TRUE ~ "NA"
  ),
  percep_rev_voisins = case_when(
    percep_rev_voisins == 1 ~ "Nettement mieux",
    percep_rev_voisins == 2 ~ "Un peu mieux",
    percep_rev_voisins == 3 ~ "Pareillement",
    percep_rev_voisins == 4 ~ "Moins bien",
    percep_rev_voisins == 5 ~ "Ne sait pas",
    percep_rev_voisins == 6 ~ "Non-concerné",
    TRUE ~ "NA"
  ),
  percep_rev_capitale = case_when(
    percep_rev_capitale == 1 ~ "Nettement mieux",
    percep_rev_capitale == 2 ~ "Un peu mieux",
    percep_rev_capitale == 3 ~ "Pareillement",
    percep_rev_capitale == 4 ~ "Moins bien",
    percep_rev_capitale == 5 ~ "Ne sait pas",
    percep_rev_capitale == 6 ~ "Non-concerné",
    TRUE ~ "NA"
  ),
  percep_rev_classement = case_when(
    percep_rev_classement == 1 ~ "Riche",
    percep_rev_classement == 2 ~ "Moyen",
    percep_rev_classement == 3 ~ "Pauvre",
    percep_rev_classement == 4 ~ "Très pauvre",
    percep_rev_classement == 5 ~ "Ne sait pas",
    TRUE ~ "NA"
  ),
  # dificultés dans les depenses
  diff_dep_loyer = as.logical(diff_dep_loyer == 1),
  diff_dep_ecl = as.logical(diff_dep_ecl == 1),
  diff_dep_maladie = as.logical(diff_dep_maladie == 1),
  diff_dep_transport = as.logical(diff_dep_transport == 1),
  diff_sco_enfts = as.logical(diff_sco_enfts == 1)
  )

data_menage_infos = data_actifs %>%
  left_join(data_logement, by = "hhid") %>%
  left_join(data_pauv, by = "hhid") %>% 
  left_join(data_chocs, by = "hhid") %>%
  rename(menage = menage.x, grappe = grappe.x) %>%
  select(-grappe.y, -menage.y, -id_code, -grappe.x.x, -menage.x.x, -grappe.y.y, -menage.y.y) %>%
  left_join(data_10_men_ner, by = "hhid") %>%
  mutate(entreprise_nonag_pres = as.logical(entreprise_nonag_pres == 1)) %>%
  rename(menage = menage.x, grappe = grappe.x) %>%
  select(-grappe.y, -menage.y)%>%
  #select(-grappe.y, -menage.y, -grappe.x.x, -menage.x.x, -grappe.y.y, -menage.y.y) %>%
  select(hhid, grappe, menage, starts_with("household_"), everything()) %>%
  mutate(wealth = map_dbl(hhid, wealth_men))


####### sélection des ménages qui ont des moutons et des chèvres #########
# Préparation des identifiants de ménage avec moutons et/ou chèvres

menages_moutons_chevres = data_17_men_ner %>% filter(code_anim %in% c(2,3), menage_possede == 1) 
id_menages_moutons_chevres = menages_moutons_chevres %>% select(hhid) %>% distinct() %>% pull(hhid)

#### infos sur les proprios des animaux #####

test_proprios3 = menages_moutons_chevres %>%
  rowwise() %>%
  mutate(
    possede_mouton = has_specie(hhid, 2),
    possede_chevre = has_specie(hhid, 3),
    owner_ids = list(na.omit(c_across(starts_with("prop_anim")))),  # id des proprios sans NA
    # Attributs pour les proprios de chèvres 
    owner_head_goat = as.logical(if_else(possede_chevre, any(owner_ids == 1), FALSE)),
    # au moins un des proprios est un homme
    owner_man_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~attribgender(.x, hhid) == TRUE), na.rm = TRUE), NA),
    # au moins un des proprios est une femme
    owner_woman_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~attribgender(.x, hhid) == FALSE), na.rm = TRUE), NA),
    owner_married_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~attribmarried(.x, hhid), na.rm = TRUE)), NA),
    owner_school_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~attribschool(.x, hhid), na.rm = TRUE)), NA),
    owner_bank_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~attribbank(.x, hhid), na.rm = TRUE)), NA),
    owner_savings_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~attribsavings(.x, hhid), na.rm = TRUE)), NA),
    owner_phone_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~attribphone(.x, hhid), na.rm = TRUE)), NA),
    owner_internet_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~attribinternet(.x, hhid), na.rm = TRUE)), NA),
    owner_job_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~attribjob(.x, hhid), na.rm = TRUE)), NA),
    owner_business_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~attribbusiness(.x, hhid), na.rm = TRUE)), NA),
    owner_farming_pr_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~attribfarming(.x, hhid, 33, TRUE), na.rm = TRUE)), NA),
    owner_farming_sec_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~attribfarming(.x, hhid, 32, FALSE), na.rm = TRUE)), NA),
    owner_num_species_goat = if_else(possede_chevre, sum(map_dbl(owner_ids, ~ sum(c_across(starts_with("nb_esp_prop"))[.x]))), NA),
    owner_decision_sale_goat = if_else(possede_chevre, any(map_lgl(owner_ids, ~ any(c_across(starts_with("decide_vente"))[.x]))), NA),
    owner_total_sale_value_goat = if_else(possede_chevre, sum(map_dbl(owner_ids, ~ sum(c_across(starts_with("val_achat_esp"))[.x], c_across(starts_with("rev_brut_vente_esp"))[.x]))), NA),
    owner_milk_production_goat = if_else(possede_chevre, sum(map_dbl(owner_ids, ~ sum(c_across(starts_with("prod_lait"))[.x]))), NA),
    owner_milk_sale_revenue_goat = if_else(possede_chevre, sum(map_dbl(owner_ids, ~ sum(c_across(starts_with("rev_vente_prlait"))[.x]))), NA),
    owner_healthcare_expenditure_goat = if_else(possede_chevre, sum(map_dbl(owner_ids, ~ sum(c_across(starts_with("prix_soins_troup"))[.x]))), NA),
    
    # Attributs pour les proprios de moutons
    owner_head_sheep = as.logical(if_else(possede_mouton, any(owner_ids == 1), FALSE)),
    # au moins un des proprios est un homme
    owner_man_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~attribgender(.x, hhid) == TRUE), na.rm = TRUE), NA),
    # au moins un des proprios est une femme
    owner_woman_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~attribgender(.x, hhid) == FALSE), na.rm = TRUE), NA),
    owner_married_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~attribmarried(.x, hhid), na.rm = TRUE)), NA),
    owner_school_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~attribschool(.x, hhid), na.rm = TRUE)), NA),
    owner_bank_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~attribbank(.x, hhid), na.rm = TRUE)), NA),
    owner_savings_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~attribsavings(.x, hhid), na.rm = TRUE)), NA),
    owner_phone_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~attribphone(.x, hhid), na.rm = TRUE)), NA),
    owner_internet_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~attribinternet(.x, hhid), na.rm = TRUE)), NA),
    owner_job_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~attribjob(.x, hhid), na.rm = TRUE)), NA),
    owner_business_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~attribbusiness(.x, hhid), na.rm = TRUE)), NA),
    owner_farming_pr_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~attribfarming(.x, hhid, 32, TRUE), na.rm = TRUE)), NA),
    owner_farming_sec_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~attribfarming(.x, hhid, 33, FALSE), na.rm = TRUE)), NA),
    owner_num_species_sheep = if_else(possede_mouton, sum(map_dbl(owner_ids, ~ sum(c_across(starts_with("nb_esp_prop"))[.x]))), NA),
    owner_decision_sale_sheep = if_else(possede_mouton, any(map_lgl(owner_ids, ~ any(c_across(starts_with("decide_vente"))[.x]))), NA),
    owner_total_sale_value_sheep = if_else(possede_mouton, sum(map_dbl(owner_ids, ~ sum(c_across(starts_with("val_achat_esp"))[.x], c_across(starts_with("rev_brut_vente_esp"))[.x]))), NA),
    owner_milk_production_sheep = if_else(possede_mouton, sum(map_dbl(owner_ids, ~ sum(c_across(starts_with("prod_lait"))[.x]))), NA),
    owner_milk_sale_revenue_sheep = if_else(possede_mouton, sum(map_dbl(owner_ids, ~ sum(c_across(starts_with("rev_vente_prlait"))[.x]))), NA),
    owner_healthcare_expenditure_sheep = if_else(possede_mouton, sum(map_dbl(owner_ids, ~ sum(c_across(starts_with("prix_soins_troup"))[.x]))), NA)
    
  ) %>%
  ungroup() %>%  # Pour éviter l'effet de groupe après rowwise
  select(-owner_ids)  # Enlève les colonnes des identifiants après utilisation

test9 = test_proprios3 %>% select(hhid:menage, -code_anim, starts_with("owner_"), vaccine) %>% mutate(vaccine = as.logical(vaccine == 1), 
                                                                                                      owner_milk_production_goat = as.logical(owner_milk_production_goat == 1),
                                                                                                      owner_milk_production_sheep = as.logical(owner_milk_production_sheep == 1)) %>% distinct(hhid, .keep_all = TRUE)

# Croiser avec le data qui contient les infos sur le chef de ménage
data_with_head_info <- test9 %>%
  left_join(chef_menage6, by = "hhid") %>% select(-grappe.y, -menage.y, -id_code) %>% select(hhid:menage.x, starts_with("head_"), starts_with("owner_"), vaccine) %>% rename(menage = menage.x, grappe = grappe.x)

# Croiser avec data menages
data_with_menage_info <- data_with_head_info %>%
  left_join(data_menage_infos, by = "hhid") %>% select(-grappe.y, -menage.y) %>% rename(grappe = grappe.x, menage = menage.x)

# croiser avec data sur les communautés 
final_data <- data_with_menage_info %>%
  left_join(data_menages2, by = c("grappe", "hhid")) %>% rename(menage = menage.x) %>% select(hhid, grappe, menage, starts_with("region"), starts_with("departement"), everything()) %>% select(-menage.y)
  #select(-menage.y.y, -menage.y, -id_code) %>% 



final_data = as.data.frame(final_data)
write.csv(final_data, "extracted data/data_niger.csv", row.names = FALSE, fileEncoding = "UTF-8")


