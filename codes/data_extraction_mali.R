# Ce fichier contient l'etraction de la base de données des ménages qui possèdent des moutons et des chèvres au mali
source("source/functions.R")
library(dplyr)
###### questionnaires ménage (concernent plusieurs individus)##########
# section 0 : identification du ménage
unzip_niger_data("00", "me")
# table des correspondances pour les régions et departements
table_region = data.frame(region = 1:8, 
                          region_nom = c("agadez", "diffa", "dosso", "maradi", "tahoua", "tillaberi", "zinder", "niamey"))

table_departement = read.csv("original data/ECVMA Niger/2018-2019/Data/departements_niger.csv", 
                             header = TRUE, sep = ";")

# left_join fusionne les donnees des tables de correspondance des regions/depart avec le dataframe de gauche en utilisant les colonnes region/departement
vars_men_id = data_00_me_ner %>% select(grappe,
                                        menage,
                                        s00q01,
                                        s00q02) %>% rename(id_menage = menage,
                                                           region = s00q01,
                                                           departement = s00q02) %>% left_join(table_region,
                                                                                               by = "region") %>% left_join(table_departement,
                                                                                                                            by = "departement")

# section 1 : caractéristiques sociodémographiques des membres du ménage
unzip_niger_data("01", "me")
# table de correspondance religion
table_religion = data.frame(religion = 1:5, 
                            religion_menage = c("musulman", "chretien", "animiste", "autre religion", "sans religion"))

vars_men_s1 = data_01_me_ner %>% select(grappe,
                                        menage, 
                                        s01q00a, 
                                        s01q01, 
                                        s01q03a:s01q03c,
                                        s01q04a,
                                        s01q07,
                                        s01q14) %>% rename(id_menage = menage, 
                                                           code_id = s01q00a, 
                                                           sexe = s01q01,
                                                           naissance_jour = s01q03a,
                                                           naissance_mois = s01q03b,
                                                           naissance_annee = s01q03c,
                                                           age_annee = s01q04a,
                                                           statut_martial = s01q07,
                                                           religion = s01q14) %>% left_join(table_religion, 
                                                                                            by = "religion")

# section 10 : entreprises non agricoles
unzip_niger_data("10_1", "me")
vars_men_s10_1 = data_10_1_me_ner %>% select(grappe, 
                                             menage, 
                                             s10q11) %>% rename(id_menage = menage, 
                                                                entreprise_nonag_pres = s10q11)
# entreprise_nonag : présence d'entreprise non agricole (réponse positive à au moins une des questions 2 à 10)
unzip_niger_data("10_2", "me")
vars_men_s10_2 = data_10_2_me_ner %>% select(grappe, 
                                             menage, 
                                             s10q22) %>% rename(id_menage = menage, 
                                                                entreprise_nonag_benef = s10q22)
# section 11 : logement
unzip_niger_data("11", "me")
vars_men_s11 = data_11_me_ner %>% select(grappe, 
                                         menage, 
                                         s11q34,
                                         s11q00,
                                         s11q43,
                                         s11q46) %>% rename(id_menage = menage, 
                                                            reseau_elect_men = s11q34,
                                                            reseau_mobile_men = s11q43,
                                                            internet_men = s11q46,
                                                            id_code_pr = s11q00 # code id du principal répondant
                                         )
# section 12 : actifs du ménage
unzip_niger_data("12", "me")
vars_men_s12 = data_12_me_ner %>% select(grappe, 
                                         menage, 
                                         s12q01, 
                                         s12q02:s12q06,
                                         s12q08,
                                         s12q09,
                                         s12q00) %>% rename(id_menage = menage,
                                                            id_article = s12q01,
                                                            possede_article = s12q02, 
                                                            nb_article = s12q03,
                                                            possede_article_men = s12q04, 
                                                            etat_article = s12q06,
                                                            val_fcfa = s12q08, # valeur d'acquisition de l'article
                                                            prix_revente = s12q09,
                                                            id_code_pr = s12q00) 

# article : 44 = immeuble, 45 = terrain non batis

# section 14 : shocks
unzip_niger_data("14", "me")
vars_men_s14 = data_14_me_ner %>% select(grappe, 
                                         menage,
                                         s14q00,
                                         s14q01, 
                                         s14q02, 
                                         s14q04d, 
                                         s14q05__1:s14q05__26) %>% rename(id_menage = menage,
                                                                          id_code = s14q00,
                                                                          choc_nature = s14q01,
                                                                          choc_men = s14q02,
                                                                          choc_men_conseq = s14q04d,
                                         ) %>% rename_with(~paste0("choc_men_strat", 
                                                                   seq_along(.)), starts_with("s14q05__")) %>% filter(choc_nature == 108)

# 2 : effectif cheptel diminué
# 108 : taux élevé de maladie des animaux

# section 16A : agriculture partie A plantations
unzip_niger_data("16a", "me")
vars_men_s16a = data_16a_me_ner %>% select(grappe, 
                                           menage,
                                           s16aq04,
                                           s16Aq00) %>% rename(id_menage = menage, 
                                                               cultive_terre_men = s16Aq00,
                                                               id_code = s16aq04)

# section 17 : elevage
unzip_niger_data("17", "me")
vars_me_s17 = data_17_me_ner %>% select(grappe, 
                                        menage, 
                                        s17q00, 
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
                                        s17q58:s17q61) %>% rename(id_menage = menage,
                                                                  menage_possede = s17q00,
                                                                  code_anim = s17q02,
                                                                  menage_a_possede = s17q03,
                                                                  taille_troup = s17q05,
                                                                  taille_troup_men = s17q06,
                                                                  prop_anim1 = s17q07a1,
                                                                  prop_anim1_autre = s17q07a4,
                                                                  prop_anim2 = s17q07b1,
                                                                  prop_anim2_autre = s17q07b4,
                                                                  prop_anim3 = s17q07c1,
                                                                  prop_anim3_autre = s17q07c4,
                                                                  prop_anim4 = s17q07d1,
                                                                  prop_anim4_autre = s17q07d4,
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
                                                                  y = s17q56, # variable réponse : avez-vous fait vacciné votre troupeau durant les 12 derniers mois?
                                                                  deparasit_troup = s17q58,
                                                                  deparasit_troup_prx = s17q59,
                                                                  soins_troup = s17q60,
                                                                  prix_soins_troup = s17q61
                                        )

# section 18 : pêche
#unzip_niger_data("18_1", "me")
#######PB1########
# souci les csv de cette section sont vides, au pire si c'est pour une variable on le prend pas
#vars_me_peche = select()


# section 20 : pauvreté
unzip_niger_data("20", "me")
vars_men_s20 = data_20_me_ner %>% select(grappe,
                                         menage,
                                         s20q02:s20q11) %>% rename(
                                           id_menage = menage,
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
unzip_niger_data("02", "me")
vars_me_s2 = data_02_me_ner %>% select(grappe, 
                                       menage, 
                                       s01q00a,
                                       s02q03,
                                       s02q29
) %>% rename(id_menage = menage,
             id_code = s01q00a,
             etude_actuel = s02q03,
             niveau_etude_max = s02q29)
# section 4 : emploi
unzip_niger_data("04", "me")
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
                                       s04q57) %>% rename(id_menage = menage,
                                                          id_code = s01q00a,
                                                          travail_champs = s04q06,
                                                          travail_remun_com = s04q07,
                                                          travail_salarie = s04q08,
                                                          travail_non_exerce = s04q11,
                                                          source_rev_prin = s04q16,
                                                          code_activite = s04q30d,
                                                          duree_emploi = s04q32,
                                                          salaire_emploi = s04q43,
                                                          travail_sec = s04q50, #emploi secondaire
                                                          cat_sociopro = s04q57
                                       )

# section 6 : épargne
unzip_niger_data("06", "me")
vars_me_s6 = data_06_me_ner %>% select(grappe,
                                       menage,
                                       s01q00a,
                                       s06q01__1,
                                       s06q02,
                                       s06q03,
                                       s06q06,
                                       s06q06_autre,
                                       s06q11,
                                       s06q18) %>% rename(id_menage = menage,
                                                          id_code = s01q00a,
                                                          compte_possession = s06q01__1, #compte en banque classique
                                                          compte_epargne = s06q02,
                                                          demande_credit = s06q03,
                                                          obtention_credit = s06q06, #raison pourlaquelle il n'a pas obtenu de crédit
                                                          obtention_credit_autre = s06q06_autre,
                                                          utilisation_credit = s06q11,
                                                          montant_dernier_credit = s06q18)


######### questionnaires commuautaires (concernent plusieurs ménages)##########

# section 1 : caracteristiques générales du quartier/ village
# chargement data section1
unzip_niger_data("01", "co")
# selection des variables section1
var_com_s1 = data_01_co_ner %>% select(grappe, 
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
                                                             transp_principal = s01q09,
                                                             reseau_elect = s01q11, #disponibilité de reseau de distribution electrique
                                                             reseau_mobile = s01q13,
                                                             presence_cs = s01q14__4 # comité de santé
                                       ) 
# section 2 : existence et accessibilité aux services sociaux
# chargement donnees section2
unzip_niger_data("02", "co")
# selection variables section2
var_com_s2 = data_02_co_ner %>% select(grappe,
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





####### sélection des ménages qui ont des moutons et des chèvres #########
data_17_me_ner$s17q02 = as.numeric(data_17_me_ner$s17q02)
#s17q02 (codes espèces, 2 : moutons, 3 : chèvres)
# s17q03 : possession d'animaux (1 si oui, 2 sinon)
table_mc = data_17_me_ner %>% filter((s17q02 == 2 |
                                        s17q02 == 3 ) & s17q03 == 1) %>% distinct(menage) # identifiant unique
# ensemble des identifiants ménages
id_menages = as.character(table_mc$menage)
nhh = nrow(table_mc)