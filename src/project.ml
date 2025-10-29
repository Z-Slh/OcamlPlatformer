open Engine



(*Etat du jeu, gestion du numero de map*)

type game_state = { 
mutable current_map : int;
mutable exit : bool; }

let create_game_state () =
  { current_map = 0; exit = false }

let get_current_map state =
  state.current_map

  let get_current_exit state =
    state.exit  

let set_current_map state new_map =
  state.current_map <- new_map
let set_current_exit state new_exit =
  state.exit <- new_exit


let game_state = create_game_state ()

(* Partie Graphique : dessin des personnages, ennemies, projectiles *)
let personnage = " o \n/O\\\n ^ " [@@ocamlformat "disable"]
let perso_canon = " @ \n(O)\n ^ " (* forme du joueur lorsque qu'il a le canon *)
let perso_up = "\\o/\n O \n ^ " (* forme du joueur lorsqu'il saute *)
let perso_jet = " o/\n§O \n ^ " (* forme du joueur lorsqu'il s'envole *)
let ennemy1 = "<__>"
let ennemy2 = "<^v>"
let ennemy3 = "||\n||\nvv"
let pouvoir1 =  
" -------
| - | + |
 ------- "
let pouvoir2 = "
︻┳═一
"
let pouvoir3 ="
    -
  /___\\
  |   |
  | J |
  | E |
  | T |
  |   |
  | P |
  | A |
  | C |
  | K |
  |___|
 /_____\\
  |   |
"
let proj_normal = "*"
let proj_canon = "¤"
let boule ="(o)"
let porte_entre = "+"
let porte_sortie = "-"

let full = 10

(* Calcule la taille en nombre de caractères horizontaux et verticaux du personnage *)
let h_width, h_height = get_bounds personnage



(* Définition des différents types utilisés *)

type joueur = {
  position_joueur: vec;
  vitesse_joueur: vec;
  mutable dessin_joueur: string;
  alive_joueur: bool;
  vie: int;
  mutable profil : bool;
  pouvoir2 : bool; (*pouvoir canon*)
  pouvoir3 : bool; (*pouvoir jet*)
  mutable monte: bool;
  mutable monteacc : int;
  mutable delaiJet : float;
  mutable compteurJet : float;
}


type projectile = {
  position_proj: vec;
  vitesse_proj: vec;
  alive: bool;
  mutable dessin: string;
  mutable mur : bool;
}

type ennemy = {
  contact3: bool;
  contact31 : int;
  around: int;
  distance: bool;
  position_ennemy: vec;
  vitesse_ennemy: vec;
  alive_ennemy: bool;
  dessin_ennemy: string;
}

(*Boite à fonction*)
let u_p joueur b = joueur.profil <- b
(* méthode qui change la forme du personnage *)
let u_d joueur nouveau_dessin =  
joueur.dessin_joueur <- nouveau_dessin
let u_m joueur b = joueur.mur <- b
let chang_draw_up x = if x = false then perso_up else personnage
let joueurBool joueur = joueur.monte <- not joueur.monte
let joueurAcc joueur = joueur.monteacc <- joueur.monteacc +1
let init_saut joueur = joueur.monteacc <- 0  
let contact_avec_sol joueur obstacle = let (a, b, c) = update_physics (joueur.position_joueur, joueur.dessin_joueur) (0.0,0.0) [gravity] obstacle in match c with
| (Some _, Some ((_, b), s)) -> b <= snd joueur.position_joueur
| (Some _, None) ->false
| (None, Some ((_, b), s)) -> b <= snd joueur.position_joueur
| _ -> false

(* vrai si sa limite avec le jetpack est dépasée *)
let can_press_j joueur =
  let current_time = Unix.gettimeofday() in
  if joueur.compteurJet = 0. then false 
  else if current_time -. joueur.compteurJet >= 3. then true
  else false

(* vrai si l'attente pour utiliser le jetpack n'est pas dépasée *)
let cannot_press_j joueur =
  let current_time = Unix.gettimeofday() in
  if joueur.delaiJet = 0. then false 
  else if current_time -. joueur.delaiJet <= 5. then true 
  else false 

let start_compteurJet personnage = personnage.compteurJet <- Unix.gettimeofday()
let start_delaiJet personnage = personnage.delaiJet <- Unix.gettimeofday()




(* Fonction pour créer les cartes initiales *)

  
let map0 =" 
#################   #######################################       #########################
#               #   #                                      #      #                        ############################################################################################################################################################################################################\n                                                                   
#               #####                                       ######                         ################################################                                                             #     #    #                #########################################                         #\n
#                                         ##                                               ################################################                              ####                                 #    #                   #          #          #         #                              #\n
#         @                    1        ######                                             ################################################                   ####                                                 #                         #          #         #         #                         #\n
###############################################################################            ################################################                                         ####                1                           ##########################################                     -  #\n
####################################################################                       ################################################           #################################################################################################################################################\n
                                                                   #    1   ###############                                             ######            #############################################################################################################################################\n
                                                                   #                      #                                              #########            #########################################################################################################################################\n
                                                                   # ###############      #                1                                                  #########################################################################################################################################\n    
                                                                   #                      #           #################################################################################################################################################################################################\n    
                                                                   #   1     ##############         ###################################################################################################################################################################################################\n
                                                                   #                              ######                ###############################################################################################################################################################################\n
                                                                   #                          1                p         ##############################################################################################################################################################################\n
                                                                   ####################################################################################################################################################################################################################################"




let map1 ="
##########                                                         ##########\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        ###########################################################        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        # +   @                                               -   #        #\n
#        ###########################################################        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
##########                                                         ##########"

  let map2="
########################################\n
#                                      #\n
# -                                    #\n
###############   ####        2        #\n
#           1            ##########    #\n
#                     2                #\n
#           1    ##########            #\n
#        2                             #\n
#    ##########              1         #\n
#               2                      #\n 
#           ##########       1         #\n
#                           2          #\n
#           1          ##########      #\n
#                    2                 #\n
#           1    ##########            #\n
#        2                             #\n
#    ##########              1         #\n
#                2                     #\n
#           ##########       1         #\n
#                            2         #\n
#           1           ##########     #\n
#                   2                  #\n
#           1    ##########            #\n
#      2                               #\n
#   ##########               1         #\n
#               2                      #\n
#           ##########       1         #\n
#                           2          #\n
#          1             ##########    #\n
#                  2                   #\n
#          1   ##########              #\n
#                                      #\n
# +  @ ##########                      #\n
########################################"

  let map3 ="
##########                                                         ##########\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        ###########################################################        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        # +  @                                                -   #        #\n
#        ###########################################################        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
#        #                                                         #        #\n
##########                                                         ##########"
  
  let map4=
  "################################################################################################################################\n 
  #                                             ##############                                                                    #\n 
  #                                             M            #                                                                    #\n 
  #                                             M            #                     ##           3   3   3   3   3   3   3   3     #\n 
  #                                             M      -     #                     ####                                           #\n 
  #        2                         #########################               ############                                         #\n 
  #      ######          ######                                              ############                                         #\n 
  #                                  ######                                        ####                                           #\n 
  ###            ##########################                  ##############        ##                                             #\n 
  #                #                                         #            #                                                       #\n 
  #       2        #                  ######################## 3   3   3  #                                                       #\n 
  #     ######     #               #                                      #                                                       #\n 
  #                #  1      #######                                      #                                                       #\n 
  ###            ###               ###########################            #                                                       #\n 
  #                #######                                  #             #                                                       #\n 
  #      c         #                                        #     p       #                                p                      #\n 
  ###############################################           #    ######   #                    ####################################\n 
  #                                       #           #######             #                    #########################          #\n 
  #                                       #                 ###############                                                       #\n 
  #                                       ########                                             #########################       ####\n 
  #                                       #          1                                         #                       #          #\n 
  #                                       #############################                        #                       ####   1   #\n 
  #                                                                   ####                     #                       #          #\n 
  #                                                        333333333  #          1             #                       #   1   ####\n 
  #                                                                   ##########################                       #          #\n 
  #                                                                                   #                                ####       #\n 
  #                                                                   333333333       #                                #          #\n 
  #                                                                                   #                                #   1   ####\n 
  #                                                                                   #                                #          #\n 
  #                                                         ####################      #                                ####   1   #\n 
  #                                                     #####                  #                                       #          #\n 
  #                                                 #####                      #      #                                #       ####\n 
  #                                             #####                          #      #                                #          #\n 
  #                                         #####                              #      #                                ####   1   #\n 
  #                                     #####                                  #      #                                #          #\n 
  #                                 #####                                      #      #                                #  1    ####\n 
  #                             #####                                          #      #                                #          #\n 
  #                         #####                                              #      #                                ####       #\n 
  #     j               #####                                                  #      #                                #          #\n 
  #                 ######                                                     #      #                                        ####\n 
  # +        @                                                 p                      #                                           #\n 
  #################################################################################################################################"
  let map5=
  "                                                                     \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n
               W       W  III  NN   N  N   N  EEEE  RRRR                \n
               W   @   W   I   N N  N  NN  N  E     R   R               \n
               W   W   W   I   N  N N  N N N  EEEE  RRRR                \n
                W W W W    I   N   NN  N  NN  E     R  R                \n
                 W   W    III  N    N  N   N  EEEE  R   RR              \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n"
  
  
  let map6=
 "                                                                      \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n
       ###   #####   #   #  #####        ###  #   # ##### ####          \n
      #      #   #   ## ##  #           #   # #   # #     #   #         \n
      #  ##  #####   # # #  #####       #   # #   # ##### ####          \n
      #   #  #   #   #   #  #           # @ #  # #  #     #   #         \n
       ###   #   #   #   #  #####        ###    #   ##### #    #        \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n
                                                                        \n"


(*Gestion de l'affichage des maps*)
let rec build_block c w h =
  if h <= 0 then ""
  else
    let rest = build_block c w (h - 1) in
    if rest = "" then (String.make w c)
    else (String.make w c) ^ "\n" ^ rest
    
    
let rec magnify_aux nx ny s posLigne posColonne tableau tableau_positions_ennemy1 tableau_positions_ennemy2 tableau_positions_ennemy3 joueur_pos=
  match s with
  | [] -> (tableau, tableau_positions_ennemy1, tableau_positions_ennemy2, tableau_positions_ennemy3, joueur_pos)
  | '\n' :: tl ->
    magnify_aux nx ny tl (posLigne + 1) 0 tableau tableau_positions_ennemy1 tableau_positions_ennemy2 tableau_positions_ennemy3 joueur_pos
  
  | ' ' :: tl ->
    magnify_aux nx ny tl posLigne (posColonne + 1) tableau tableau_positions_ennemy1 tableau_positions_ennemy2 tableau_positions_ennemy3 joueur_pos
  
  | '@' :: tl ->
    let joueur_pos_new = (float_of_int posColonne *. float_of_int nx, float_of_int posLigne) in
    magnify_aux nx ny tl posLigne (posColonne + 1) tableau tableau_positions_ennemy1 tableau_positions_ennemy2 tableau_positions_ennemy3 joueur_pos_new
  
  | '1' :: tl -> 
    let new_vec_ennemy1 = (float_of_int posColonne *. float_of_int nx, float_of_int posLigne) in 
    let new_ennemy1 = {contact3 = false; contact31 = 0; around = 1; distance = false; position_ennemy = new_vec_ennemy1; vitesse_ennemy = (0.0, 0.0); alive_ennemy = true; dessin_ennemy = ennemy1} :: tableau_positions_ennemy1 in
    magnify_aux nx ny tl posLigne (posColonne + 1) tableau new_ennemy1 tableau_positions_ennemy2 tableau_positions_ennemy3 joueur_pos
  
  
  | '2' :: tl -> 
    let new_vec_ennemy2 = (float_of_int posColonne *. float_of_int nx, float_of_int posLigne ) in 
    let new_ennemy2 = {contact3 = false; contact31 = 0; around = 1; distance = false; position_ennemy = new_vec_ennemy2; vitesse_ennemy = (0.0, 0.0); alive_ennemy = true; dessin_ennemy = ennemy2} :: tableau_positions_ennemy2 in
    magnify_aux nx ny tl posLigne (posColonne + 1) tableau tableau_positions_ennemy1 new_ennemy2 tableau_positions_ennemy3 joueur_pos
  
  | '3' :: tl -> 
    let new_vec_ennemy3 = (float_of_int posColonne *. float_of_int nx, float_of_int posLigne ) in 
    let new_ennemy3 = {contact3 = false; contact31 = 0; around = 1; distance = false; position_ennemy = new_vec_ennemy3; vitesse_ennemy = (0.0, 0.0); alive_ennemy = true; dessin_ennemy = ennemy3} :: tableau_positions_ennemy3 in
    magnify_aux nx ny tl posLigne (posColonne + 1) tableau tableau_positions_ennemy1 tableau_positions_ennemy2 new_ennemy3 joueur_pos
    
  | '-' :: tl ->
    let new_vec = (float_of_int posColonne *. float_of_int nx, float_of_int posLigne) in
    let new_tableau = (new_vec, porte_sortie) :: tableau in
    magnify_aux nx ny tl posLigne (posColonne + 1) new_tableau tableau_positions_ennemy1 tableau_positions_ennemy2 tableau_positions_ennemy3 joueur_pos
  
  | '+' :: tl ->
    let new_vec = (float_of_int posColonne *. float_of_int nx, float_of_int posLigne) in
    let new_tableau = (new_vec, porte_entre) :: tableau in
    magnify_aux nx ny tl posLigne (posColonne + 1) new_tableau tableau_positions_ennemy1 tableau_positions_ennemy2 tableau_positions_ennemy3 joueur_pos  
  
  | 'M' :: tl ->
    let new_vec = (float_of_int posColonne *. float_of_int nx, float_of_int posLigne) in
    let new_block = "M\nM" in
    let new_tableau = (new_vec, new_block) :: tableau in
    magnify_aux nx ny tl posLigne (posColonne + 1) new_tableau tableau_positions_ennemy1 tableau_positions_ennemy2 tableau_positions_ennemy3 joueur_pos
  | 'p' :: tl ->
    let new_vec = (float_of_int posColonne *. float_of_int nx, float_of_int posLigne) in
    let new_block = pouvoir1 in
    let new_tableau = (new_vec, new_block) :: tableau in
    magnify_aux nx ny tl posLigne (posColonne + 1) new_tableau tableau_positions_ennemy1 tableau_positions_ennemy2 tableau_positions_ennemy3 joueur_pos  
  
  | 'c' :: tl ->
    let new_vec = (float_of_int posColonne *. float_of_int nx, float_of_int posLigne) in
    let new_block = pouvoir2 in
    let new_tableau = (new_vec, new_block) :: tableau in
    magnify_aux nx ny tl posLigne (posColonne + 1) new_tableau tableau_positions_ennemy1 tableau_positions_ennemy2 tableau_positions_ennemy3 joueur_pos    
  
  | 'j' :: tl ->
    let new_vec = (float_of_int posColonne *. float_of_int nx, float_of_int posLigne) in
    let new_block = pouvoir3 in
    let new_tableau = (new_vec, new_block) :: tableau in
    magnify_aux nx ny tl posLigne (posColonne + 1) new_tableau tableau_positions_ennemy1 tableau_positions_ennemy2 tableau_positions_ennemy3 joueur_pos 
  
  | c :: tl ->
    let new_vec = (float_of_int posColonne *. float_of_int nx, float_of_int posLigne) in
    let new_block = build_block c nx ny in
    let new_tableau = (new_vec, new_block) :: tableau in
    magnify_aux nx ny tl posLigne (posColonne + 1) new_tableau tableau_positions_ennemy1 tableau_positions_ennemy2 tableau_positions_ennemy3 joueur_pos
      
  let magnify nx ny s =
    magnify_aux nx ny (List.rev (List.init (String.length s) (String.get s))) 0 0 [] [] [] [] (0.0, 0.0)

(* Fonction "split_on_newline" qui divise une chaîne de caractères en une liste de sous-chaînes en utilisant le caractère '\n' comme séparateur. *)
let split_on_newline s =
  let rec aux acc i =
    try
      let j = String.index_from s i '\n' in
      let word = String.sub s i (j - i) in
      aux (word :: acc) (j + 1)
    with Not_found ->
      let remaining = String.sub s i (String.length s - i) in
      List.rev (remaining :: acc)
  in
  aux [] 0

(* Fonction "rev" qui inverse l'ordre des caractères dans une chaîne de caractères. *)  
let rev x =
  String.to_seq x |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq

(* Fonction "concat_str_list" qui concatène une liste de chaînes de caractères en les séparant par le caractère 'c'. *)  
let rec concat_str_list str_list c =
  match str_list with
  | [] -> ""
  | head::[]   -> head
  | head::tail -> head ^ c ^ concat_str_list tail c

(* Fonction "str_reverse" qui prend une chaîne de caractères, la divise en lignes, inverse l'ordre des caractères dans chaque ligne, puis concatène les lignes inversées en utilisant le caractère '\n' comme séparateur. *)
let rec str_reverse (s : string) = 
    let str_list = split_on_newline s in
    concat_str_list (List.map rev str_list) "\n" ;;

(*Application de magnify sur les différentes maps afin de creer les tableaux de solides et d'ennmies pour chaque map*)
let (tableauMap0, tableau_positions_ennemy1_map0, tableau_positions_ennemy2_map0, tableau_positions_ennemy3_map0, pos_jooueur_map0) = magnify 2 2 ( str_reverse map0) 
let (tableauMap1, tableau_positions_ennemy1_map1, tableau_positions_ennemy2_map1, tableau_positions_ennemy3_map1, pos_jooueur_map1) = magnify 2 2 ( str_reverse map1) 
let (tableauMap2, tableau_positions_ennemy1_map2, tableau_positions_ennemy2_map2, tableau_positions_ennemy3_map2, pos_jooueur_map2) = magnify 2 2 ( str_reverse map2) 
let (tableauMap3, tableau_positions_ennemy1_map3, tableau_positions_ennemy2_map3, tableau_positions_ennemy3_map3, pos_jooueur_map3) = magnify 2 2 ( str_reverse map3) 
let (tableauMap4, tableau_positions_ennemy1_map4, tableau_positions_ennemy2_map4, tableau_positions_ennemy3_map4, pos_jooueur_map4) = magnify 2 2 ( str_reverse map4) 
let (tableauMap5, tableau_positions_ennemy1_map5, tableau_positions_ennemy2_map5, tableau_positions_ennemy3_map5, pos_jooueur_map5) = magnify 2 2 ( str_reverse map5) 
let (tableauMap6, tableau_positions_ennemy1_map6, tableau_positions_ennemy2_map6, tableau_positions_ennemy3_map6, pos_jooueur_map6) = magnify 2 2 ( str_reverse map6) 
  
(*Initialisation des maps*)
(* État initial du système, c'est un tuple composé de la position du personnage, de sa vitesse,
   et d'une liste de projectiles *)

let init_state0 = ({position_joueur = pos_jooueur_map0; vitesse_joueur = (0.0, 0.0); dessin_joueur = personnage;alive_joueur = true; vie = full; profil = true;  pouvoir2 = false; pouvoir3 =false; monte= true ;monteacc = 0;delaiJet = 0.;compteurJet = 0.;},tableau_positions_ennemy1_map0@ tableau_positions_ennemy2_map0@tableau_positions_ennemy3_map0 , []) 
let init_state1 = ({position_joueur = pos_jooueur_map1; vitesse_joueur = (0.0, 0.0); dessin_joueur = personnage;alive_joueur = true; vie = full; profil = true;  pouvoir2 = false; pouvoir3 =false; monte= true ;monteacc = 0;delaiJet = 0.;compteurJet = 0.;}, tableau_positions_ennemy1_map1@ tableau_positions_ennemy2_map1@tableau_positions_ennemy3_map1, [])
let init_state2 = ({position_joueur = pos_jooueur_map2; vitesse_joueur = (0.0, 0.0); dessin_joueur = personnage;alive_joueur = true; vie = full; profil = true;  pouvoir2 = false; pouvoir3 =false; monte= true ;monteacc = 0;delaiJet = 0.;compteurJet = 0.;}, tableau_positions_ennemy1_map2@ tableau_positions_ennemy2_map2@tableau_positions_ennemy3_map2, [])
let init_state3 = ({position_joueur = pos_jooueur_map3; vitesse_joueur = (0.0, 0.0); dessin_joueur = personnage;alive_joueur = true; vie = full; profil = true;  pouvoir2 = false; pouvoir3 =false; monte= true ;monteacc = 0;delaiJet = 0.;compteurJet = 0.;}, tableau_positions_ennemy1_map3@ tableau_positions_ennemy2_map3@tableau_positions_ennemy3_map3, [])
let init_state4 = ({position_joueur = pos_jooueur_map4; vitesse_joueur = (0.0, 0.0); dessin_joueur = personnage;alive_joueur = true; vie = full; profil = true;  pouvoir2 = false; pouvoir3 =false; monte= true ;monteacc = 0;delaiJet = 0.;compteurJet = 0.;}, tableau_positions_ennemy1_map4@ tableau_positions_ennemy2_map4@tableau_positions_ennemy3_map4, [])
let init_state5 = ({position_joueur = pos_jooueur_map5; vitesse_joueur = (0.0, 0.0); dessin_joueur = "";alive_joueur = false; vie = full; profil = true;  pouvoir2 = false; pouvoir3 =false; monte= true ;monteacc = 0;delaiJet = 0.;compteurJet = 0.;}, tableau_positions_ennemy1_map5@ tableau_positions_ennemy2_map5@tableau_positions_ennemy3_map5, [])
let init_state6 = ({position_joueur = pos_jooueur_map6; vitesse_joueur = (0.0, 0.0); dessin_joueur = "";alive_joueur = false; vie = full; profil = true;  pouvoir2 = false; pouvoir3 =false; monte= true ;monteacc = 0;delaiJet = 0.;compteurJet = 0.;}, tableau_positions_ennemy1_map6@ tableau_positions_ennemy2_map6@tableau_positions_ennemy3_map6, [])

let has_projectile_with_mur_true projectiles =
  let has_mur proj = proj.mur in
  List.exists has_mur projectiles

let filter_obstacles obstacles =
  List.filter (fun (_, dessin) -> dessin <> "M\nM") obstacles

(* Cette fonction est appelée de manière répétée plusieurs fois par seconde pour mettre à jour
l'état du système. Elle doit renvoyer le nouvel état, c'est-à-dire la nouvelle position
du joueur, sa vitesse, et une liste de projectiles mise à jour. *)
let update (joueur, ennemies, projectiles) key_mod =
  game_state.exit <- false;
  let obstacles = let current_map_value = game_state.current_map in match  current_map_value with
  | 0 -> if has_projectile_with_mur_true projectiles then filter_obstacles tableauMap0 else tableauMap0
  | 1 -> if has_projectile_with_mur_true projectiles then filter_obstacles tableauMap1 else tableauMap1
  | 2 -> if has_projectile_with_mur_true projectiles then filter_obstacles tableauMap2 else tableauMap2
  | 3 -> if has_projectile_with_mur_true projectiles then filter_obstacles tableauMap3 else tableauMap3
  | 4 -> if has_projectile_with_mur_true projectiles then filter_obstacles tableauMap4 else tableauMap4
  | 5 -> tableauMap5
  | 6 -> tableauMap6
  | _ -> failwith "Numero de map invalide"  (* Gérer le cas où numero_map n'est pas dans la plage attendue *)
in


let (horizontal_vitesse_joueur, vertical_vitesse_joueur, updated_projectiles) =
if joueur.alive_joueur then (match key_mod with
  | Some (key, shift, control) ->
  (match key with
    | Char('j') -> if (joueur.pouvoir3 = false || joueur.dessin_joueur = boule || joueur.dessin_joueur = perso_canon || ((joueur.dessin_joueur = personnage || joueur.dessin_joueur = perso_up) && cannot_press_j joueur)) then (fst joueur.vitesse_joueur, snd joueur.vitesse_joueur, projectiles) else let() = if (joueur.dessin_joueur = perso_jet) then (u_d joueur personnage) else (u_d joueur perso_jet) in let () = if (joueur.dessin_joueur = perso_jet) then start_compteurJet joueur else start_delaiJet joueur in (fst joueur.vitesse_joueur, snd joueur.vitesse_joueur, projectiles)
    | Char('c') -> if (joueur.pouvoir2 = false || joueur.dessin_joueur = perso_jet || joueur.dessin_joueur = boule) then (fst joueur.vitesse_joueur, snd joueur.vitesse_joueur, projectiles) else let() = if (joueur.dessin_joueur = perso_canon) then (u_d joueur personnage) else (u_d joueur perso_canon) in (fst joueur.vitesse_joueur, snd joueur.vitesse_joueur, projectiles) 
    | Char('b') -> if (joueur.dessin_joueur = perso_jet || joueur.dessin_joueur = perso_canon) then (fst joueur.vitesse_joueur, snd joueur.vitesse_joueur, projectiles) else let() = if (joueur.dessin_joueur = boule) then (u_d joueur personnage) else (u_d joueur boule) in (fst joueur.vitesse_joueur, snd joueur.vitesse_joueur, projectiles)
    | Char('d') -> let() = (u_p joueur true) in  if joueur.monteacc >0 && joueur.monteacc <9 then let () = joueurAcc joueur in (30.0, 20.0, projectiles) else (30.0, -20.0, projectiles)
    | Char('q') -> let() = (u_p joueur false) in
    if shift || (control && fst joueur.position_joueur > 1.0) then
      (-50.0, 0.0, projectiles)
    else if fst joueur.position_joueur > 1.0 then
      if joueur.monteacc >0 && joueur.monteacc <9 then let () = joueurAcc joueur in (-30.0, 20.0, projectiles) else (-30.0, -20.0, projectiles)
    else (0.0, 0.0, projectiles)
    | Char('z') ->
      if joueur.dessin_joueur = perso_jet then (0.0, 30.0, projectiles) else 
      let () = joueurAcc joueur in
      let() = if contact_avec_sol joueur obstacles then init_saut joueur in
      if joueur.monteacc > 9 then (0.0, -.30.0, projectiles) else (0.0,30.0, projectiles)  
      
    | Char('s') -> if shift || control then (0.0, -50.0, projectiles) else (0.0, -30.0, projectiles)
    | Char('p') -> if joueur.dessin_joueur = boule then (fst joueur.vitesse_joueur, snd  joueur.vitesse_joueur , projectiles) else 
      let (a,b) = if shift then ((1., 3.),(0., 20.)) else if (joueur.profil = false) then ((2., 1.),(-50., 0.)) else ((1., 1.), (50., 0.))in 
      let final_dessin_proj = if joueur.dessin_joueur = perso_canon then proj_canon else proj_normal in  
      let new_projectile = {
      position_proj = (fst joueur.position_joueur+. fst a, snd joueur.position_joueur+. snd a) ;
      vitesse_proj = (fst b, snd b);
      alive = true;
      dessin = final_dessin_proj;
      mur= false;
      } in
      
      (fst joueur.vitesse_joueur, snd joueur.vitesse_joueur, new_projectile :: projectiles)
    | _ -> (0.0, 0.0, projectiles))
  | None -> if joueur.monteacc >0 && joueur.monteacc <9 then let () = joueurAcc joueur in (0.0, 30.0, projectiles) else (0.0, -.30.0, projectiles)
  ) else (
  (0.0, 0.0, projectiles)
  )
  in
  let () = u_d joueur (if (joueur.dessin_joueur = boule || joueur.dessin_joueur = perso_canon || joueur.dessin_joueur = perso_jet) then joueur.dessin_joueur else (chang_draw_up (contact_avec_sol joueur obstacles))) 
  in 
  let () = u_d joueur (if joueur.dessin_joueur = perso_jet && can_press_j joueur then let () = start_delaiJet joueur in personnage else joueur.dessin_joueur) 
  
in
(* Mise à jour de la position des projectiles et des collisions avec les obstacles *)
let updated_projectiles_nouv =
  List.map (fun projectile ->
    if projectile.alive then
      let (new_position_proj, velocity_proj, contacts_proj) = update_physics (projectile.position_proj, projectile.dessin) projectile.vitesse_proj [] (obstacles) in
      match contacts_proj with
      | (Some ((_, _), e) , _) when e = "M\nM"  -> if projectile.dessin = proj_canon then {projectile with mur = true; alive = false} else {projectile with alive = false  }
      | (Some y, _) -> { projectile with alive = false }
      | (_, Some z) -> { projectile with alive = false }
      | (None, None) -> { projectile with position_proj = new_position_proj; vitesse_proj = velocity_proj }
    else
      projectile
      ) updated_projectiles
    in
    
    (* Mise à jour de la position de tous les ennemis *)
let updated_enemies =
    List.map (fun ennemy ->
      
      if ennemy.alive_ennemy && ennemy.dessin_ennemy = ennemy1 then
        let (horizontal_vitesse) =
        match ennemy.distance with
        | false -> 10.0
        | true -> -10.0
        in
        
        let (new_position, _, contacts) =
        let alive_projectiles = List.filter (fun projectile -> projectile.alive) updated_projectiles in
        let projectile_positions = List.map (fun proj -> (proj.position_proj, proj.dessin)) alive_projectiles in
        update_physics (ennemy.position_ennemy, ennemy.dessin_ennemy) (horizontal_vitesse, 0.0) [] (obstacles @ projectile_positions)
        in
      
        let updated_enemy =
          match contacts with
          | (Some ((_,_), "*"), _) -> { ennemy with alive_ennemy = false }
          | (_, Some ((_,_), "*")) -> { ennemy with alive_ennemy = false }
          | (Some ((_,_), "¤"), _) -> { ennemy with alive_ennemy = false }
          | (_, Some ((_,_), "¤")) -> { ennemy with alive_ennemy = false }
          | (Some _, _) ->{ ennemy with distance = not ennemy.distance } 
          | _ -> { ennemy with position_ennemy = new_position; }
        in
        updated_enemy
    else if ennemy.alive_ennemy && ennemy.dessin_ennemy =ennemy2 then
        let (horizontal_vitesse, vertical_vitesse) =
        match ennemy.around with
        | 1 -> (10.0, -10.0)   (* Déplacement vers la droite *)
        | 2 -> (-10.0, -10.0)  (* Déplacement vers le bas *)
        | 3 -> (-10.0, 10.0)  (* Déplacement vers la gauche *)
        | 4 -> (10.0, 10.0)   (* Déplacement vers le haut *)
        | _ -> (0.0, 0.0)
      in
    
      let (new_position, _, contacts) =
      let alive_projectiles = List.filter (fun projectile -> projectile.alive) updated_projectiles in
      let projectile_positions = List.map (fun proj -> (proj.position_proj, proj.dessin)) alive_projectiles in
      update_physics (ennemy.position_ennemy, ennemy.dessin_ennemy) (horizontal_vitesse, vertical_vitesse) [] (obstacles@ projectile_positions)
      in
  
      let updated_enemy =
        match contacts with
        | (Some ((_,_), "*"), _) -> { ennemy with alive_ennemy = false }
        | (_, Some ((_,_), "*")) -> { ennemy with alive_ennemy = false }
        | (Some ((_,_), "¤"), _) -> { ennemy with alive_ennemy = false }
        | (_, Some ((_,_), "¤")) -> { ennemy with alive_ennemy = false }
        | (None, None) ->
          if ennemy.around = 2 then
            { ennemy with position_ennemy = (fst ennemy.position_ennemy-.1., snd ennemy.position_ennemy); around = (ennemy.around + 1) mod 5 }
          else
            { ennemy with position_ennemy = new_position; around = (ennemy.around + 1) mod 5 }
            
            | _ -> { ennemy with position_ennemy = new_position }
          in
          updated_enemy
    else if ennemy.alive_ennemy && ennemy.dessin_ennemy = ennemy3 then
      (* Logique spécifique pour l'ennemi de type 3 *)
      let (horizontal_vitesse, vertical_vitesse) =
      match ennemy.contact3, ennemy.contact31 with
      | true, 0 -> (10.0, -50.0)  (* Descendre vers le joueur lorsque contacté *)
      | true, 1 -> (-10.0, 50.0) 
      | false, _ ->(0.0, 0.0)
      |_,_ ->(0.0, 0.0)
    in
    
    let (new_position, _, contacts) =
    let alive_projectiles = List.filter (fun projectile -> projectile.alive) updated_projectiles in
    let projectile_positions = List.map (fun proj -> (proj.position_proj, proj.dessin)) alive_projectiles in
    update_physics (ennemy.position_ennemy, ennemy.dessin_ennemy) (horizontal_vitesse, vertical_vitesse) [] (obstacles @ projectile_positions)
  in
  
  let updated_enemy =
    match contacts with
    | (Some ((_, _), "*"), _) -> { ennemy with alive_ennemy = false }
    | (_, Some ((_, _), "*")) -> { ennemy with alive_ennemy = false }
    | (Some ((_, _), "¤"), _) -> { ennemy with alive_ennemy = false }
    | (_, Some ((_, _), "¤")) -> { ennemy with alive_ennemy = false }
    | ( _, Some _) ->{ ennemy with contact31 = (ennemy.contact31 + 1) mod 2 } 
    | _ ->
      let new_triggered =
        if not ennemy.contact3 && fst joueur.position_joueur >= fst ennemy.position_ennemy then
          true  (* Activer le déclencheur lorsque le joueur passe en dessous *)
        else
          ennemy.contact3
        in
        { ennemy with position_ennemy = new_position; contact3 = new_triggered }
      in
      updated_enemy
  else
    ennemy
    ) ennemies  
    in
    (* Mise à jour de la position du joueur et des collisions avec les obstacles *)
    let new_joueur_position, new_joueur_velocity, contactsJoueur =
    let alive_enemies = List.filter (fun ennemy -> ennemy.alive_ennemy) updated_enemies in
    let ennemies_positions = List.map (fun ennemy -> (ennemy.position_ennemy, ennemy.dessin_ennemy)) alive_enemies in
    update_physics (joueur.position_joueur, joueur.dessin_joueur) (horizontal_vitesse_joueur, vertical_vitesse_joueur) [(0.0, -200.)] (obstacles@ennemies_positions)
    
    
    
  in
  
  
  let updated_joueur = match contactsJoueur with
  (*Contact avec les ennemies*)
  | (Some ((_, _), e) , _) when e = ennemy1 || e = ennemy2 || e = ennemy3 -> if joueur.vie <= 1 then (
    game_state.current_map <- 6;
    game_state.exit <- true;
  { joueur with alive_joueur = false }
  ) else { joueur with vie = joueur.vie-1; position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }
  | (_, Some ((_, _), e)) when e = ennemy1 || e = ennemy2 || e = ennemy3  ->  if joueur.vie <= 1 then (
    game_state.current_map <- 6;
    game_state.exit <- true;
  { joueur with alive_joueur = false } ) else 
    { joueur with vie = joueur.vie-1; position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }
  (*Contact avec les superpouvoirs*)
  | (Some ((_, _), e) , _) when e = pouvoir1 -> { joueur with vie = full; position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }
  | (Some ((_, _), e) , _) when e = pouvoir2 -> { joueur with pouvoir2 = true; position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }
  | (Some ((_, _), e) , _) when e = pouvoir3 -> { joueur with pouvoir3 = true; position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }
  
  | (_ ,Some ((_, _), e) ) when e = pouvoir1 -> { joueur with vie = full; position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }
  | (_ , Some ((_, _), e)) when e = pouvoir2 -> { joueur with pouvoir2 = true; position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }
  | (_ , Some ((_, _), e)) when e = pouvoir3 -> { joueur with pouvoir3 = true; position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }
  
  (*contact avec porte entre*)
  | (Some ((_, _), e), _) when e = porte_entre ->
    (* Décrémentation du numéro de la map *)
    game_state.current_map <- game_state.current_map - 1;
    game_state.exit <- true;
    { joueur with position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }
    
  | (_, Some ((_, _), e)) when e = porte_entre ->
    (* Décrémentation du numéro de la map *)
    game_state.current_map <- game_state.current_map - 1;
    game_state.exit <- true;
    { joueur with position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }
    
    (*Contact avec porte de sortie*)
  | (Some ((_, _), e), _) when e = porte_sortie ->
    (* Incrémentation du numéro de la map *)
    game_state.current_map <- game_state.current_map + 1;
    game_state.exit <- true;
    
    { joueur with position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }
    
  | (_, Some ((_, _), e)) when e = porte_sortie ->
    (* Incrémentation du numéro de la map *)
    game_state.current_map <- game_state.current_map + 1;
    game_state.exit <- true;
    { joueur with position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }
    
    
    (*Pas de contacts*)
  | _ ->
    { joueur with position_joueur = new_joueur_position; vitesse_joueur = new_joueur_velocity }in
    
      
  if game_state.exit=false then
    (updated_joueur, updated_enemies, updated_projectiles_nouv)
  else match game_state.current_map with 
  | 0 -> init_state0
  | 1 -> init_state1
  | 2 -> init_state2
  | 3 -> init_state3
  | 4 -> init_state4
  | 5 -> init_state5
  | 6 -> init_state6
  |_->init_state6

let translate (translation: vec) (solides: solid list): solid list =
  List.map (fun (position, dessin) -> ((fst position +. fst translation, snd position +. snd translation), dessin)) solides



(* Affiche l'état du système *)
let affiche (joueur, ennemies, projectiles) =
  let translated_enemies =
    List.map (fun ennemy ->
      if ennemy.alive_ennemy then
        translate ((if fst joueur.position_joueur > 0.0 && fst joueur.position_joueur < width /. 2.0 then 0.0 else width /. 2.0 -. fst joueur.position_joueur),
                    if snd joueur.position_joueur > 0.0 && snd joueur.position_joueur < height /. 2.0 then 0.0
                    else if snd joueur.position_joueur <= 0.0 && snd joueur.position_joueur > -.height /. 2.0 then height -. 1.0
                    else height /. 2.0 -. snd joueur.position_joueur) [(ennemy.position_ennemy, ennemy.dessin_ennemy)]
      else
        []
    ) ennemies
  in
  

  let translated_solides =
  let current_map_value = game_state.current_map in
  match current_map_value with
  | 0 ->  let tableau_a_afficher = if has_projectile_with_mur_true projectiles then filter_obstacles tableauMap0 else  tableauMap0 in translate ((if fst joueur.position_joueur > 0.0 && fst joueur.position_joueur < width /. 2.0 then 0.0 else width /. 2.0 -. fst joueur.position_joueur),
  if snd joueur.position_joueur > 0.0 && snd joueur.position_joueur < height /. 2.0 then 0.0
  else if snd joueur.position_joueur <= 0.0 && snd joueur.position_joueur > -.height /. 2.0 then height -. 1.0
  else height /. 2.0 -. snd joueur.position_joueur) tableau_a_afficher@[(100.0, 35.), ("Vie : "^string_of_int joueur.vie^"/"^string_of_int full)]

  | 1 -> let tableau_a_afficher = if has_projectile_with_mur_true projectiles then filter_obstacles tableauMap1 else  tableauMap1 in  translate ((if fst joueur.position_joueur > 0.0 && fst joueur.position_joueur < width /. 2.0 then 0.0 else width /. 2.0 -. fst joueur.position_joueur),
  if snd joueur.position_joueur > 0.0 && snd joueur.position_joueur < height /. 2.0 then 0.0
  else if snd joueur.position_joueur <= 0.0 && snd joueur.position_joueur > -.height /. 2.0 then height -. 1.0
  else height /. 2.0 -. snd joueur.position_joueur) tableau_a_afficher@[(100.0, 35.), ("Vie : "^string_of_int joueur.vie^"/"^string_of_int full)]

  | 2 -> let tableau_a_afficher = if has_projectile_with_mur_true projectiles then filter_obstacles tableauMap2 else  tableauMap2 in  translate ((if fst joueur.position_joueur > 0.0 && fst joueur.position_joueur < width /. 2.0 then 0.0 else width /. 2.0 -. fst joueur.position_joueur),
  if snd joueur.position_joueur > 0.0 && snd joueur.position_joueur < height /. 2.0 then 0.0
  else if snd joueur.position_joueur <= 0.0 && snd joueur.position_joueur > -.height /. 2.0 then height -. 1.0
  else height /. 2.0 -. snd joueur.position_joueur)  tableau_a_afficher@[(100.0, 35.), ("Vie : "^string_of_int joueur.vie^"/"^string_of_int full)]

  | 3 -> let tableau_a_afficher = if has_projectile_with_mur_true projectiles then filter_obstacles tableauMap3 else  tableauMap3 in   translate ((if fst joueur.position_joueur > 0.0 && fst joueur.position_joueur < width /. 2.0 then 0.0 else width /. 2.0 -. fst joueur.position_joueur),
  if snd joueur.position_joueur > 0.0 && snd joueur.position_joueur < height /. 2.0 then 0.0
  else if snd joueur.position_joueur <= 0.0 && snd joueur.position_joueur > -.height /. 2.0 then height -. 1.0
  else height /. 2.0 -. snd joueur.position_joueur) tableau_a_afficher@[(100.0, 35.), ("Vie : "^string_of_int joueur.vie^"/"^string_of_int full)]

  | 4 -> let tableau_a_afficher = if has_projectile_with_mur_true projectiles then filter_obstacles tableauMap4 else  tableauMap4 in translate ((if fst joueur.position_joueur > 0.0 && fst joueur.position_joueur < width /. 2.0 then 0.0 else width /. 2.0 -. fst joueur.position_joueur),
  if snd joueur.position_joueur > 0.0 && snd joueur.position_joueur < height /. 2.0 then 0.0
  else if snd joueur.position_joueur <= 0.0 && snd joueur.position_joueur > -.height /. 2.0 then height -. 1.0
  else height /. 2.0 -. snd joueur.position_joueur) tableau_a_afficher@[(100.0, 35.), ("Vie : "^string_of_int joueur.vie^"/"^string_of_int full)]

  | 5 -> tableauMap5
  | 6 -> tableauMap6
  | _ -> failwith "Numero de map invalide"  (* Gérer le cas où numero_map n'est pas dans la plage attendue *)
in

  let translated_joueur =
      if joueur.alive_joueur then
        translate ((if fst joueur.position_joueur > 0.0 && fst joueur.position_joueur < width /. 2.0 then 0.0 else width /. 2.0 -. fst joueur.position_joueur),
                    if snd joueur.position_joueur > 0.0 && snd joueur.position_joueur < height /. 2.0 then 0.0
                    else if snd joueur.position_joueur <= 0.0 && snd joueur.position_joueur > -.height /. 2.0 then height -. 1.0
                    else height /. 2.0 -. snd joueur.position_joueur) [
          (joueur.position_joueur, joueur.dessin_joueur)
        ]
      else 
        (* Mettez à jour numero_map lorsque joueur.alive_joueur est false *)
        []
       in

  let affichage_projectiles = List.map (fun p ->
      if p.alive then
        let proj_pos = p.position_proj in
        let proj_dessin = p.dessin in
        (proj_pos, proj_dessin)
      else
        ((0.0, 0.0), "")  (* Projectile non affiché s'il n'est pas en vie *)
    ) projectiles in

  let translated_projectiles = translate ((if fst joueur.position_joueur > 0.0 && fst joueur.position_joueur < width /. 2.0 then 0.0 else width /. 2.0 -. fst joueur.position_joueur),
                                         if snd joueur.position_joueur > 0.0 && snd joueur.position_joueur < height /. 2.0 then 0.0
                                         else if snd joueur.position_joueur <= 0.0 && snd joueur.position_joueur > -.height /. 2.0 then height -. 1.0
                                         else height /. 2.0 -. snd joueur.position_joueur) affichage_projectiles in
  
  translated_solides @ translated_joueur@translated_projectiles @ List.flatten translated_enemies





(* Appelle de la boucle principale le premier argument est l'état initial du système.
   Les autres arguments sont les fonctions update et affiche *)
  
  
  let _ =
   loop init_state0 update affiche 


