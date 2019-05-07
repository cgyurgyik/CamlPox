open Disease 

(*Mutables *)

let curr_curr = ref false

(*Helper Methods*)

(** [random_helper l n] chooses a random element from the list [l] of size [n]*)
let random_helper l n = 
  if n = 0 then raise Empty else 
    List.nth l (Random.int n)

(** [random_neighbors n acc neighbors] is a helper function to get a list
    of [n] random [neighbors]. 
    Requires: n >= 0 *)
let rec random_neighbors (n : int) (acc : country list) 
    (neighbors : country list ) : country list  =
  if n = 0 then acc
  else try (random_neighbors (n-1) 
              ((random_helper neighbors 
                  (List.length neighbors))::acc) neighbors) with
  | Empty -> []

(** [infection_rate global_score spread] determines the infection rate, 
    which is the product of the [global_score] and disease [spread]. *)
let infection_rate global_score spread = 
  let p = (float_of_int global_score) *. spread |> int_of_float in
  if p = 0 then 1 else p

(*Types*)

type t = {
  player_score : int;
  infected_countries : country list;
  uninfected_countries : country list;
  spread : float;
  cure_spread : float;
  virulency : float;
  acquired_traits : quality list;
  infected_pop : int;
  healthy_pop : int;
  dead_pop : int;
  curr_cure: cure option; 
  curr_upgrades : 
    (string * int * evolve_type * float * Disease.quality option) list
}

(*Population related methods *)

let uninfected_list dis_type = 
  List.filter (fun x -> not(is_infected x)) (country_list dis_type)

let infected_list dis_type = 
  List.filter (fun x -> is_infected x) (country_list dis_type)

let calc_uninfected dis_type = 
  List.fold_left (fun acc x-> (get_healthy_pop x) + acc) 0 
    (country_list dis_type)

let calc_infected dis_type = 
  List.fold_left (fun acc x-> (get_inf_pop x) + acc) 0 (infected_list dis_type)

let calc_dead dis_type = 
  List.fold_left (fun acc x -> (get_dead_pop x) + acc ) 0 (country_list dis_type)

let calc_total dis_type = 
  List.fold_left 
    (fun acc x -> (get_total_pop x) + acc) 0 (country_list dis_type)

let get_cure_score cur_op = 
  match cur_op with 
  | None -> 0.0
  | Some n -> get_cure_spread n

(*Type t related methods*)

let init_state score dis dis_type cure curr_upgrades purchased_val = 
  let calc_inf = calc_infected dis_type in
  let calc_killed = calc_dead dis_type in
  let calculated_score = ((calc_inf + calc_killed)/1000000) - purchased_val in  
  {
    player_score = 
      (if score > calculated_score then score else calculated_score);
    infected_countries = infected_list dis_type;
    uninfected_countries = uninfected_list dis_type;
    spread = get_spread dis;
    cure_spread = get_cure_score cure;
    virulency = get_virulency dis;
    acquired_traits = get_traits dis;
    infected_pop = calc_inf;
    healthy_pop = calc_uninfected dis_type ;
    dead_pop = calc_dead dis_type;
    curr_cure = cure;
    curr_upgrades = curr_upgrades
  }

let get_player_score t =
  t.player_score

let upgrade_player_score t upgrade_val dis dis_type upgrade_vals =
  init_state (t.player_score - upgrade_val) 
    dis dis_type t.curr_cure t.curr_upgrades upgrade_vals

let get_infected_pop t = 
  t.infected_pop

let get_healthy_pop t = 
  t.healthy_pop

let get_dead_pop t =
  t.dead_pop

let get_inf_counts t = 
  t.infected_countries

let get_health_counts t =
  t.uninfected_countries

let get_curr_upgrades t =
  t.curr_upgrades

let get_spread_rt t =
  t.spread

let get_traits t =
  t.acquired_traits

let get_cure st = 
  st.curr_cure

let get_virulency st =
  st.virulency

let assc_table dis_type =
  List.fold_left (fun acc x -> 
      (get_id x, (
          if get_total_pop x = 0 then 1.0 else
            (float_of_int (get_inf_pop x) /. float_of_int (get_total_pop x)))
      )::acc) [] (country_list dis_type)

(*Evolution/Upgrade related methods  *)

let get_evolve_info (name, v, typ, bst, qual) = (v,typ,bst,qual)

let get_evolve_name (name, v, typ, bst, qual) = name

let get_evolve_val (v, typ, bst,qual) = v

let get_evolve_type (v, typ, bst,qual) = typ

let get_evolve_boost (v, typ, bst,qual) = bst

let get_evolve_quality (v, typ,bst,qual) = qual

let legal_evolve t name =
  let rec legal_ev_helper t name lst =
    match lst with
    | [] -> false
    | x::xs -> if get_evolve_name x = name then true else
        legal_ev_helper t name xs
  in legal_ev_helper t name t.curr_upgrades

(** [remove_upgrade t name] is a helper function to remove an upgrade with name
    [name] from the current upgrades list of [t]. *)
let remove_upgrade t name = 
  let rec remove_helper t name acc lst = 
    match lst with
    | [] -> acc
    | x::xs -> if (get_evolve_name x <> name) 
      then (remove_helper t name (x::acc) xs) 
      else remove_helper t name acc xs
  in remove_helper t name [] t.curr_upgrades

let change_upgrades t name = 
  {t with curr_upgrades = List.rev(remove_upgrade t name)}

let get_upgrade_values t name = 
  let rec get_helper t name (lst : ('a * 'b * 'c * 'd *'e) list) =
    match lst with
    | [] -> failwith "Specifications not met."
    | x::xs -> if (get_evolve_name x) = name then 
        get_evolve_info x
      else get_helper t name xs
  in get_helper t name t.curr_upgrades

let commence_cure count st =  
  if not(!curr_curr) &&
     (((float_of_int (get_inf_pop count)) /. 
       (float_of_int (get_total_pop count)) > 0.25)) then 
    (curr_curr := true;
     Some(init_cure count)) else st.curr_cure

(*[gen_spreader st dis_type dis f] spreads the cure/disease to a country and
  a percentage of its neighbors from [st] in [dis_type], [f] either decreases
  or increases the infected/health populations*)
let gen_spreader st dis_type dis f upgrade_val =
  let rec spreader st n ls = 
    match ls with 
    | [] -> st
    | h::t -> f h;
      if get_inf_pop h = 0 then disinfect h;
      let new_st = init_state (st.player_score) dis dis_type 
          (commence_cure h st) 
          st.curr_upgrades upgrade_val in 
      spreader new_st n t in 
  let rec iterator st ls = 
    match ls with 
    | [] -> st
    | h::t -> let r_val = infection_rate h.global_score 
                  (get_cure_score st.curr_cure )in 

      let rand_neigh = h::(random_neighbors r_val [] 
                             (get_neighbors h dis_type)) in
      let new_st = spreader st r_val rand_neigh in 
      iterator new_st t in
  iterator st (st.infected_countries)

let spread_infection (st : t) (dis_type : Disease.t) 
    (dis : Disease.disease) upgrade_val : t = 
  gen_spreader st dis_type dis 
    (fun h ->  
       let clim = get_climate h in
       let value = 
         if (List.mem clim st.acquired_traits) then 
           begin
             match clim with 
             | Temperate n -> n 
             | Tropical n -> n 
             | Arid n -> n 
             | Chill n -> n end
         else 1.0 in 
       increase_infection h (st.spread *. value) dis st.cure_spread ) 
    upgrade_val

let calc_cure st cure = 
  let calc = 
    let cur = get_cure_score st.curr_cure in 
    let pos =  
      cur +. cur *. ((float_of_int (st.infected_pop)) /. 
                     (float_of_int (get_infected_pop st + get_healthy_pop st 
                                    + get_dead_pop st))) in 
    min pos 1.0 in
  update_cure_spread cure calc

let spread_cure (st : t) (dis_type : Disease.t) (cure : Disease.cure option)
    (dis : Disease.disease) upgrade_val : t = 
  if !curr_curr then begin
    let cured = match cure with 
      | None -> failwith "[spread_cure] There should be a cure."
      | Some n -> n in 
    let new_st =   gen_spreader st dis_type dis 
        (fun h -> decrease_infection h (get_cure_score cure )) upgrade_val in
    calc_cure st cured; new_st
  end else st

let kill st dis_type dis upgrade_val =
  let vir_score = st.virulency in 
  ignore(List.map (fun x -> increase_virulency x vir_score) 
           st.infected_countries);
  init_state (st.player_score) dis dis_type 
    (st.curr_cure) (st.curr_upgrades) upgrade_val