open Yojson.Basic.Util

(**Types*)

exception UnknownCountry of string
exception Empty
exception Malformed

type quality = | Temperate of float | Tropical of float | Arid of float
               | Chill of float

type country = {
  country_name : string; 
  mutable total_population : int; 
  mutable population_healthy : int; 
  mutable population_inf: int; 
  mutable population_dead : int;
  mutable contains_disease : bool;
  climate : quality;
  global_score : int;
  neighbors : string list;
  id : char
}

type countries = country list

type disease = {
  disease_name : string;
  start_country : country;
  mutable traits : quality list;
  mutable spread : float;
  mutable virulency : float;
}

type cure = {
  start : country;
  mutable cure_spread : float
}

type evolve_type = | Spread | Virulency | Trait

type evolve = {
  evolve_name : string; 
  point_value : int;
  e_type : evolve_type;
  boost : float;
  trait : quality option
}

type t = {
  countries : country list;
  evolutions : evolve list
}

(*Helper Methods*)

(**[parse str] is a the first word of the trimmed [str]
   Raises: [Empty] if the length of [str] is 0*)
let parse str =
  let checked = String.trim str in
  if String.length checked = 0 then raise Empty else
    let split = String.split_on_char ' ' checked in 
    match split with 
    | h::t-> Some h
    | [] -> None

(** [calc_spread count spread] is a helper function to calculate the [spread]
    rate of in the country [count]. *)
let calc_spread count spread pop = 
  let pos = (spread)*.(float_of_int pop) 
            |> int_of_float in 
  if pos = 0 then 1 else pos

(** to_quality is a helper function that converts a string [s] to a quality if
    [s] represents a valid quality, fails otherwise. *)
let to_quality (s : string) : quality =
  if s = "temperate" then Temperate 1.25
  else if s = "tropical" then Tropical 1.25
  else if s = "arid" then Arid 1.25
  else if s = "chill" then Chill 1.25
  else raise Malformed

(*JSON Methods *)

(** [country_converter_json json] is a helper function to parse [json]
    into type country.
    Requires: [json] is a valid JSON country representation. *)
let country_converter_json json = {
  country_name : string = json |> member "name" |> to_string;
  total_population : int = json |> member "population" |> to_int;
  global_score : int = (json |> member "global_score" |> to_int) + 50;
  population_inf = 0;
  population_dead = 0;
  population_healthy : int = json |> member "population" |> to_int;
  contains_disease = false;
  climate : quality = json |> member "climate" |> to_string |> to_quality;
  neighbors : string list = json |> member "neighbors" |> to_list 
                            |>  filter_string;
  id = 
    let stringed = json |> member "id" |> to_string in 
    String.get stringed 0 
}

(** [evolve_converter_json json] is a helper function to parse [json]
    into type evolve.
    Requires: [json] is a valid JSON country representation. *)
let evolve_converter_json json = {
  evolve_name = json |> member "name" |> to_string;
  point_value = json |> member "point_value" |> to_int;
  e_type = begin 
    let temp = json |> member "e_type" |> to_string in
    match temp with
    | "spread" -> Spread
    | "virulency" -> Virulency
    | "trait" -> Trait 
    | _ -> failwith "e_type in JSON does not match specifications."
  end;
  boost = json |> member "boost" |> to_float;
  trait = try( Some (json |> member "trait" |> to_string |> to_quality)) with
      Malformed -> None
}

let from_json json = {
  countries =
    json |> member "countries" |> to_list |> List.map country_converter_json;
  evolutions =
    json |> member "evolutions" |> to_list |> List.map evolve_converter_json}

(*Country Methods*)

let country_list dis = 
  dis.countries

let is_infected count = 
  count.contains_disease

let rec find_country countries country_name = 
  match countries with 
  | [] -> None 
  | h::t -> if  h.country_name = country_name then Some h else find_country t
        country_name

let rec get_countrynames (countries : country list) =
  match countries with
  | [] -> []
  | h::t -> h.country_name :: (get_countrynames t)

let get_neighbors country dis_type = 
  List.fold_left (fun acc x -> 
      match find_country (country_list dis_type) x  with 
      | None -> acc 
      | Some c -> c::acc) [] country.neighbors

let get_inf_neighbors country dis_type = 
  List.fold_left (fun acc x -> 
      match find_country (country_list dis_type) x  with 
      | None -> acc 
      | Some c -> if (is_infected c) then
          c::acc else acc) [] country.neighbors

let infect count =
  count.contains_disease <-true

let disinfect count =
  count.contains_disease <-false

let get_healthy_pop count = 
  count.population_healthy

let get_total_pop count = 
  count.total_population

let get_inf_pop count =
  count.population_inf

let get_dead_pop count =
  count.population_dead

let get_global_score count =
  count.global_score

let get_climate count =
  count.climate

let get_id count = 
  count.id

(*Evolution Methods *)

let evolution_list dis =
  dis.evolutions

let rec get_evolvenames (evo : evolve list) =
  match evo with
  | [] -> []
  | h::t -> 
    (h.evolve_name, h.point_value, h.e_type, h.boost, h.trait) :: 
    (get_evolvenames t)
(*Disease Methods *)

let init_disease countries name country intensity qual = 
  let (parsed_c,parsed_i) = 
    match (Some (String.trim country), parse intensity) with 
    |(None,_) 
    | (_, None) -> raise Empty 
    | (Some a, Some b) -> (a,b) in
  let found = 
    match find_country countries parsed_c with 
    | None -> raise (UnknownCountry country)
    | Some c -> c in
  found.contains_disease <- true;
  let spread_val = let parsed = parsed_i in 
    if parsed = "benign" then 0.005 else 
    if parsed = "dangerous" then 0.01 else 
    if parsed = "deadly" then 0.05 else raise Malformed in
  let increase = (spread_val)*.(float_of_int found.total_population) 
                 |> int_of_float in
  found.population_healthy <- (found.population_healthy - increase);
  found.population_inf <- (found.population_inf + increase);
  found.contains_disease <- true;
  { 
    disease_name = name; 
    start_country = found; 
    spread = spread_val;
    traits = qual;
    virulency = 0.001;
  } 

let update_dis_spread dis value =
  dis.spread <- value

let get_spread dis = 
  dis.spread

let get_virulency dis =
  dis.virulency

let get_disease_name dis =
  dis.disease_name

let get_traits dis =
  dis.traits

let get_start_count dis = 
  dis.start_country

let update_dis_traits dis qual = 
  let q = get_traits dis in
  dis.traits <- (qual :: q)

let increase_infection count spread dis cure_spread = 
  count.contains_disease <-true;
  let increase =  calc_spread count spread (count.population_healthy) in
  let health = count.population_healthy - increase in 
  let inf = count.population_inf + increase in
  count.population_healthy <- (max 0 health);
  count.population_inf <- (if inf >= count.total_population then 
                             count.total_population else inf)

let increase_virulency count vir =
  let increase = int_of_float (vir *. float_of_int (count.population_inf)) in 
  let dead = count.population_dead + increase in
  let inf = count.population_inf - increase in
  let total_pop = count.total_population - increase in
  count.population_dead <- max 0 dead;
  count.population_inf <- max 0 inf;
  count.total_population <- max 0 total_pop

let update_virulency dis value = 
  dis.virulency <- value

(*Cure Methods *)

let init_cure count = 
  {
    start = count;
    cure_spread = 0.000000001 
  }

let decrease_infection count spread =  
  let decrease =  calc_spread count spread count.population_inf in
  let health = count.population_healthy + decrease in 
  let inf = count.population_inf - decrease in
  count.population_healthy <- (if health >= count.total_population then
                                 count.total_population else health);
  count.population_inf <- (if inf <= 0  then 0 else inf)

let update_cure_spread cure value =
  cure.cure_spread <- value 

let get_cure_start cure = 
  cure.start

let get_cure_spread cure =
  cure.cure_spread