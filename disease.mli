(** 
   This module represents a disease that may spread throughout a list of 
   countries. The disease has a name and starting country initialized
   by the player upon play. It handles loading of data from JSON as well.
*)

(** The abstract type representing the quality of a country's climate or
    a quality of the disease.  *)
type quality = | Temperate of float | Tropical of float | Arid of float
               | Chill of float

(** The abstract type of values representing countries. [total_population] 
    represents the total population of the country. An individual can either be 
    part of [population_healthy] or [population_inf], but not both. Their union
    is disjoint. [contains_disease] determines whether the country currently is
    infected, [climate] represents quality of country that determines effect of
    certain disease types and the [global_score] represents the score 
    associated with each country. [neighbors] represents the neighbors to 
    the country. *)
type country = {
  country_name : string;
  mutable total_population : int;
  mutable population_healthy : int;
  mutable population_inf : int;
  mutable population_dead : int;
  mutable contains_disease : bool;
  climate : quality; 
  global_score : int; 
  neighbors : string list;
  id : char
}

(** The abstract type of values representing diseases. The [disease_name] and
    [start_country] are initialized by the player. The [trait] represents
    qualities disease has that affects countries of certain climates. 
    The [spread] represents the spread rate of the disease. 
    The [virulency] represents the fatality rate of infected population. *)
type disease = {
  disease_name : string;
  start_country : country;
  mutable traits : quality list;
  mutable spread : float;
  mutable virulency : float
}

(** The abstract type of values represent cures. The cure has a starting country
    [start] and a mutable field representing the cure spread rate as
    [cure_spread]. *)
type cure = {
  start : country;
  mutable cure_spread : float
}

(** The abstract type representing what the evolution will upgrade in the
    game state. *)
type evolve_type = | Spread | Virulency | Trait

(** The abstract type representing items that can be obtained if the player has 
    enough points. The [boost] represents how much the evolve will upgrade by,
    and the [point_value] represents the cost of the evolve. *)
type evolve = {
  evolve_name : string; 
  point_value : int;
  e_type : evolve_type;
  boost : float;
  trait : quality option
}

(** Represents the collection of countries and evolutions*)
type t

(** Raised when the country name is unknown. *)
exception UnknownCountry of string

(** Raised when a string is of length 0 *)
exception Empty

(** Raised when a string does not match a particular keyword *)
exception Malformed

(** [init_disease countries name country intensity qual] picks a [country] from 
    [countries], and initializes it as the starting country for the disease, 
    and introduces an infected populataion.It also names the disease with 
    [name], provides it a value associated with [intensity], and
    a climate of type [qua].
    Raises: [UnknownCountry] if [country] is not in [countries], 
    and Malformed if [intensity] is not a correct keyword*)
val init_disease : country list -> string -> string -> string -> quality list 
  -> disease

(** [from_json json] is the list of countries that [json] represents.
    Requires: [json] is a valid JSON country list representation. *)
val from_json : Yojson.Basic.json -> t

(**[country_list dis] is a list of all the countries from [dis]. *)
val country_list : t -> country list 

(** [get_countrynames countries] is a string list representing names 
    of [countries] *)
val get_countrynames : country list -> string list

(** [get_climate count] is the quality of [country] *)
val get_climate : country -> quality

(** [get_neighbors country dis_type] is a list of the neighbors to [country]. *)
val get_neighbors : country -> t -> country list

(** [get_disease_name dis] is a list of the neighbors to [country]. *)
val get_disease_name : disease -> string

(** [get_inf_neighbors country dis_type] is a list of the infected neighbors 
    to [country]. *)
val get_inf_neighbors : country -> t -> country list

(**[evolution_list dis] is a list of all the evolutions in [dis]. *)
val evolution_list : t -> evolve list 

(** [get_evolvenames evo] is string list representing names of [evolutions] *)
val get_evolvenames : evolve list -> 
  (string * int * evolve_type * float * quality option) list

(** [get_traits] is a quality list representing traits the disease has. *)
val get_traits : disease -> quality list

(**[get_spread dis] is the spread factor of [dis]*)
val get_spread : disease -> float

(**[get_virulency dis] is the virulence factor of [dis]*)
val get_virulency : disease -> float

(**[get_start_count dis] is the country where [dis] started*)
val get_start_count : disease -> country

(**[is_infected count] is whether [count] is infected or not*)
val is_infected : country -> bool

(**[increase_infection count spread] infects [count] with the spread score of 
   [spread], increases its infected population and decreases its population. *)
val increase_infection : country -> float -> disease -> float -> unit

(**[increase_virulency count spread] infects [count] with the virulence score of 
   [spread], increases its infected population and decreases its infected
    population and total population.*)
val increase_virulency : country -> float -> unit

(** [update_virulency dis value] updates the virulency of [dis] with [value]. *)
val update_virulency : disease -> float -> unit

(** [update_dis_traits dis qual] updates the traits of [dis] by adding the
    quality [qual]  *)
val update_dis_traits : disease -> quality -> unit

(**[decrease_infection count spread] disinfects [count] with the virulence score 
   of [spread], decreases its infected population and increases its population*)
val decrease_infection : country -> float -> unit 

(**[get_pop count] is the population of the country [count]*)
val get_total_pop : country -> int

(**[get_inf_pop count] is the infected population of the country [count]*)
val get_inf_pop : country -> int

(**[get_healthy_pop count] is the healthy population of the country [count]*)
val get_healthy_pop : country -> int

(**[get_dead_pop count] is the dead population of the country [count]*)
val get_dead_pop : country -> int

(**[get_global_score count] is the global score of the country [count]*)
val get_global_score : country -> int

(** [get_id] is the id of the country [count] *)
val get_id : country -> char

(** [find_country countries country_name] is a helper function to find
    [country_name] in [countries]. *)
val find_country: country list -> string -> country option 

(**[infect count] infects the country [count]*)
val infect: country -> unit

(**[disinfect count] disinfects the country [count]*)
val disinfect : country -> unit

(**[init_dis count dis] initializes the formation of a cure in [count]*)
val init_cure : country -> cure

(**[get_cure_start cure] gets the country of [cure] where the formation of 
   the cure began*)
val get_cure_start : cure -> country

(**[get_cure_spread cure] gets the effectiveness of the cure in [cure] *)
val get_cure_spread : cure -> float

(**[update_cure_spread cure value] updates the effectiveness of the cure 
   in [cure] by [value] *)
val update_cure_spread : cure -> float -> unit

(**[update_dis_spread dis value] increments the spreadability of [dis]
   by [value]. *)
val update_dis_spread : disease -> float -> unit 

(** [get_id count] returns the country id *)
val get_id : country -> char