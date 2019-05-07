open Disease

type t 

(**[non_infected_list dis_type] are all the countries in [dis_type] that
   are not infected *)
val uninfected_list : Disease.t -> Disease.country list

(**[infected_list dis_type] are all the countries in [dis_type] that
   are infected*)
val infected_list : Disease.t -> Disease.country list

(**[calc_healthy dis_type] is the total population of all the countries in
   [dis_type] that are healthy*)
val calc_uninfected : Disease.t -> int

(**[calc_infected dis_type] is the total population of all the countries in
   [dis_type] that are infected*)
val calc_infected : Disease.t -> int

(**[calc_total dis_type] is the total population of all the countries in
   [dis_type]*)
val calc_total : Disease.t -> int

(**[calc_dead dis_type] is the total dead population of all the countries in
   [dis_type]*)
val calc_dead : Disease.t -> int

(**[init_state dis dis_type] initializes the state with a score of 0,
   obtains the infected and noninfected countries from [dis_type], 
   the spread from [dis], virulency from [dis], and the total infected,
   noninfected and dead populations of all the countries in [dis_type]*)
val init_state : int -> Disease.disease -> Disease.t -> Disease.cure option -> 
  (string * int * evolve_type * float * quality option) list -> int -> t

(**[get_player_score t] gets the player's current score in state [t]. *)
val get_player_score : t -> int

(**[upgrade_player_score t n] upgrades the player's current score 
   in state [t]. *)
val upgrade_player_score : t -> int -> Disease.disease -> Disease.t -> int -> t

(**[get_curr_upgrades t] gets the current upgrades allowed in state [t]. *)
val get_curr_upgrades : t -> (string * int * Disease.evolve_type * float 
                              * quality option) list

(**[legal_evolve t] gets the current upgrades allowed in state [t]. *) 
val legal_evolve : t -> string -> bool

(**[change_upgrades t name] changes the list of legal upgrades in state [t]. *)
val change_upgrades : t -> string -> t

(**[get_upgrade_value t name] gets the value of the upgrade named [name] 
   in state [t].
   Requires: [name] is an upgrade in the list. *)
val get_upgrade_values : t -> string -> (int * evolve_type * float * quality option)

(** [get_evolve_helper (name, v, typ, bst,qual)] returns the last 3 values of the
    tuple, the value [v], evolve type [typ], and boost value [bst].*)
val get_evolve_info : 
  (string * int * evolve_type * float * Disease.quality option) -> 
  (int * evolve_type * float  * Disease.quality option)

(** [get_evolve_name (name, v, typ, bst,qual)] is a helper function to return the 
    [name] of the evolve. *)
val get_evolve_name : 
  (string * int * evolve_type * float * Disease.quality option) -> string

(** [get_evolve_val (v, typ, bst,qual)] is a helper function to return the type of the 
    evolve. *)
val get_evolve_val : (int * evolve_type * float * Disease.quality option) -> int

(** [get_evolve_type (v, typ, bst, qual)] is a helper function to return the type
    [typ] of the evolve. *)
val get_evolve_type : (int * evolve_type * float * Disease.quality option) -> 
  evolve_type

(** [get_evolve_boost (a,b,c,d,e)] is a helper function to return the boost
    [bst] of the evolve. *)
val get_evolve_boost : (int * evolve_type * float * Disease.quality option) -> 
  float

(** [get_evolve_quality (a,b,c,d,e)] is a helper function to return the quality
    [qual] of the evolve. *)
val get_evolve_quality : (int * evolve_type * float * Disease.quality option) -> 
  Disease.quality option

(**[get_infected_pop t] gets the total infected population of [t] *)
val get_infected_pop : t -> int

(**[get_healthy_pop t] gets the total healthy population of [t] *)
val get_healthy_pop : t -> int 

(**[get_dead_pop t] gets the total dead population of [t] *)
val get_dead_pop : t -> int 

(**[get_spread_rt t] gets the spread rate stored in [t] *)
val get_spread_rt : t -> float

(**[get_inf_count t] gets the total infected countries of [t] *)
val get_inf_counts : t -> Disease.country list

(**[get_inf_count t] gets the total healthy countries of [t] *)
val get_health_counts : t -> Disease.country list

(** [spread_infection st dis_type dis] spreads the infection using the
    calculated infection rate based on the infected countries of [st], and the
    spread of [dis]. The infection is spread only through mutual connections;
    i.e. the countries are neighbors. The player's score in [st] is then 
    updated. *)
val spread_infection : t -> Disease.t -> Disease.disease -> int -> t

(**[commence_cure count st] initializes a cure with a start country [count],
   this occurs iff a cure has not been initialized and if the infected 
   population of [count] is more than n, otherwise returns the current cure
   from [st]*)
val commence_cure : Disease.country -> t -> 
  Disease.cure option

(**[get_cure st] is the cure of [st]*)
val get_cure : t -> Disease.cure option 

(**[get_virulency st] is the virulency of [st]*)
val get_virulency : t -> float

(**[assc_table dis_type] gets the necessary association table for [dis_type]
   to produce a valid gradient color map for disease spread visualization. *)
val assc_table :  Disease.t -> (char * float) list

(**[get_cure_score cur_op] is the current spreadibility of the cure *)
val get_cure_score : Disease.cure option -> float

(** [spread_cure st dis_type cure dis] spreads the cure using the
    calculated cure rate based on the infected countries of [st], and the
    spread of [cure]. The cure is spread only through mutual connections;
    i.e. the countries are neighbors.*)
val spread_cure : t -> Disease.t -> Disease.cure option -> Disease.disease -> int -> t

(** [kill st dis_type dis] spreads the virulency using the
    calculated infection rate based on the infected countries of [st], and the
    virulency of [dis], and promptly murders the populations.*)
val kill : t -> Disease.t -> Disease.disease -> int -> t

(**[get_traits t] returns the list of qualities currently in the traits of the
   disease in current state. *)
val get_traits : t -> Disease.quality list 