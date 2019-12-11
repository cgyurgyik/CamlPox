open OUnit2
open Disease
open Command
open State

(*[tester f name exp arg] is a helper function to test, 
  you provide the name, the expected result [exp], the function you want to 
  test [f] and the arguments to the function [arg]*)
let tester (name : string) (exp) (f) (arg) : test =  
  name >:: (fun _ -> assert_equal
               exp (f arg))

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(* JSON files for testing purposes *)
let country = from_json (Yojson.Basic.from_file "country.json")
let minimap = from_json (Yojson.Basic.from_file "minimap.json")
let st_minimap_1 = from_json (Yojson.Basic.from_file "minimap.json")
let st_minimap_2 = from_json (Yojson.Basic.from_file "minimap.json")
let st_minimap_3 = from_json (Yojson.Basic.from_file "minimap.json")
let hogwarts_dis = 
  match (find_country (country_list st_minimap_2) "Hogwarts") with
  | None -> failwith "Nonexistent"
  | Some x -> x.contains_disease <- true; x.population_inf <- 300; 
    x.population_healthy <- 0
let hogwarts = 
  match (find_country (country_list st_minimap_2) "Hogwarts") with
  | None -> failwith "Nonexistent"
  | Some x -> x
let hogwarts_2 = 
  match (find_country (country_list st_minimap_3) "Hogwarts") with
  | None -> failwith "Nonexistent"
  | Some x -> x
let durmstrang = 
  match (find_country (country_list st_minimap_3) "Durmstrang") with
  | None -> failwith "Nonexistent"
  | Some x -> x
let beauxbatons = 
  match (find_country (country_list st_minimap_3) "Beauxbatons") with
  | None -> failwith "Nonexistent"
  | Some x -> x

(* Disease Test Cases *)
let test_country = {country_name = "England"; 
                    total_population = 1010; 
                    population_healthy = 1000;
                    population_inf = 10;
                    population_dead = 0;
                    contains_disease = true;
                    climate = Temperate 1.2;
                    global_score = 100; 
                    neighbors = [];
                    id = 'a'}                   

let test_country_2 = {country_name = "Java"; 
                      total_population = 10000;
                      population_healthy = 10000; 
                      population_inf = 0;
                      population_dead = 0; 
                      contains_disease = true;
                      climate = Arid 1.2;
                      global_score = 1000;
                      neighbors = ["Alpha"; "Beta"];
                      id = 'b'}

let test_country_3 = {country_name = "Java II"; 
                      total_population = 10000; 
                      population_healthy = 5000;
                      population_inf = 5000; 
                      population_dead = 0;
                      contains_disease = true; 
                      climate = Tropical 1.3;
                      global_score = 1000; 
                      neighbors = ["Alpha"];
                      id = 'c'}

let test_country_4 = {country_name = "Java III"; 
                      total_population = 100; 
                      population_healthy = 100;
                      population_inf = 0; 
                      population_dead = 0;
                      contains_disease = true; 
                      climate = Tropical 1.3;
                      global_score = 5; 
                      neighbors = [];
                      id = 'd'}

let test_country_5 = {country_name = "yikes"; 
                      total_population = 600; 
                      population_healthy = 100;
                      population_inf = 373; 
                      population_dead = 127;
                      contains_disease = true; 
                      climate = Temperate 1.2;
                      global_score = 5; 
                      neighbors = ["ouch"];
                      id = 'e'}

let test_country_6 = {country_name = "increase virulency"; 
                      total_population = 2000; 
                      population_healthy = 1000;
                      population_inf = 1000; 
                      population_dead = 8000;
                      contains_disease = true; 
                      climate = Temperate 1.2;
                      global_score = 5; 
                      neighbors = ["test for it"];
                      id = 'f'}

let test_country_7 = {country_name = "increase virulency"; 
                      total_population = 2000; 
                      population_healthy = 1000;
                      population_inf = 1000; 
                      population_dead = 8000;
                      contains_disease = true; 
                      climate = Temperate 1.2;
                      global_score = 5; 
                      neighbors = ["test for it"];
                      id = 'g'}

let test_country_8 = {country_name = "increase virulency"; 
                      total_population = 2000; 
                      population_healthy = 1000;
                      population_inf = 1000; 
                      population_dead = 8000;
                      contains_disease = true; 
                      climate = Temperate 1.2;
                      global_score = 5; 
                      neighbors = ["test for it"];
                      id = 'h'}

let test_disease = {disease_name = "Camel Pox"; start_country = test_country; 
                    traits = []; spread = 0.75; virulency = 0.0}

let testa_disease = init_disease (country_list minimap) "Camel Pox" "Hogwarts" 
    "deadly"

let test_dis_disease = {disease_name = "Caml Pox"; start_country = test_country;
                        traits = [Tropical 1.15]; spread = 0.23; 
                        virulency = 0.057}
let test_dis1_disease = 
  {disease_name = "Caml Pox"; start_country = test_country; 
   traits = [Tropical 1.15]; spread = 0.23; virulency = 0.057}

let test_dis2_disease = 
  {disease_name = "Caml Pox"; start_country = test_country; 
   traits = [Chill 1.15; Arid 0.59]; spread = 0.23; virulency = 0.057}

let test_dis3_disease =
  {disease_name = "Caml Pox"; start_country = test_country; 
   traits = []; spread = 0.23; virulency = 0.057}

let test_dis4_disease = 
  {disease_name = "Caml Pox"; start_country = test_country; 
   traits = [Tropical 1.15]; spread = 0.23; virulency = 0.057}

let dis_test_cure = init_cure (List.hd (country_list minimap)) 

let dis_test_cure2 = init_cure (List.hd (country_list minimap)) 

(* %%% Disease Testing %%% *)

(** [make_country_list_test name dis expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [country_list dis] *)
let make_country_list_test
    (name : string)
    (dis : Disease.t)
    (expected_output : string list) : test = 
  name >:: (fun _ -> assert_equal true 
               (cmp_set_like_lists expected_output 
                  (get_countrynames (country_list dis))))

(** [make_evolution_list_test name dis expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [evolution_list dis] *)
let make_evolution_list_test
    (name : string)
    (dis : Disease.t)
    (expected_output : 
       (string * int * Disease.evolve_type * float * 
        Disease.quality option) list) : test = 
  name >:: (fun _ -> assert_equal true 
               (cmp_set_like_lists expected_output 
                  (get_evolvenames (evolution_list dis))))

(** [make_get_spread_test name disease expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [get_spread disease] *)
let make_get_spread_test
    (name : string)
    (disease : Disease.disease)
    (expected_output : float) : test =
  name >:: (fun _ -> assert_equal expected_output (get_spread disease))

(** [make_get_start_test name disease expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [get_start disease] *)
let make_get_start_test
    (name : string)
    (disease : Disease.disease)
    (expected_output : Disease.country) : test =
  name >:: (fun _ -> assert_equal expected_output (get_start_count disease))

(** [make_is_infected_test name count expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [is_infected count] *)
let make_is_infected_test
    (name : string)
    (count : Disease.country)
    (expected_output : bool) : test =
  name >:: (fun _ -> assert_equal expected_output (is_infected count))

(** [make_get_total_pop_test name count expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [get_totalpop count] *)
let make_get_total_pop_test
    (name : string)
    (count : Disease.country)
    (expected_output : int) : test =
  name >:: (fun _ -> assert_equal expected_output (get_total_pop count))

(** [make_get_healthy_pop_test name count expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [get_healthy_pop count] *)
let make_get_healthy_pop_test
    (name : string)
    (count : Disease.country)
    (expected_output : int) : test =
  name >:: (fun _ -> assert_equal expected_output 
               (Disease.get_healthy_pop count))

(** [make_get_inf_pop_test name count expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [get_inf_pop count] *)
let make_get_inf_pop_test
    (name : string)
    (count : Disease.country)
    (expected_output : int) : test =
  name >:: (fun _ -> assert_equal expected_output (get_inf_pop count))

(** [make_get_neighbors_test name country dis_type expected_output] constructs
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [get_neighbors country dis_type] *)
let make_get_neighbors_test
    (name : string)
    (country : Disease.country)
    (dis_type : Disease.t)
    (expected_output : country list) : test =
  name >:: 
  (fun _ -> assert_equal expected_output (get_neighbors country dis_type))

(** [make_get_global_score_test name count expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [get_global_score count] *)
let make_get_global_score_test
    (name : string)
    (count : Disease.country)
    (expected_output : int) : test =
  name >:: (fun _ -> assert_equal expected_output (get_global_score count))

(** [make_infect_test name count expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [infect count] *)
let make_infect_test
    (name : string)
    (count : Disease.country)
    (expected_output : bool) : test =
  name >:: (fun _ -> assert_equal expected_output
               (let _ = infect count in count.contains_disease))

(** [make_disinfect_test name count expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [disinfect count] *)
let make_disinfect_test
    (name : string)
    (count : Disease.country)
    (expected_output : bool) : test =
  name >:: (fun _ -> assert_equal expected_output
               (let _ = disinfect count in count.contains_disease))

(** [make_increase_inf_test name count expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [increase_inf count] *)
let make_increase_inf_test
    (name : string)
    (count : Disease.country)
    (spread : float)
    (expected_output : int * int) : test =
  name >:: (fun _ -> assert_equal expected_output
               (let _ = increase_infection count spread in 
                (count.population_healthy, count.population_inf)))

(** [make_decrease_inf_test name count expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [decrease_inf count] *)
let make_decrease_inf_test
    (name : string)
    (count : Disease.country)
    (spread : float)
    (expected_output : int * int) : test =
  name >:: (fun _ -> assert_equal expected_output
               (let _ = decrease_infection count spread in 
                (count.population_healthy, count.population_inf)))

(** [make_find_country_test name lst count expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [find_country lst count] *)
let make_find_country_test
    (name : string)
    (lst : Disease.country list)
    (count : string)
    (expected_output : country option) : test =
  name >:: (fun _ -> assert_equal expected_output
               (find_country lst count))

(** [make_init_disease_test name lst dis_name start_country dis_intensity 
    expected_output] constructs an OUnit test named [name] that asserts the 
    equality of [expected_output] with [init_disease lst dis_name 
    start_country dis_intensity] *)
let make_init_disease_test
    (name : string)
    (lst : Disease.country list)
    (dis_name : string)
    (start_country : string)
    (dis_intensity : string)
    (qual : Disease.quality list)
    (expected_output : disease) : test =
  name >:: (fun _ -> assert_equal expected_output
               (init_disease lst dis_name start_country dis_intensity qual))

(** [make_init_disease_raise_empty_exn_test name lst dis_name 
    start_country dis_intensity expected_output] constructs an OUnit
    test named [name] that asserts the raising of [Empty]
    with [init_disease lst dis_name start_country dis_intensity] given inputs
    that are expected to raise Empty *)
let make_init_disease_raise_empty_exn_test
    (name : string)
    (lst : Disease.country list)
    (dis_name : string)
    (start_country : string)
    (dis_intensity : string)
    (qual : Disease.quality list) : test =
  name >:: (fun _ ->
      let f = fun () -> init_disease lst dis_name start_country dis_intensity 
          qual in assert_raises (Disease.Empty) f)

(** [make_init_disease_raise_malf_exn_test name lst dis_name 
    start_country dis_intensity expected_output] constructs an OUnit
    test named [name] that asserts the raising of [Malformed]
    with [init_disease lst dis_name start_country dis_intensity] given inputs
    that are expected to raise Malformed *)
let make_init_disease_raise_malf_exn_test
    (name : string)
    (lst : Disease.country list)
    (dis_name : string)
    (start_country : string)
    (dis_intensity : string)
    (qual : Disease.quality list) : test =
  name >:: (fun _ ->
      let f = fun () -> init_disease lst dis_name start_country dis_intensity
          qual in assert_raises (Disease.Malformed) f)

(** [make_init_disease_raise_unknown_country_exn_test name lst dis_name 
    start_country dis_intensity expected_output] constructs an OUnit
    test named [name] that asserts the raising of [Malformed]
    with [init_disease lst dis_name start_country dis_intensity] given inputs
    that are expected to raise Unknwon Country *)
let make_init_disease_raise_unknown_country_exn_test
    (name : string)
    (lst : Disease.country list)
    (dis_name : string)
    (start_country : string)
    (dis_intensity : string)
    (qual : Disease.quality list) : test =
  name >:: (fun _ ->
      let f = fun () -> init_disease lst dis_name start_country dis_intensity 
          qual in assert_raises (Disease.UnknownCountry start_country) f)

(** make_get_cure_start_test name cure expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [get_cure_start cure] *)
let make_get_cure_start_test
    (name : string)
    (cure : Disease.cure)
    (expected_output : Disease.country) : test =
  name >:: (fun _ -> assert_equal expected_output (get_cure_start cure))

(** make_get_cure_spread_test name cure expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [get_cure_spread cure] *)
let make_get_cure_spread_test
    (name : string)
    (cure : Disease.cure)
    (expected_output : float) : test =
  name >:: (fun _ -> assert_equal expected_output (get_cure_spread cure))

(** make_update_cure_spread_test name cure spread expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [get_cure_spread cure] after updating the cure spread with
    [update_cure_spread cure spread] *)
let make_update_cure_spread_test
    (name : string)
    (cure : Disease.cure)
    (spread : float)
    (expected_output : float) : test =
  name >:: (fun _ -> (ignore (update_cure_spread cure spread));
             assert_equal expected_output (get_cure_spread cure))

let disease_tests = 
  [
    make_country_list_test "country list test I" minimap
      ["Hogwarts"; "Durmstrang"; "Beauxbatons"];
    make_evolution_list_test "evolution list test I" minimap
      [("Phoenix Feather",100, Spread, 0.2, None); 
       ("Elder Wand",500, Virulency, 0.1, None);
       ("Hedwig",2,Trait,0.0, Some (Chill 1.25))];
    make_get_spread_test "get spread test II" test_disease 0.75;
    make_get_start_test "get start test I" test_disease test_country;
    make_is_infected_test "is infected test I" test_country true;
    make_get_total_pop_test "get total population test I" test_country 1010;
    make_get_healthy_pop_test "get healthy population test I" test_country 1000;
    make_get_inf_pop_test "get infected population test I" test_country 10;
    make_get_global_score_test "get global score test I" test_country 100;
    tester "get id I" 'd' (fun (count)->get_id count) test_country_4;
    make_infect_test "infect test I" test_country true;
    make_disinfect_test "disinfect test I" test_country false;
    make_decrease_inf_test "decrease infection test I" test_country_3 0.5 
      (7500, 2500);
    make_init_disease_raise_unknown_country_exn_test 
      "init disease test II: raise Empty" (country_list minimap) 
      "Camel Pox" "" "dangerous" [];
    make_init_disease_raise_malf_exn_test 
      "init disease test III: raise Malformed" (country_list minimap) 
      "Camel Pox" "Hogwarts" "not correct" [];
    make_init_disease_raise_unknown_country_exn_test 
      "init disease test IV: raise UnknownCountry" (country_list minimap) 
      "Camel Pox" "Not A Country" "benign" [];
    make_find_country_test "find country test I" (country_list minimap) 
      "Jones" None;
    make_get_cure_start_test "get cure start I"
      dis_test_cure (List.hd (country_list minimap));
    make_get_cure_spread_test "get cure spread test I" dis_test_cure 0.000000001;
    make_update_cure_spread_test "update cure spread test I" dis_test_cure 5.0 5.0;
    (* get_virulency*)
    tester "get virulency disease I" 
      (0.0) (fun (dis) -> Disease.get_virulency dis) test_disease;
    tester "get virulency disease II" 
      (0.057) (fun (dis) -> Disease.get_virulency dis) test_dis_disease;
    (* get_dead_pop *)
    tester "get dead pop disease I"
      0 (fun (count) -> Disease.get_dead_pop count) test_country_2;
    tester "get dead pop disease II"
      127 (fun (count) -> Disease.get_dead_pop count) test_country_5;
    (* update_virulency *)
    tester "update virulency I"
      0.68 (fun (dis, vir) -> 
          update_virulency dis vir; Disease.get_virulency dis)
      (test_dis1_disease, 0.68);
    (* increase_virulency *)
    tester "increase virulency I"
      1800 (fun (count, vir) ->
          increase_virulency count vir; Disease.get_total_pop count)
      (test_country_6, 0.2);
    tester "increase virulency II"
      800 (fun (count, vir) ->
          increase_virulency count vir; Disease.get_inf_pop count)
      (test_country_7, 0.2);
    tester "increase virulency III"
      8200 (fun (count, vir) ->
          increase_virulency count vir; Disease.get_dead_pop count)
      (test_country_8, 0.2);
    tester "get climate I" (Tropical 1.3)
      (fun (count) -> get_climate count) test_country_4;
    tester "get climate II" (Temperate 1.2)
      (fun (count) -> get_climate count) test_country_8;
    tester "get disease traits II" []
      (fun (dis) -> Disease.get_traits dis) test_disease;
    tester "get disease traits II" [Tropical 1.15]
      (fun (dis) -> Disease.get_traits dis) test_dis1_disease;
    tester "get disease traits III" [Chill 1.15; Arid 0.59]
      (fun (dis) -> Disease.get_traits dis) test_dis2_disease;
    tester "update dis traits I" [Temperate 2.6]
      (fun (dis,mult) -> update_dis_traits dis mult; Disease.get_traits dis)
      (test_dis3_disease, Temperate 2.6);
    tester "update dis traits II" [Arid 0.3; Tropical 1.15]
      (fun (dis,mult) -> update_dis_traits dis mult; Disease.get_traits dis)
      (test_dis4_disease, Arid 0.3)
  ]

(* %%% State Testing %%% *)

(* State Test Cases *)
let test0_disease = init_disease (country_list minimap) "Camel Pox" "Hogwarts" 
    "benign" []
let test1_disease = init_disease (country_list minimap) "Camel Pox" "Hogwarts" 
    "deadly" []
let test2_disease = init_disease (country_list minimap) "Camel Pox" "Hogwarts" 
    "deadly" []
let test1_cure = init_cure (List.hd (country_list minimap)) 
let test2_cure = init_cure (List.hd (country_list minimap)) 
let test3_cure = init_cure (List.hd (country_list minimap)) 
let initial_state = init_state 0 test0_disease minimap None
    [("Phoenix Feather", 100, Spread, 0.2, None); 
     ("Elder Wand", 500, Virulency, 0.1, None);
     ("Hedwig",2,Trait,0.0, Some (Chill 1.2))] 0
let initial_state_changed_1 = init_state 0 test0_disease minimap None 
    [("Elder Wand", 500, Virulency, 0.1, None);
     ("Hedwig",2,Trait,0.0, Some (Chill 1.2))] 0
let initial_state_changed_2 = init_state 0 test0_disease minimap None 
    [("Hedwig",2,Trait,0.0, Some (Chill 1.2))] 0
let initial_state_2 = init_state 500 test0_disease minimap None [] 0
let test_state_1 = init_state 0 test1_disease minimap None [] 0
let test_state_2 = init_state 0 test1_disease minimap None [] 0
let test_state_3 = init_state 0 test1_disease minimap (Some test1_cure) [] 0
let test_state_4 = init_state 0 test1_disease minimap None [] 0
let test_state_5 = init_state 0 test1_disease minimap (Some test1_cure) [] 0
let test_state_6 = init_state 0 test1_disease minimap (Some test1_cure) [] 0
let test_state_7 = init_state 0 test_dis2_disease minimap 
    (Some test1_cure) [] 0

(** [make_uninfected_list_test name dis_type expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with uninfected_list dis_type] *)
let make_uninfected_list_test
    (name : string)
    (dis_type : Disease.t)
    (expected_output : Disease.country list) : test =
  name >:: (fun _ -> assert_equal expected_output (uninfected_list dis_type))

(** [make_infected_list_test name dis_type expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [infected_list dis_type] *)
let make_infected_list_test
    (name : string)
    (dis_type : Disease.t)
    (expected_output : Disease.country list) : test =
  name >:: (fun _ -> assert_equal expected_output (infected_list dis_type))

(** [make_uninfected_list_test name dis_type expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [uninfected_list dis_type] *)
let make_uninfected_list_test
    (name : string)
    (dis_type : Disease.t)
    (expected_output : Disease.country list) : test =
  name >:: (fun _ -> assert_equal expected_output (uninfected_list dis_type))

(** [make_calc_uninfected_test name dis_type expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [calc_uninfected dis_type] *)
let make_calc_uninfected_test
    (name : string)
    (dis_type : Disease.t)
    (expected_output : int) : test =
  name >:: (fun _ -> assert_equal expected_output (calc_uninfected dis_type))

(** [make_calc_infected_test name dis_type expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [calc_infected dis_type] *)
let make_calc_infected_test
    (name : string)
    (dis_type : Disease.t)
    (expected_output : int) : test =
  name >:: (fun _ -> assert_equal expected_output (calc_infected dis_type))

(** [make_calc_total_test name dis_type expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [calc_total dis_type] *)
let make_calc_total_test
    (name : string)
    (dis_type : Disease.t)
    (expected_output : int) : test =
  name >:: (fun _ -> assert_equal expected_output (calc_total dis_type))

(** [make_init_state_test name dis dis_type expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [init_state dis dis_type] *)
let make_init_state_test
    (name : string)
    (score : int)
    (dis : Disease.disease)
    (dis_type : Disease.t)
    (cure : Disease.cure option)
    (curr_upgrade : 
       (string * int * evolve_type * float * Disease.quality option) list)
    (purchased_val : int)
    (expected_output : State.t) : test =
  name >:: (fun _ -> assert_equal expected_output 
               (init_state score dis dis_type cure curr_upgrade purchased_val))

(** [make_get_player_score_test name st expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [get_player_score st] *)
let make_get_player_score_test
    (name : string)
    (st : State.t)
    (expected_output : int) : test =
  name >:: (fun _ -> assert_equal expected_output (get_player_score st))

(** make_upgrade_player_score_test name st score expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [get_player_score (upgrade_player_score st score)] *)
let make_upgrade_player_score_test
    (name : string)
    (st : State.t)
    (upgrade_val : int)
    (dis : Disease.disease)
    (dis_type : Disease.t)
    (upgrade_vals : int)
    (expected_output : int) : test =
  name >:: (fun _ ->
      assert_equal expected_output 
        (get_player_score 
           (upgrade_player_score st upgrade_val dis dis_type upgrade_vals)))

(** [make_get_infectd_poulation_test name st expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [get_infected_pop st] *)
let make_get_infected_population_test
    (name : string)
    (st : State.t)
    (expected_output : int) : test =
  name >:: (fun _ -> assert_equal expected_output (get_infected_pop st))

(** [make_get_healthy_population_test name st expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [get_healthy_pop st] *)
let make_get_healthy_population_test
    (name : string)
    (st : State.t)
    (expected_output : int) : test =
  name >:: (fun _ -> assert_equal expected_output (get_healthy_pop st))


(** [make_get_inf_counts_test name st expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [get_inf_counts st] *)
let make_get_inf_counts_test
    (name : string)
    (st : State.t)
    (expected_output : Disease.country list) : test =
  name >:: (fun _ -> assert_equal expected_output (get_inf_counts st))


(** [make_get_health_counts_test name st expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [get_health_counts st] *)
let make_get_health_counts_test
    (name : string)
    (st : State.t)
    (expected_output : Disease.country list) : test =
  name >:: (fun _ -> assert_equal expected_output (get_health_counts st))

(** [make_spread_infection_test st dis_type dis expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [spread_infection st dis_type dis]. *)
let make_spread_infection_test
    (name : string)
    (st : State.t)
    (dis_type : Disease.t)
    (dis : Disease.disease)
    (purchased_val : int)
    (expected_output : State.t) : test =
  name >:: (fun _ -> 
      assert_equal expected_output
        (spread_infection st dis_type dis purchased_val))

(** [make_get_curr_upgrades_test name st expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [get_curr_upgrades st] *)
let make_get_curr_upgrades_test
    (name : string)
    (st : State.t)
    (expected_output : 
       (string * int * evolve_type * float * Disease.quality option) list) 
  : test =
  name >:: (fun _ -> assert_equal expected_output (get_curr_upgrades st))

(** [make_change_upgrades_test name upgrade_name st expected_output] constructs 
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [change_upgrades upgrade_name st] *)
let make_change_upgrades_test
    (name : string)
    (upgrade_name : string)
    (st : State.t)
    (expected_output : State.t) : test =
  name >:: (fun _ -> assert_equal expected_output 
               (change_upgrades st upgrade_name))

(** [make_legal_evolve_test name t upgrade_name expected_output] constructs an 
    OUnit test named [name] that asserts the equality of [expected_output] with
    [legal_evolve t upgrade_name *)
let make_legal_evolve_test
    (name : string)
    (st : State.t)
    (upgrade_name : string)
    (expected_output : bool) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (legal_evolve st upgrade_name))

(** [make_get_upgrade_value_test name t upgrade_name expected_output] 
    constructs an OUnit test named [name] that asserts the equality of 
    [expected_output] with [get_upgrade_value t upgrade_name] *)
let make_get_upgrade_values_test
    (name : string)
    (st : State.t)
    (upgrade_name : string)
    (expected_output : 
       (int * evolve_type * float * Disease.quality option)) : test =
  name >:: (fun _ -> assert_equal expected_output 
               (get_upgrade_values st upgrade_name))

(** [make_get_spread_rt_test name t expected_output] 
    constructs an OUnit test named [name] that asserts the equality of 
    [expected_output] with [get_spread_rt t] *)
let make_get_spread_rt_test
    (name : string)
    (st : State.t)
    (expected_output : float) : test =
  name >:: (fun _ -> assert_equal expected_output (get_spread_rt st))

(** [make_commence_cure_test name count dis st expected_output]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] with [commence_cure name count dis st] *)
let make_commence_cure_test
    (name : string)
    (count : Disease.country)
    (dis : Disease.disease)
    (st : State.t)
    (expected_output : Disease.cure option) : test =
  name >:: (fun _ -> assert_equal expected_output (commence_cure count st))

(** [make_get_cure_test name t expected_output] 
    constructs an OUnit test named [name] that asserts the equality of 
    [expected_output] with [get_cure t] *)
let make_get_cure_test
    (name : string)
    (st : State.t)
    (expected_output : Disease.cure option) : test =
  name >:: (fun _ -> assert_equal expected_output (get_cure st))

(** [make_get_cure_score_test name cure_op expected_output
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] with [get_cure_score cure_op] *)
let make_get_cure_score_test
    (name : string)
    (cure_op : Disease.cure option)
    (expected_output : float) : test =
  name >:: (fun _ -> assert_equal expected_output (get_cure_score cure_op))

let state_tests = 
  [
    make_uninfected_list_test "uninfected_list test I" st_minimap_1
      (country_list st_minimap_1);
    make_infected_list_test "infected_list test I" st_minimap_1 [];
    make_infected_list_test "infected_list test II" st_minimap_2 [hogwarts];
    make_infected_list_test "infected_list test III" country [];
    make_calc_uninfected_test "calculate uninfected list test I" 
      st_minimap_1 1850; 
    make_calc_uninfected_test "calculate uninfected list test II" 
      st_minimap_2 1550;
    make_calc_uninfected_test "calculate uninfected list test III" country 0; 
    make_calc_infected_test "calculate infected list test I" st_minimap_1 0;
    make_calc_infected_test "calculate infected list test II" st_minimap_2 300;
    make_calc_infected_test "calculate infected list test III" country 0;
    make_calc_total_test "calculate total test I" minimap 1850;
    make_calc_total_test "calculate total test II" country 0;
    make_init_state_test "init_state test I" 0 test0_disease minimap None 
      [("Phoenix Feather", 100, Spread, 0.2, None); 
       ("Elder Wand", 500, Virulency, 0.1, None);
       ("Hedwig",2,Trait,0.0, Some (Chill 1.2))] 0
      initial_state;
    make_init_state_test "init_state test II" 500 test0_disease minimap 
      None [] 0 initial_state_2;
    make_get_curr_upgrades_test "get_curr_upgrades test I" initial_state
      [("Phoenix Feather", 100, Spread, 0.2, None); 
       ("Elder Wand", 500, Virulency, 0.1, None);
       ("Hedwig",2,Trait,0.0, Some (Chill 1.2))];
    make_get_curr_upgrades_test "get_curr_upgrades test I" initial_state_2
      [];
    make_change_upgrades_test "change upgrade test I" 
      "Phoenix Feather" initial_state initial_state_changed_1;
    make_change_upgrades_test "change upgrade test II" 
      "Elder Wand" initial_state_changed_1 initial_state_changed_2;
    make_get_player_score_test "get_player_score test I" initial_state 0;
    make_get_player_score_test "get_player_score test II" initial_state_2 500;
    make_upgrade_player_score_test "upgrade player score test I"
      test_state_4 100 test1_disease minimap 100 (-100);
    make_upgrade_player_score_test "upgrade player score test II"
      test_state_5 (-5) test1_disease minimap (-5) 5;
    make_upgrade_player_score_test "upgrade player score test III"
      test_state_5 1 test1_disease minimap 100000 (-1);
    make_get_infected_population_test "get infected population test I"
      test_state_5 (calc_infected minimap);
    make_get_healthy_population_test "get healthy population test I"
      test_state_5 (calc_uninfected minimap);
    make_get_inf_counts_test "get infected countries test I"
      test_state_5 (infected_list minimap);
    make_get_health_counts_test "get healthy countries test I"
      test_state_5 (uninfected_list minimap);
    make_uninfected_list_test "uninfected list test I" 
      st_minimap_3 [hogwarts_2; durmstrang; beauxbatons];
    make_uninfected_list_test "uninfected list test II" 
      st_minimap_2 [durmstrang; beauxbatons];
    make_legal_evolve_test "legal evolve test I" 
      initial_state "Phoenix Feather" true;
    make_legal_evolve_test "legal evolve test II" 
      initial_state "Elder Wand" true;
    make_legal_evolve_test "legal evolve test III" 
      initial_state "greg" false;
    make_legal_evolve_test "legal evolve test IV" 
      initial_state "Elder Wand zero" false;
    make_get_upgrade_values_test "get upgrade val test I"
      initial_state "Phoenix Feather" (100, Spread, 0.2, None);
    make_get_upgrade_values_test "get upgrade val test I"
      initial_state "Elder Wand" (500, Virulency, 0.1, None);
    make_get_spread_rt_test "get spread rate test I" initial_state 0.005;
    make_get_spread_rt_test " get spread rate test II" test_state_1 0.05;
    make_commence_cure_test "commence cure test I" 
      (List.hd (country_list minimap)) test0_disease test_state_6 
      (Some test1_cure);
    make_commence_cure_test "commence cure test II" 
      (List.hd (country_list minimap)) test0_disease test_state_5
      (Some test1_cure);
    make_get_cure_test "get cure test I" test_state_1 None;
    make_get_cure_test "get cure test II" initial_state None;
    make_get_cure_test "get cure test III" initial_state_changed_1 None;
    make_get_cure_test "get cure test IV" test_state_3 (Some test1_cure);
    make_get_cure_score_test "get cure score test I" (Some test1_cure) 
      0.000000001;
    make_get_cure_score_test "get cure score test II" 
      ((ignore (update_cure_spread test2_cure 4.7)); Some test2_cure) 4.7;
    make_get_cure_score_test "get cure score test III"
      ((ignore (update_cure_spread test3_cure (-2.3))); Some test3_cure) (-2.3);
    tester "update_dis_spread test I" (1.0)  
      (fun (dis,value) -> update_dis_spread dis value; get_spread dis)
      (({disease_name = "Caml Pox"; start_country = test_country; traits = [];
         spread = 0.0001; virulency = 0.001}), 
       1.0);
    tester "update_dis_spread test II" (0.0099)  
      (fun (dis,value) -> update_dis_spread dis value; get_spread dis)
      (({disease_name = "Caml Pox"; start_country = test_country; traits = [];
         spread = 0.0001; virulency = 0.001}), 
       0.0099);
    tester "calc dead test I" 0 (fun (dis) -> calc_dead dis) minimap;
    tester "get dead pop test I" 0 (fun (st) -> get_dead_pop st) test_state_1;
    tester "get virulency test I" 
      0.001 (fun (st) -> State.get_virulency st) test_state_1;
    tester "get traits st test I" []
      (fun (st) -> State.get_traits st) test_state_6;
    tester "get traits st test II" [Chill 1.15; Arid 0.59]
      (fun (st) -> State.get_traits st) test_state_7;
    tester "get evolve quality I" (Some (Arid 5.0))
      (fun (a, b, c, d) -> get_evolve_quality (a,b,c,d))
      (1, Trait, 0.0, Some (Arid 5.0));
    tester "get evolve quality II" (None)
      (fun (a, b, c, d) -> get_evolve_quality (a,b,c,d))
      (1, Virulency, 0.5, None)
  ]

(* %%% Command Testing %%% *)

(** [make_parse_test name str expected_output] constructs an OUnit test named
    [name] that asserts the equality of [expected_output] with [parse str] *)
let make_parse_test
    (name : string)
    (str : string)
    (expected_output : Command.command) : test = 
  name >:: (fun _ -> assert_equal expected_output (parse str))

(** [make_parse_malf_test name str] constructs an OUnit test named [name] that 
    asserts the raising of [Malformed] with [parse str] *)
let make_parse_malf_test
    (name : string)
    (str : string) : test =
  name >:: (fun _ ->
      let f = fun () -> parse str in assert_raises (Command.Malformed) f)

(** [make_parse_empty_test name str] constructs an OUnit test named [name] that 
    asserts the raising of [Empty] with [parse str] *)
let make_parse_empty_test
    (name : string)
    (str : string) : test =
  name >:: (fun _ ->
      let f = fun () -> parse str in assert_raises (Command.Empty) f)

let command_tests = 
  [
    make_parse_test "Parse Test I" "evolve f" (Evolve ["f"]);
    make_parse_test "Parse Test II" "score" Score;
    make_parse_test "Parse Test III" "quit" Quit;
    make_parse_test "Parse Test IV" "next" Next;
    make_parse_test "Parse Test V" "evolve yikes" (Evolve ["yikes"]);
    make_parse_test "Parse Test VI" "score !#234" Score;
    make_parse_test "Parse Test VII" "quit score" Quit;
    make_parse_test "Parse Test VIII" "next evolve" Next;
    make_parse_test "Parse Test IX" 
      "evolve multi item" (Evolve ["multi"; "item"]);
    make_parse_test "Parse Test X" "n" Next;
    make_parse_test "Parse Test XI" "upgrades" Upgrades;
    tester "Parse Test XII" (Get ["f"])
      (fun (str) -> parse str) "get f";
    tester "Parse Test XIII" (Get ["California"])
      (fun (str) -> parse str) "get California";
    make_parse_malf_test "Parse Malf Test I" "evolvexyz";
    make_parse_malf_test "Parse Malf Test II" "random";
    make_parse_malf_test "Parse Malf Test III" "hello score";
    make_parse_empty_test "Parse Empty Test I" "";
  ]

let suite =
  "test suite for A7"  >::: List.flatten [
    disease_tests;
    state_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite
