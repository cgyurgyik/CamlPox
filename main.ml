open Disease
open Command
open State

(* Visualization, Print *)

(** [contains mlist element] is a helper function to determine 
    whether [element] is in [mlist].*)
let rec contains mlist element =
  match mlist with
  | [] -> false
  | h::t -> if h = element then true else contains t element

(**[make d] takes in a date [d] and returns the next day, following Gregorian 
   calendar rules to include leap years. *)
let make d = 
  let months = [("Jan",0); ("Feb",1); ("Mar",2); ("Apr",3); 
                ("May",4); ("Jun",5); ("Jul",6); ("Aug",7); ("Sep",8); ("Oct",9); 
                ("Nov",10); ("Dec",11)] in
  let thirty = ["Sep"; "Apr"; "Jun"; "Nov"] in
  let thir1 = ["Jan"; "Mar"; "May"; "Jul"; "Aug";
               "Oct"; "Dec"] in
  let leap y = ((y mod 4 = 0) && (y mod 100 <> 0)) || 
               (y mod 400 = 0) in 
  let incr = function 
    | (m,d,y) -> 
      let day = 
        if d = 30 && contains thirty m then 1 else
        if d = 31 && contains thir1 m then 1 else
        if d = 28 && m = "Feb" && (not (leap y)) then 1 else
        if d =29 && m = "Feb" && (leap y) then 1 else
          d + 1 in 
      let month = 
        let num = (List.assoc m months) mod 12 in
        if day = 1 then begin  
          fst (List.nth months ((num + 1) mod 12 )) end else fst (List.nth months (num)) in
      let year = 
        if m = "Dec" && d = 31 then y+1 else y in
      (month, day, year) in 
  let (x,y,z) = incr d in
  Printf.printf "\x1b[38;5;15m                                                    [%s %i %i]" x y z;
  (x,y,z)

(** [print_screen d sleep_time] takes in a file d and prints it
    one line at a time, clearing the screen after each line is printed. There is a
    sleep period of [sleep_time] between each print.*)
let print_screen d sleep_time =
  ignore(Sys.command "clear");
  let dir = open_in d in
  let rec read_helper x =
    match (input_line x) with
    | exception End_of_file -> close_in x
    | (str:string) ->   print_endline "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
      print_endline str;
      ignore(Sys.command ("sleep " ^ sleep_time));
      ignore(Sys.command "clear"); read_helper x
  in read_helper dir

(** [print_file f] prints each line of [f] *)
let rec print_file f = 
  try (
    let s = Pervasives.input_line f in 
    ANSITerminal.(print_string [red;] (s^"\n"));
    flush stdout); print_file f with 
  | End_of_file -> Pervasives.close_in f

(** [print_country_list lst] is a helper function to print a list of 
    country names in [lst] *)
let print_country_list lst =
  let sorted = List.sort compare lst in
  let _ = print_endline ""; print_string "{ " in 
  let rec print_lst lst = 
    match lst with
    | [] -> print_endline "";
    | [h] -> ANSITerminal.(print_string [blue] (h.country_name));
    | h::t -> ANSITerminal.(print_string [blue] h.country_name);
      ANSITerminal.(print_string [blue] ", "); print_lst t in
  print_lst sorted; print_string " }"

(** [print_evolve_info] is a helper function to print the information 
    for one evolve. *)
let print_evolve_info (name,v,e_type,boost,trait) = 
  ANSITerminal.(print_string [blue] (name ^ " :"));
  ANSITerminal.(print_string [white] (" " ^ (string_of_int(v))));
  let e = match e_type with
    | Spread -> "[Spread Upgrade]"
    | Virulency -> "[Virulency Upgrade]"
    | Trait -> "[Trait Upgrade]" in
  ANSITerminal.(print_string [cyan] (" " ^ (e)));
  begin
    match e_type with 
    | Spread ->
      ANSITerminal.(print_string [white] (" ==> Boost: " ^ 
                                          (string_of_float(boost))));
    | Virulency ->
      ANSITerminal.(print_string [white] (" ==> Boost: " ^ 
                                          (string_of_float(boost))));
    | Trait -> 
      begin 
        match trait with 
        | Some Tropical _ -> ANSITerminal.(print_string [white] " Tropical") ;
        | Some Temperate _ -> ANSITerminal.(print_string [white] " Temperate") ;
        | Some Arid _ -> ANSITerminal.(print_string [white] " Arid") ;
        | Some Chill _ -> ANSITerminal.(print_string [white] " Chill") ;
        | None -> ANSITerminal.(print_string [white] "None") ;
      end  
  end;
  print_endline ""

(** [print_evolve_list lst] is a helper function to print a list of 
    evolve names in [lst]. Informs the player when there is nothing available. *)
let print_evolve_list lst =
  let _ = print_endline ""; print_endline "{" in 
  let rec print_lst lst = 
    match lst with
    | [] -> print_endline "";
    | [h] -> print_evolve_info h;
    | h::t -> print_evolve_info h; print_lst t in
  if lst = [] then 
    let _ = ANSITerminal.(print_string [blue] " There are no upgrades currently available. ")
    in print_endline ""
  else print_lst lst; print_string "}"

(** [get_climate_string clim] is a helper function to return the climate 
    string associated with the climate [clim]. *)
let get_climate_string clim =
  match clim with 
  |Temperate _ -> "Temperate"
  |Chill _ -> "Chill"
  |Arid _ -> "Arid"
  |Tropical _ -> "Tropical"

(** [print_climate st] prints the climate of [st] in an aesthetic fashion.*)
let print_climate st = 
  print_string "\x1b[38;5;172m[";
  let rec print_lst lst = 
    print_string ("\x1b[0m");
    match lst with
    | [] -> print_string "";
    | [h] -> print_string (get_climate_string h);
    | h::t -> print_string (get_climate_string h);
      print_string ", "; print_lst t in
  print_lst (get_traits st);
  print_string "\x1b[38;5;172m]";
  print_string "\x1b[0m"

(** [subs s n] is a helper function to return the substring of a string [s]. If 
    [s] is less than or equal to n characters, it returns the a string of that 
    size. If [s] is greater than n characters, 
    it returns the first n characters. *)
let subs s n = 
  if String.length s <= n then s else 
    String.sub s 0  n 

(** [print_current_status st] prints a list of infected countries and 
    uninfected countries in [st], 
    with the total number of infected and uninfected populations.*)
let print_current_status st dis =
  ANSITerminal.(print_string[yellow] "\rInfected States:");
  print_country_list ((get_inf_counts st));
  print_string "\n--------------------------------------------------------------------------------------------------------------------------------\n";
  ANSITerminal.(print_string[green] "Healthy States:");
  print_country_list ((get_health_counts st));
  print_string "\n--------------------------------------------------------------------------------------------------------------------------------\n";
  ANSITerminal.(print_string[cyan] "Populations:\n");
  ANSITerminal.(print_string[yellow] ("Infected: ["));
  print_string (string_of_int (get_infected_pop st));
  ANSITerminal.(print_string[yellow] ("] "));
  ANSITerminal.(print_string[green] ("Healthy: ["));
  print_string (string_of_int (get_healthy_pop st));
  ANSITerminal.(print_string[green] ("] "));
  ANSITerminal.(print_string[red] ("Dead: ["));
  print_string (string_of_int (get_dead_pop st));
  ANSITerminal.(print_string[red] ("] "));
  print_string "        \x1b[38;5;172mThe disease is most infectous in ";
  if get_traits st = [] then print_string "\x1b[38;5;172mno" 
  else print_climate st;
  print_string " \x1b[38;5;172mweather.";
  let total_pop = float_of_int (get_infected_pop st + get_healthy_pop st  ) in 
  ANSITerminal.(print_string[yellow] ("\nPercent Infected: ["));
  let infected = 
    if (get_healthy_pop st) > 10000000 then max (get_infected_pop st) 1000 
    else get_infected_pop st in 
  let p_inf = if total_pop = 0.0 then 0.0 else ((float_of_int(infected))/.total_pop)*.100.0 in
  let p_health = if total_pop = 0.0 then 0.0 else ((float_of_int(get_healthy_pop st))/.total_pop)*.100.0 in
  print_string 
    ((subs (string_of_float p_inf) 3) ^ "%");
  ANSITerminal.(print_string[yellow] ("] "));
  ANSITerminal.(print_string[green] ("Percent Healthy: ["));
  print_string 
    (  ( subs (string_of_float p_health) 3 )^ "%");
  ANSITerminal.(print_string[green] ("] "));
  print_string("\x1b[38;5;88m            The disease is " ^ "[" ^ "\x1b[38;5;15m" ^ (subs (string_of_float (min 100.0 ((get_spread dis) *. 100.0))) 5) ^ "%\x1b[38;5;88m] infectious\x1b[0m");
  print_string("\x1b[38;5;27m      The cure is " ^ "[" ^ "\x1b[38;5;15m" ^ (subs(string_of_float (min 100.0 (get_cure_score (get_cure st) *. 100.0))) 5) ^ "%\x1b[38;5;27m] effective\x1b[0m");
  print_endline ""

(** [cursor] prints to display a cursor for a user to enter a command *)
let cursor = 
  fun () -> print_endline ""; print_string "> "

(** [find v ls] is a helper function that finds an element [v] in list [ls], 
    and returns false otherwise. *)
let find v ls = 
  List.fold_left (fun acc x -> if x = v || acc then true else acc ) false ls 

(** [color n] colors a portion of the map using the respective
    association table with [n]. [n] represents the color of the string.
     This allows for the spread of the disease to be colored
    on any table with a valid association table. *)
let color n = 
  print_string ("\x1b[38;5;"^(string_of_int n)^"m█")
let rec make_map f dis_type = 
  try (
    let split_char word = List.init (String.length word) (String.get word) in 
    let s = Pervasives.input_line f in 
    let splitted = split_char s in 
    (*List.iter (print_char) splitted;*)
    let assc = (assc_table dis_type) in
    let rec rec_print ls = 
      match ls with 
      | [] -> ()
      | h::t -> 
        let find =  List.assoc_opt h assc in 
        match find with 
        | None -> 
          begin
            match h with 
            | '!' -> color 27
            | '@' -> color 39
            | '#' -> color 51
            | '$' -> color 49
            | '"' -> color 48
            | '^' -> color 47
            | '&' -> color 82
            | '*' -> color 118
            | '(' -> color 154
            | ')' -> color 190
            | ';' -> color 184
            | '+' -> color 178
            | '=' -> color 172
            | '{' -> color 166
            | '}' -> color 160
            | ' ' -> color 15
            | '-'
            | '|' -> color 232
            | _ -> print_string ("\x1b[38;5;232;48;5;15;1m"^(Char.escaped h));
              print_string ("\x1b[0m")
          end;
          rec_print t
        | Some found -> 
          if found = 0.0 then color 27 else
          if found <= 0.05 then color 39 else
          if found <= 0.07 then color 51 else
          if found <= 0.14 then color 49 else
          if found <= 0.21 then color 48 else
          if found <= 0.28 then color 47 else
          if found <= 0.35 then color 82 else
          if found <= 0.42 then color 118 else
          if found <= 0.49 then color 154 else
          if found <= 0.56 then color 190 else
          if found <= 0.63 then color 184 else
          if found <= 0.70 then color 178 else
          if found <= 0.77 then color 172 else
          if found <= 0.85 then color 166 else
          if found <= 0.92 then color 160 else
            color 88;
          rec_print t in 
    rec_print splitted; print_string "\n"; make_map f dis_type) with 
    End_of_file -> Pervasives.close_in f

(* Game Functionality *)

(* A reference to determine the sum of the point values of purchased items. *)
let purchased_values = ref 0

(**[index_of_dir s] opens an input channel from the file [s] *)
let rec index_of_dir s f=
  try(
    let open_file = Pervasives.open_in s in
    f open_file;
  ) with Unix.Unix_error (Unix.ENOENT, "opendir", d) -> raise Not_found

(** [callable_dis_int str] Determines whether [str] is a correct disease 
    intensity. If it is not, the player is informed. If "quit" is entered, the 
    game is ended. *)
let rec callable_dis_int str = 
  match String.trim str with
  | "quit" -> 
    print_endline "";
    ANSITerminal.(print_string [white] 
                    "\nYou have quit the game. Thanks for playing! :-)\n");
    print_endline "";
    Pervasives.exit 0
  | "benign"
  | "deadly"
  | "dangerous" -> str
  | "" -> ANSITerminal.(print_string [red] " You did not enter anything. Please try again. ");
    cursor (); callable_dis_int (read_line ())
  | _ -> (ANSITerminal.
            (print_string [blue] str; print_string [red] " 
         is not a correct intensity.
         Your choices are: "; print_string [yellow] 
               "benign, dangerous, or deadly"; 
             cursor ())); callable_dis_int (read_line ())

(** [callable_strt_count str] Determines whether [str] is a correct start 
    country. If it is not, the player is informed. If "quit" is entered, 
    the game is ended. *)
let rec callable_strt_count d strs = 
  let str = String.trim strs in
  match str with
  | "quit" ->  print_endline "";
    ANSITerminal.(print_string [white] 
                    "\nYou have quit the game. Thanks for playing! :-)\n");
    print_endline "";
    Pervasives.exit 0
  | "" ->   ANSITerminal.(print_string [red] 
                            "You did not enter anything. Please try again. "; 
                          cursor ()); callable_strt_count d (read_line ())
  | _ ->
    match find_country (country_list d) str with  
    | None ->  (ANSITerminal.
                  (print_string [blue] str; print_string [red] 
                     " is not one of the states in the list. Please try again"; 
                   cursor ())); callable_strt_count d (read_line ())
    | Some c -> str

(* Game State *)

(** [won st] is the helper function that is true when the adventurer has 
    won the game with state [st] and false otherwise. *)
let won st = 
  (get_healthy_pop st + get_infected_pop st) = 0

(** [lost st] is the helper function that is true when the adventurer has 
    lost the game with state [st] and false otherwise. *)
let lost st = 
  get_infected_pop st = 0

(** [initialize_disease d] initializes the name, start country, and intensity 
    of the disease [d]. If "quit" is entered, the game is ended. *)
let initialize_disease d = 
  let dis_name = read_line
      (ANSITerminal.
         (print_string [white]"\nPlease enter a name for the disease:\n"; 
          cursor ())) in 
  match dis_name with
  | "quit" ->  print_endline "";
    ANSITerminal.(print_string [white] 
                    "\nYou have quit the game. Thanks for playing! :-)\n");
    print_endline "";
    Pervasives.exit 0
  | _ ->
    print_string ("
    \x1b[38;5;196m
------------------------------------------------------
        \x1b[38;5;69m" ^dis_name ^"\x1b[38;5;15m is a great choice!
        You must have done well on Prelim I.          
        Please enter the intensity of the disease.    
        Your choices are: \x1b[38;5;160;5m              
        benign, dangerous, or deadly                  \x1b[0;38;5;160m
------------------------------------------------------");
    print_string ("\x1b[0m\n");              
    cursor ();
    let dis_intensity = callable_dis_int (read_line ()) in
    print_string ("
    \x1b[38;5;196m
------------------------------------------------------
      \x1b[38;5;15mPlease enter the starting state.
      You may choose from the following states\x1b[38;5;196m
------------------------------------------------------");
    print_endline "\x1b[0m";
    print_country_list
      (country_list d); print_endline "\x1b[0m";
    index_of_dir "US_map_country_sel.txt" print_file;
    cursor ();
    let start_country = callable_strt_count d (read_line ()) in
    ignore(Sys.command ("clear"));
    init_disease (country_list d) dis_name start_country dis_intensity 

(** [step dis st dis_type n] is a helper function to execute the "Next"
    command using [dis], [st], [dis_type]. It executes it [n] number of times.
    Requires: [n] >= 1. *)
let rec step dis st dis_type n d =
  if n = 0 then (st,d)
  else 
    let new_spread = spread_infection st dis_type dis (!purchased_values) in
    let new_kill = kill new_spread dis_type dis (!purchased_values) in
    let new_st = spread_cure new_kill dis_type
        (get_cure new_kill) dis (!purchased_values) in
    ignore(Sys.command "clear");
    print_current_status new_st dis;
    index_of_dir "US_visual_map.txt" make_map dis_type;
    let new_d = make d in
    if won new_st then 
      let _ = ANSITerminal.(print_string [yellow]
                              ("\nCongratulations, you have won the game! \n")) 
      in
      let _ = ANSITerminal.(print_string [blue]
                              (get_disease_name dis )) in
      ANSITerminal.(print_string[yellow] 
                      (" is unstoppable. Your final score is: " ^ string_of_int 
                         (get_player_score new_st)));
      print_endline "";
      if get_curr_upgrades new_st = [] then
        ANSITerminal.(print_string [yellow] 
                        "\nYou used all of the items! \n" )
      else
        ANSITerminal.(print_string [white] 
                        "\nHere are the items you have left over: \n" ); 
      print_evolve_list (get_curr_upgrades new_st);
      print_endline "";
      ANSITerminal.(print_string[yellow] 
                      "\nWe think highly of you now.\n");
      print_endline ""; Pervasives.exit 0
    else
    if lost new_st then 
      let _ = ANSITerminal.(print_string [yellow]
                              ("\n Looks like the disease could not outcompete the cure \n"));
        ANSITerminal.(print_string[yellow] 
                        ("Your final score is: " ^ string_of_int 
                           (get_player_score new_st))) in
      print_endline "\nBetter luck next time!"; Pervasives.exit 0
    else
      ANSITerminal.(print_string[white]) "\nPlease enter a new command:\n"; 
    cursor(); 
    ignore (Sys.command "sleep .2s");
    step dis new_st dis_type (n-1) new_d

(** [repl dis st input] is the helper function that executes the Read Eval
    Print Loop cycle continuously until the user enters "quit" which will then
    end the game or if the user has won the game which will automatically end 
    the game. Different prompts and messages will be printed according to the 
    user's input commands each time.*)
let rec repl dis st dis_type input d = 
  match (parse input) with 
  | Upgrades -> print_evolve_list (get_curr_upgrades st);
    cursor (); repl dis st dis_type (read_line ()) d
  | Get state ->
    let statename = String.trim(String.concat " " state) in
    let countries = (country_list dis_type) in
    if (List.mem statename (get_countrynames countries)) then begin
      let found =match find_country countries statename with 
        |None -> failwith "there should be a country" 
        |Some n -> n in
      print_endline "\x1b[38;5;160m";
      print_endline "--------------------------------------------------------";
      print_endline("\x1b[38;5;27mName: "^"\x1b[38;5;15;1m"^statename);
      print_endline("\x1b[38;5;49;0mClimate: "^ "\x1b[38;5;15m"^(get_climate_string (get_climate found)));
      print_endline("\x1b[38;5;190mHealthy " ^ "\x1b[38;5;15m"^string_of_int(Disease.get_healthy_pop found));
      print_endline("\x1b[38;5;178mInfected " ^ "\x1b[38;5;15m"^string_of_int(Disease.get_inf_pop found));
      print_endline("\x1b[38;5;160mKilled " ^ "\x1b[38;5;15m"^string_of_int(Disease.get_dead_pop found));
      print_endline "\x1b[38;5;160m--------------------------------------------------------";
      print_string "\x1b[0m";
      cursor (); repl dis st dis_type (read_line ()) d; end
    else
      ANSITerminal.(print_string [red] "\nPlease enter a valid state.\n");
    ANSITerminal.(print_string [white] "\nPlease enter a new command.\n");
    cursor (); repl dis st dis_type (read_line ()) d;
  | Evolve upgrade-> 
    let upgrade_name = String.trim(String.concat " " upgrade) in
    if legal_evolve st upgrade_name then 
      (* the upgrade name is in the current upgrade list *)
      let ev_value,ev_type,ev_boost, qual = (get_upgrade_values st upgrade_name) in
      if get_player_score st >= (ev_value) then 
        (* the player has enough points *)
        let temp = !purchased_values in purchased_values := (temp + ev_value); 
        let new_upgrades = (change_upgrades st upgrade_name) in
        let new_state =  
          upgrade_player_score new_upgrades ev_value dis dis_type (!purchased_values) in
        let _ = match ev_type with
          | Spread -> 
            update_dis_spread dis ((get_spread_rt new_state) +. ev_boost)
          | Virulency -> 
            update_virulency dis ((get_virulency new_state) +. ev_boost)
          | Trait -> 
            update_dis_traits dis (
              match qual with
              |None -> failwith "No traits in the upgrade."
              |Some n -> n)
        in
        print_endline "";
        ANSITerminal.(print_string [white] "You acquired the ");
        ANSITerminal.(print_string [blue] upgrade_name);
        ANSITerminal.(print_string [white] " Evolve Upgrade!
        \nThe disease gets a bit stronger ... much unlike the chances of a snow day occurring at Cornell. Here are the rest of the available upgrades: \n" ); 
        print_evolve_list (get_curr_upgrades new_state);
        ANSITerminal.(print_string [white] "\nPlease enter a new command.\n");
        cursor (); repl dis new_state dis_type (read_line ()) d;
      else
        let _ = ANSITerminal.(print_string [red] 
                                "I'm sorry, you don't have enough points for ");
          ANSITerminal.(print_string [blue] upgrade_name);
          ANSITerminal.(print_string [white] "\nPlease enter a new command.\n"); 
        in cursor (); repl dis st dis_type (read_line ()) d;
    else let _ = ANSITerminal.(print_string [red] 
                                 "\nThis item isn't available.\n ");
           ANSITerminal.(print_string [white] 
                           "\nPlease enter a new command.\n");
      in cursor (); repl dis st dis_type (read_line ()) d
  | Score -> 
    ANSITerminal.(print_string[white] ("\n--> Current score:\n"));
    print_string ("    " ^ string_of_int(get_player_score st));
    print_endline ("");
    ANSITerminal.(print_string[white]) "\nPlease enter your command:\n";
    cursor (); repl dis st dis_type (read_line ()) d
  | Quit -> 
    print_endline "";
    ANSITerminal.(print_string [white] 
                    "\nYou have quit the game. Thanks for playing! :-)\n");
    print_endline "";
    Pervasives.exit 0
  | exception Malformed -> 
    print_endline "";
    ANSITerminal.(print_string [red] "This is an illegal move. ");
    ANSITerminal.(print_string[white]) "\nPlease re-enter your command:\n";
    cursor (); repl dis st dis_type (read_line ()) d
  | exception Empty -> print_endline ("");
    ANSITerminal.(print_string [red] "You did not enter anything. ");
    ANSITerminal.(print_string[white]) "\nPlease enter a new command:\n";
    cursor ();
    repl dis st dis_type (read_line ()) d
  | Next -> 
    ignore(Sys.command "clear");
    let new_st = step dis st dis_type 1 d in
    repl dis (fst new_st) dis_type (read_line ()) (snd new_st)
  | Commands -> 
    ANSITerminal.(print_string [white] "\nHere's a list of valid commands: { \n");
    ANSITerminal.(print_string [cyan] "evolve [upgrade name], next, forward [number of times], score, quit, upgrades, commands, get [state]");
    ANSITerminal.(print_string [white] " }");
    print_endline ("");
    ANSITerminal.(print_string[white]) "\nPlease enter your command:\n";
    cursor (); repl dis st dis_type (read_line ()) d
  | Forward n ->  
    try (
      let number_of_rounds = int_of_string((String.concat "" n)) in
      ignore(Sys.command "clear");
      if number_of_rounds < 1 then
        begin
          ANSITerminal.(print_string [red] "Please enter a valid number greater than or equal to 1.");
          cursor ();
          repl dis st dis_type (read_line ()) d;
        end
      else 
        let new_st = step dis st dis_type number_of_rounds d in
        repl dis (fst new_st) dis_type (read_line ()) (snd new_st)
    ) with Failure _ -> ANSITerminal.(print_string [red] "Please enter a valid number greater than or equal to 1.");
      cursor ();
      repl dis st dis_type (read_line ()) d

(** [play_game f] starts the disease game in file [f]. *)
let play_game disease_json =
  let d = initialize_disease disease_json [] in 
  let init_st = init_state 0 d disease_json None 
      (get_evolvenames (evolution_list disease_json)) (!purchased_values)  in
  print_string ("
    \x1b[38;5;160;48;5;15;1m
--------------------------------------------------------
        Great! We have now commenced the game!          
        Your commands are: \x1b[38;5;129m                             
{evolve [upgrade name], next, forward [number of times],
         score, quit, upgrades, commands, get [state] }\x1b[38;5;160;48;5;15;1m 
------------------------------------------------------- ");
  print_string ("\x1b[0m\n"); 

  ANSITerminal.(print_string [white]"\nPlease enter your command:\n"; cursor ());
  repl d init_st disease_json (read_line ()) ("Jul", 04, 1776)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ignore(Sys.command("sleep 0s"));
  ignore(Sys.command "clear");
  print_endline "";
  print_string ("\x1b[38;5;160;48;5;15;1m
----------------------------------------------------------------------------------
    Welcome, your goal is to infect everybody in the great                        
    United States of America. You will have won once the entire                   
    population is DEAD!                                                           
    Good luck.                                       
----------------------------------------------------------------------------------");
  print_string ("\x1b[0m");
  print_endline "";
  play_game (from_json (Yojson.Basic.from_file "maps/states.json"))

(* Execute the game engine. *)
let () = 
  ignore(Sys.command "clear");
  index_of_dir "load/ascii.txt" print_file;
  ignore(Sys.command("sleep 3s"));
  print_screen "load/load.txt" ".2s";
  main ()
