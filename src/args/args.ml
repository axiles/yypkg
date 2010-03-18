(* gallium's Arg module is quite frustrating: it's simple but also quite limited
 * This module tries to provide a nicer higher-level interface for argument
 * parsing
 * Among others, we're making it easy to have mandatory arguments and mandatory
 * and forbidden combinations of arguments.
 *)

type 'a data = 
  | String
  | Nothing
  | Subargs of ('a list)

type arg = {
  name : string;
  t : arg data;
  help_text : string;
}

(* planned API:
  * let w = {
  *   name = "-prefix";
  *   t = String;
  *   help_text = "installation prefix"; }
  * in
  * let x = {
  *   name = "-install";
  *   t = String;
  *   help_text = "installs the package"; }
  * in
  * let y = {
  *   name = "-uninstall";
  *   t = String;
  *   help_text = "uninstalls the package"; }
  * in
  * let z = {
  *   name = "-list";
  *   t = Nothing;
  *   help_text = "lists the installed packages"; }
  * in
  * let o =
  *   let a = {
  *     name = "-pred";
  *     t = String;
  *     help_text = "list of predicates to check when installing a package" }
  *   in
  *   let b = {
  *     name = "-regen";
  *     t = Nothing;
  *     help_text = "regenerates the database cache"; }
  *   in
  *   { name = "-configure";
  *   t = Subargs [ a; b ] ;
  *   help_text = "configures yypkg" (* will say which args are mandatory *) }
  * in
  * let l = [ w; x; y; z; o ] in
  * match parse_args Sys.argv l with
  *   | [ "-reconfigure", None, args ] -> ()
  *   | [ "-list", None, [] ] -> ()
  *   | l -> 
    *   assert (not (List.mem_assoc "-reconfigure"));
    *   assert (not (List.mem_assoc "-list"));
    *   ()
  *)
