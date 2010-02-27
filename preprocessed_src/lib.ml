open Printf
  
open Types
  
let __mikmatch_regexp_1 =
  Str.regexp
    "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)-\\([]\000-,.-\\_-\255^-]+\\)-\\([0-9]+\\)"
  
let __mikmatch_regexp_2 = Str.regexp "alpha-\\([0-9]+\\)"
  
let __mikmatch_regexp_3 = Str.regexp "beta-\\([0-9]+\\)"
  
let __mikmatch_regexp_4 = Str.regexp "rc-\\([0-9]+\\)"
  
let __mikmatch_regexp_5 = Str.regexp "stable"
  
let version_of_string s =
  let __mikmatch_match_target_2 = s
  in
    (try
       let __mikmatch_regexp_1_target = __mikmatch_match_target_2
       in
         if Str.string_match __mikmatch_regexp_1 __mikmatch_regexp_1_target 0
         then
           (let s = Str.matched_group 4 __mikmatch_regexp_1_target in
            let rel =
              Pervasives.int_of_string
                (Str.matched_group 3 __mikmatch_regexp_1_target) in
            let min =
              Pervasives.int_of_string
                (Str.matched_group 2 __mikmatch_regexp_1_target) in
            let maj =
              Pervasives.int_of_string
                (Str.matched_group 1 __mikmatch_regexp_1_target) in
            let iter =
              Pervasives.int_of_string
                (Str.matched_group 5 __mikmatch_regexp_1_target)
            in
              fun () ->
                let status =
                  let __mikmatch_match_target_1 = s
                  in
                    (try
                       let __mikmatch_regexp_2_target =
                         __mikmatch_match_target_1
                       in
                         if
                           Str.string_match __mikmatch_regexp_2
                             __mikmatch_regexp_2_target 0
                         then
                           (let x =
                              Pervasives.int_of_string
                                (Str.matched_group 1
                                   __mikmatch_regexp_2_target)
                            in fun () -> Alpha x)
                         else raise Run_mikmatch_str.Mikmatch_exit
                     with
                     | Run_mikmatch_str.Mikmatch_exit ->
                         (try
                            let __mikmatch_regexp_3_target =
                              __mikmatch_match_target_1
                            in
                              if
                                Str.string_match __mikmatch_regexp_3
                                  __mikmatch_regexp_3_target 0
                              then
                                (let x =
                                   Pervasives.int_of_string
                                     (Str.matched_group 1
                                        __mikmatch_regexp_3_target)
                                 in fun () -> Beta x)
                              else raise Run_mikmatch_str.Mikmatch_exit
                          with
                          | Run_mikmatch_str.Mikmatch_exit ->
                              (try
                                 let __mikmatch_regexp_4_target =
                                   __mikmatch_match_target_1
                                 in
                                   if
                                     Str.string_match __mikmatch_regexp_4
                                       __mikmatch_regexp_4_target 0
                                   then
                                     (let x =
                                        Pervasives.int_of_string
                                          (Str.matched_group 1
                                             __mikmatch_regexp_4_target)
                                      in fun () -> RC x)
                                   else raise Run_mikmatch_str.Mikmatch_exit
                               with
                               | Run_mikmatch_str.Mikmatch_exit ->
                                   (try
                                      let __mikmatch_regexp_5_target 
                                        = __mikmatch_match_target_1
                                      in
                                        if
                                          Str.string_match
                                            __mikmatch_regexp_5
                                            __mikmatch_regexp_5_target 0
                                        then (fun () -> Stable)
                                        else
                                          raise Run_mikmatch_str.
                                            Mikmatch_exit
                                    with
                                    | Run_mikmatch_str.Mikmatch_exit ->
                                        (fun () ->
                                           match () with _ ->
                                                          raise
                                                            (Match_failure
                                                               ("src/lib.ml",
                                                               10, 15)))))))
                      ()
                in
                  {
                    major = maj;
                    minor = min;
                    release = rel;
                    status = status;
                    package_iteration = iter;
                  })
         else raise Run_mikmatch_str.Mikmatch_exit
     with
     | Run_mikmatch_str.Mikmatch_exit ->
         (fun () ->
            match () with _ -> raise (Match_failure ("src/lib.ml", 9, 2))))
      ()
  
let string_of_version v =
  let status =
    match v.status with
    | Alpha x -> sprintf "alpha-%d" x
    | Beta x -> sprintf "beta-%d" x
    | RC x -> sprintf "rc-%d" x
    | Stable -> "stable"
  in
    sprintf "%d.%d.%d-%s-%d" v.major v.minor v.release status
      v.package_iteration
  
let dir_sep =
  match Sys.os_type with
  | "Unix" | "Cygwin" -> "/"
  | "Win32" -> "\\"
  | _ -> assert false
  

