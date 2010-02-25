(* this has to be kept ordered !!! *)
type status = 
  | Alpha of int
  | Beta of int
  | RC of int
  | Stable

type version = {
  major : int;
  minor : int;
  release : int;
  status : status;
  package_iteration : int;
}

