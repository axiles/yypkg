open Printf

exception Package_name_must_end_in_txz_tgz_or_tbz2

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

type cmd_line = {
  output : string;
  folder : string;
  pkg_name : string;
  version : version;
}

RE int = digit+
RE major_minor_release = 
  (int as maj : int) "." (int as min : int) "." (int as rel : int)

let version_of_string s =
  let /(major_minor_release "-" (".*" as s) "-" (int as iter : int))/ = s in
  let status = match s with
    | RE "alpha-" (int as x : int) -> Alpha x
    | RE "beta-" (int as x : int) -> Beta x
    | RE "rc-" (int as x : int) -> RC x
    | RE "stable" -> Stable
  in
  {
    major = maj;
    minor = min;
    release = rel;
    status = status;
    package_iteration = iter;
  }

let string_of_version v =
  let status = 
    match v.status with
      | Alpha x -> sprintf "alpha-%d" x
      | Beta x -> sprintf "beta-%d" x
      | RC x -> sprintf "rc-%d" x
      | Stable -> "stable"
  in
  sprintf "%d.%d.%d-%s-%d" v.major v.minor v.release status v.package_iteration

let tar, xz, gzip, bzip2 =
  match Sys.os_type with
    | "Unix"
    | "Cygwin" -> "tar", "xz -9", "gzip -9", "bzip2 -9"
    | "Win32" -> "tar.exe", "xz.exe -9", "gzip.exe -9", "bzip2.exe -9"
    | _ -> assert false

let parse_command_line () = 
  let output = ref "" in
  let folder = ref "" in
  let pkg_name = ref "" in
  let version = ref "" in
  let lst = [
    "-o", Arg.Set_string output, "set output file";
    "-name", Arg.Set_string pkg_name, "set the package name";
    "-version", Arg.Set_string version, "set the package version";
  ]
  in
  let usage_msg = "makeypkg -name foo -o /path/to/package.txz /folder/to/package" in
  let () = Arg.parse lst ((:=) folder) usage_msg in
  if "" = !output || "" = !folder || "" = !pkg_name || "" = !version then
    let () = prerr_endline "Error: incorrect usage of makeypkg" in
    let () = prerr_endline usage_msg in
    exit 0
  else
    {
      output = !output;
      folder = !folder;
      pkg_name = !pkg_name;
      version = version_of_string !version;
    }

let meta ~pkg_name ~pkg_size ~version =
  String.concat "\n" [
    "((credits \"me\")";
    sprintf "(package_name \"%s\")" pkg_name;
    sprintf "(package_size_expanded \"%s\")" pkg_size;
    "(package_type Other)";
    sprintf "(package_version %s)" (string_of_version version);
    "(packager_email \"a@a.com\")";
    "(packager_name \"ME\"))";
  ]

let package_script_el c ~pkg_size =
  let meta = meta ~pkg_name:c.pkg_name ~pkg_size ~version:c.version in
  let install = sprintf "(\"%s\" (Expand \"%s/*\" \"%s\"))" c.folder c.folder "." in
  let uninstall = sprintf "(Reverse \"%s\")" c.folder in
  sprintf "(\n%s\n(\n%s\n)\n(\n%s\n)\n)" meta install uninstall

let write_temp_file base_name contents =
  let path, oc = Filename.open_temp_file base_name "" in
  let () = output_string oc contents in
  let () = close_out oc in
  path

let compressor_of_ext s =
  (* this function may raise a bunch of exceptions which should be caught with a
   * "try compressor_of_ext with _ -> ...": no need to be more specific, it only
   * means the user gave a wrong filename *)
  let ext_of_filename s =
    let l = String.length s in
    let i = String.rindex s '.' in
    String.sub s (i+1) (l-i-1)
  in
  match ext_of_filename s with
    | "tgz" -> gzip
    | "txz" -> xz
    | "tbz2" -> bzip2
    | _ -> assert false

let () =
  let cmd_line = parse_command_line () in
  let compressor = try compressor_of_ext cmd_line.output with 
    | _ -> raise Package_name_must_end_in_txz_tgz_or_tbz2
  in
  let pkg_size = FileUtil.string_of_size (fst (FileUtil.StrUtil.du [ "." ])) in
  let package_script_el = package_script_el ~pkg_size cmd_line in
  let script_path = write_temp_file "package_script.el" package_script_el in
  let transform = sprintf "--transform=s#%s#package_script.el#" script_path in
  let command = sprintf
    "%s cv --absolute-names --exclude \"install\" %s %s %s | %s -9 > %s "
    tar script_path cmd_line.folder transform compressor cmd_line.output
  in
  print_endline command;
  ignore (Sys.command command)
