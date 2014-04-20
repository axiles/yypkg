module Of :
  sig
    val metadata : Types.metadata -> Pre_sexp.t
    val conf : Types.conf -> Pre_sexp.t
    val db : Types.db -> Pre_sexp.t
    val script : Types.script -> Pre_sexp.t
    val repository : Types.Repo.t -> Pre_sexp.t
  end
module To :
  sig
    val script : Pre_sexp.t -> Types.script
    val conf : Pre_sexp.t -> Types.conf
    val db : Pre_sexp.t -> Types.db
    val metadata : Pre_sexp.t -> Types.metadata
    val repository : Pre_sexp.t -> Types.Repo.t
  end
