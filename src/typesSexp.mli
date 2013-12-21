module Of :
  sig
    val metadata : Types.metadata -> Pre_sexp.t
    val conf : Types.conf -> Pre_sexp.t
    val db : Types.db -> Pre_sexp.t
    val sherpa_conf : Types.SherpaT.sherpa_conf -> Pre_sexp.t
    val script : Types.script -> Pre_sexp.t
    val repo : Types.SherpaT.repo -> Pre_sexp.t
  end
module To :
  sig
    val script : Pre_sexp.t -> Types.script
    val conf : Pre_sexp.t -> Types.conf
    val db : Pre_sexp.t -> Types.db
    val metadata : Pre_sexp.t -> Types.metadata
    val sherpa_conf : Pre_sexp.t -> Types.SherpaT.sherpa_conf
    val repo : Pre_sexp.t -> Types.SherpaT.repo
  end
