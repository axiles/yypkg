module Button : sig
  val okOnly : string
  val okCancel : string
  val abortRetryIgnore : string
  val yesNoCancel : string
  val yesNo : string
  val retryCancel : string
  val critical : string
  val question : string
  val exclamation : string
  val information : string
  val defaultButton1 : string
  val defaultButton2 : string
  val defaultButton3 : string
  val defaultButton4 : string
  val msgBoxHelpButton : string
end

val main : unit -> unit
val msgbox : ?title:string -> buttons:string list -> string list -> int
