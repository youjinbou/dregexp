open Ocamlbuild_plugin;;
open Command;;


dispatch begin function
| After_rules -> 

  ocaml_lib ~dir:"+oUnit" ~extern:true "oUnit"

(*
    (* add ulex preprocessor for use_ulex tag 
    flag ["ocaml";"pp";"use_ulex"] (S[A "camlp4o";A "-I";A "/usr/lib/ocaml/ulex";A "pa_ulex.cma"]);
    flag ["ocaml";"use_ulex"] (S[A "-I";A "/usr/lib/ocaml/ulex"]);
    *)
    flag ["ocaml";"compile";"native";"inline"] (S [A "-inline"; A "26"]);
    flag ["ocaml";"compile";"native";"unsafe"] (S [A "-unsafe"]);
    flag ["ocaml";"compile";"native";"asm"] (S [A "-S"]);
*) 

| _ -> ()
end;;
