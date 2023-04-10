(* () = assert (String.equal (env! "FOLD_PPX_ENV_CHECK") "42") *)

let () = assert (String.equal [%env "FOLD_PPX_ENV_CHECK"] "42")
