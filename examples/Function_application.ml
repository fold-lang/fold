let a01 = func a b c
let a02 = func ~a ~b c

let a03 =
  func this_is_a_very_long_parameter_name
    this_is_another_very_long_parameter_name shorter_one a b

let a03 =
  func
    (fun a b ->
      begin
        print_endline "hello";
        a + b
      end
    )
    this_is_another_very_long_parameter_name shorter_one a b

let rec loop () =
  loop
    (print ~output:stdout
       (eval ~strict:true config (read ~input:stdin ~strict:true context))
    )

let a04 = f a b (2 + 2) (g x y (f x - 1))
let a05 = f (g (h x))
let a06 = f a (g b (h c))
