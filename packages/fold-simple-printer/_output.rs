
let rec x = 2, y = {3, 'a'}

let person = { ...p,
  name = "Xavier", age = 23
}

let result =
  call_function
    opt=?opt,
    (hello lang=en, "World"),
    name="hello",
    x


print 2 + 2, output=stdout

x + y - {
  let z = 1;
  z * 2
}





[let rec] (x = 2, y = (3, 'a')) {
  [let] (person = { [... p] (name = "Xavier", age = 23) }) {
    print_endline "hey";
    [let] (result = f [?opt] ~name:"hello" x) {
      (- (+ x y) ([let] (z = 1) {
        print_endline "hello";
        * z 2
      }))
    }
  }
}



let rec x = 2 and y = (3, 'a') in
let person = { p with name = "Xavier"; age = 23; } in
let result = f ?opt ~name:"hello" x in
x + y - (let z = 1 in z * 2)


(let rec ((x 2) (y (3, 'a')))
  (let (person { p with name = "Xavier"; age = 23; })
    let (result (f ?opt ~name:"hello" x))
      (- (+ x y) (let (z 1) ( * z 2)))))

[let rec ((x 2) (y (3, 'a')))
  (let (person { p with name = "Xavier"; age = 23; })
    let (result (f ?opt ~name:"hello" x))
      (- (+ x y) (let (z 1) ( * z 2))))]

[let rec ((x 1) (y 2))
  (let (x2 3)
    let (x4 4)
      (- 5 (let (z 1) ( * z 2))))]


let rec [x = 2, y = (3, 'a')] in
let person = { p with name = "Xavier"; age = 23; } in
let result = f ?opt ~name:"hello" x in
x + y - (let z = 1 in z * 2)
    



(form "lec" (rec) (+ 1 2))

[lec (rec) (+ 1 2)]

let[rec] (+ 1 2)
let (+ 1 2)


{
  let rec x = 2, y = (3, 'a');
  let person = { ...p, name = "Xavier", age = 23 };
  print_endline "hey";
  let result = f ?opt ~name:"hello" x;
  x + y - {
    let z = 1;
    z * 2
  }
}