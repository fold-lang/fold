let x = 1;
let x = string_of_int(x);

let x = {
  let a = 1;
  let b = a + 2;
  Ok(a, b);
};

type person = {
  name: string,
  age: int,
  profession: option(string),
};

module Person = {
  let print = p => print_endline(p.name ++ " " ++ string_of_int(p.age));
};

let p = {name: "Anonymous", age: 0, profession: None};

let f = (~a, ~opt=0, ~use=?, ~name, x) => {
  ignore((a, opt, use, name, x));
  opt;
};

let x = {
  let rec x = 2
  and y = (3, 'a');
  let age = 30 - fst(y);
  let person = {...p, name: "Xavier", age};
  Person.print(person);
  let result = {
    let (a, opt, foo) = (
      1,
      None,
      Some(
        if (age > 30) {
          0;
        } else {
          10;
        },
      ),
    );
    f(~a, ~opt?, ~use=?foo, ~name="hello", x);
  };

  let rec map: ('a => 'b, list('a)) => list('b) =
    (f, l) =>
      switch (l) {
      | [] => []
      | [x, ...xs] => [f(x), ...map(f, xs)]
      };

  x
  + result
  - {
    let z = p.age;
    z * 2;
  };
};
