let force_opt opt =>
  switch opt {
  | Some x => x
  | _ => assert false
  };

let compose f g a => f (g a);

let const a b => a;

let do_and_return f v x => {
  f x;
  v
};

let doUnit f => do_and_return f ();

let option_map f opt =>
  switch opt {
  | None => None
  | Some v => Some (f v)
  };
