let force_opt opt =>
  switch opt {
  | Some x => x
  | _ => assert false
  };

let compose f g a => f (g a);

let const a b => a;
