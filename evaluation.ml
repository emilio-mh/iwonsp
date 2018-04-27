
let cost (inst : Nsp.nspi) (seed : Nsp.seed) : float =
  if !(seed.fit) != 0.0 then
    !(seed.fit)
  else
  let f = ref 1.0 in
  let t = Array.make inst.s 0 in
    for j = 0 to inst.d - 1 do
      for i = 0 to inst.n - 1 do
        let tmp = seed.sch.(i).(j) in
        f := !f *. (float_of_int (inst.prf.(i).(j).(tmp)));
        Array.set t tmp (t.(tmp) + 1)
      done;
      for k=0 to inst.s - 1 do
        f := !f *. (float_of_int (t.(k) - 1 + inst.cvg.(j).(k)))
      done;
      Array.fill t 0 inst.s 0
    done; seed.fit :=  !f; !f

let compare (inst : Nsp.nspi) (s1 : Nsp.seed) (s2 : Nsp.seed) : int =
  (if !(s1.fit) = 0.0 then ignore (cost inst s1));
  (if !(s2.fit) = 0.0 then ignore (cost inst s2));
  if !(s1.fit) < !(s2.fit) then -1 else 1