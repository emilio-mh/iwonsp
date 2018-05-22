
let cost (inst : Nsp.nspi) (sch : int array array) : float =
  
  let f = ref 0.1 in
  for i = 0 to inst.n - 1 do
    for j = 0 to inst.d - 1 do
      f := !f +. float_of_int (inst.prf.(i).(j*inst.s + sch.(i).(j)))
    done
  done;
  for d = 0 to inst.d - 1 do
    for s = 0 to inst.s - 1 do
      let num = ref 0 in
      for n = 0 to inst.n - 1 do
        if sch.(n).(d) = s then num := !num + 1 else ()
      done;
      f := !f *. (float_of_int (max 1   (inst.cvg.(d).(s) - !num) ) )
    done
  done;  
  !f 

let compare (inst : Nsp.nspi) (s1 : Nsp.seed) (s2 : Nsp.seed) : int =
  if (s1.fit) < (s2.fit) then -1 else 1