
let gen_seed (inst : Nsp.nspi) (n : Nsp.seed) : Nsp.seed =
  let ni = Nsp.copy n in
  let i = Random.int inst.n in
  let j = Random.int inst.d in
  Array.set ni.sch.(i) j ((n.sch.(i).(j) + 1) mod inst.s);
  ni

let rec gen_seed_k (inst : Nsp.nspi) (k : int) (n : Nsp.seed) : Nsp.seed =
  if k = 0 then {sch=n.sch; fit=Evaluation.cost inst n.sch} else gen_seed_k inst (k-1) (gen_seed inst n)

let reproduce (inst : Nsp.nspi) (seed : Nsp.seed) (n : int) (k : int) =
  Array.map (gen_seed_k inst k) (Array.make n seed)

let random_seed (inst : Nsp.nspi) : Nsp.seed =
  let m = Array.map (Array.map (Random.int)) (Array.make inst.n (Array.make inst.d inst.s))in
  {sch=m; fit=  Evaluation.cost inst m}  

let random_population (inst : Nsp.nspi) (n : int) : Nsp.seed array =
  let m = (Array.make n inst) in
  (Array.map random_seed m )
 
let sweep (inst : Nsp.nspi) (s : Nsp.seed) : Nsp.seed = 
  let ni = Nsp.copy s in
  let maxfit = ref s.fit in
  let maxseed = ref (Nsp.copy ni) in
  let improve = ref true in
  while !improve do
    improve := false;
    for n = 0 to inst.n - 1 do
      for d = 0 to inst.d - 1 do
        let tmp = ni.sch.(n).(d) in
        for s = 0 to inst.s - 1 do       
          Array.set ni.sch.(n) d s;
          let nf = Evaluation.cost inst ni.sch in
          if nf < !maxfit then
            (maxseed := Nsp.copy ni; maxfit := nf; improve := true)
          else ()
        done;
        Array.set ni.sch.(n) d tmp
      done
    done;
  done;
  {sch=(!maxseed).sch;fit=Evaluation.cost inst (!maxseed).sch}
