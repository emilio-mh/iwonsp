
let gen_seed (inst : Nsp.nspi) (n : Nsp.seed) : Nsp.seed =
  let ni = Nsp.copy n in
  let i = Random.int inst.n in
  let j = Random.int inst.d in
  Array.set ni.sch.(i) j ((Random.int inst.s));
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
 
