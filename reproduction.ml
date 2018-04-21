

let gen_seed (n : Nsp.seed) (shifts : int) (ctr : int) : Nsp.seed =
  let ni = Nsp.copy n in
  let i = Random.int ni.n in
  let j = Random.int ni.d in
  Array.set ni.(i) j (((Random.int shifts) * ctr + 1) mod shifts);
  ni in

let reproduce (shifts : int) (k : int) (parent : Nsp.seed) : Nsp.seed list =
  List.init k (gen_seed parent shifts) in

let random_population (inst : Nsp.nspi) (n : int) : Nsp.seed list =
