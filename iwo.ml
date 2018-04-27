type params = {maxit : int; (*Number of iterations*)
               dist_i : int; (*Initial distance of dispersion*)
               dist_f : int; (*Final distance of dispersion*)
               maxpop : int;
               rf : int} (*Number of seeds produced by each plant*)

let iwo inst (par : params) =
  let reproduce = Reproduction.reproduce inst in
  let pop = Reproduction.random_population inst par.maxpop in
  let newpop = Array.copy pop in
  let k = ref (par.dist_i) in
  for i = 0 to par.maxit do
    for j=0 to (Array.length pop) - 1 do
      let p1 = reproduce pop.(j) par.rf !k in
      Array.sort (Evaluation.compare inst) p1;
      Array.blit (Array.of_list (List.merge (Evaluation.compare inst) (Array.to_list newpop) (Array.to_list (p1)))) 0 newpop 0 par.maxpop
    done;
    Array.blit newpop 0 pop 0 par.maxpop
  done