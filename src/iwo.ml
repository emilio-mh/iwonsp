type params = {maxit : int; (*Number of iterations*)
               dist_i : int; (*Initial distance of dispersion*)
               dist_f : int; (*Final distance of dispersion*)
               maxpop : int;
               reps : int;
               rf : int} (*Number of seeds produced by each plant*)

let iwo inst (par : params) =
  let reproduce = Reproduction.reproduce inst in
  let pop = Reproduction.random_population inst par.maxpop in
  let newpop = Array.copy pop in
  let mx = ref Pervasives.max_float in
  Array.sort (Evaluation.compare inst) newpop;
  let k = ref (par.dist_i) in
  for r = 0 to par.reps - 1 do
    for i = 0 to par.maxit do
      for j=0 to (Array.length pop) - 1 do
        let p1 = reproduce pop.(j) par.rf !k in
        Array.sort (Evaluation.compare inst) p1;
        let rr = (Array.of_list (List.merge (Evaluation.compare inst) (Array.to_list newpop) (Array.to_list (p1)))) in
        for u=0 to par.maxpop - 1 do
          Array.set newpop u rr.(u)
        done
        
      done;
      k := ((par.dist_f - par.dist_i) * i / (par.maxit)) + par.dist_i;
      for u=0 to par.maxpop - 1 do
        Array.set pop u (newpop.(u))
      done;
      if pop.(0).fit < !mx then 
      (Printf.printf "%d, %f\n" !k pop.(0).fit;
      mx := pop.(0).fit;
      flush stdout);
    done
  done; pop