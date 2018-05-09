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
  Array.sort (Evaluation.compare inst) newpop;
  let k = ref (par.dist_i) in
  for r = 0 to par.reps - 1 do
    for i = 0 to par.maxit do
      Printf.printf "%d / %d  \r" (i + par.maxit*r) (par.maxit*par.reps);
      flush stdout;
      for j=0 to (Array.length pop) - 1 do
        let p1 = reproduce pop.(j) par.rf !k in
        Array.sort (Evaluation.compare inst) p1;
        Array.blit (Array.of_list (List.merge (Evaluation.compare inst) (Array.to_list newpop) (Array.to_list (p1)))) 0 newpop 0 par.maxpop
      done;
      k := ((par.dist_f - par.dist_i) * i / (par.maxit)) + par.dist_i;
      Array.blit newpop 0 pop 0 par.maxpop
      prerr_float pop.(0).fit;
      prerr_newline ();
    done
  done; pop