type params = {maxit : int; (*Number of iterations*)
               dist_i : int; (*Initial distance of dispersion*)
               dist_f : int; (*Final distance of dispersion*)
               rf : int} (*Number of seeds produced by each plant*)

let iwo inst (par : params) (it : int) (maxp : int) =
  let pop = Reproduction.random_population inst maxp in
  for i = 0 to par.maxit do
    
  done