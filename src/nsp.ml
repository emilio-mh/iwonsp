type nspi = {n : int; d : int; s : int; cvg : int array array; prf : int array array; maxfit : float}
type seed = {sch : int array array; fit : float}

let re = Str.regexp "[\r\n]" 

let copy (s : seed) : seed = 
  {sch=(Array.copy s.sch); fit= 0.0}

let readFile (fname : string) : nspi = 

  let f = open_in fname in
  let line = Str.global_replace re "" (input_line f) in 
  
  let szs = Array.of_list (List.map int_of_string (String.split_on_char '\t' line)) in
  let ns = szs.(0) in (*Number of nurses*)
  let ds = szs.(1) in (*Number of days*)
  let ss = szs.(2) in (*Number of shifts*)
  ignore (input_line f);
  let coverage = Array.make_matrix ds ss 0 in (*Covrage matrix*)
  let ack = ref 0.1 in
  for i=0 to ds - 1 do
    let line = Str.global_replace re "" (input_line f) in 
    let tmp = Array.of_list (List.map int_of_string (String.split_on_char '\t' line)) in
    for j=0 to ss - 1 do
      Array.set coverage.(i) j tmp.(j);
      ack := !ack *. (float_of_int (coverage.(i).(j) + 1) ** 2.0)
    done
  done;
  ignore (input_line f);
  let preference = Array.make ns (Array.make (ds*ss) 0) in (*Preference matrix*)
  for i=0 to ns - 1 do
    let lin = Str.global_replace re "" (input_line f) in 
    let line = String.sub lin 0 ((String.length lin) - 1) in
    let tmp = Array.of_list (List.map int_of_string (String.split_on_char '\t' line)) in
    Array.blit tmp 0 (preference.(i)) 0 (ds*ss);
    
  done;

  let mf = (4.0 *. float_of_int (ns*ds)) *. !ack in
  {n=ns;d=ds;s=ss;cvg=coverage;prf=preference;maxfit=mf}

let printRes out seed =
  let n = (Array.length seed.sch) in
  let m = (Array.length seed.sch.(0)) in
  for i = 0 to n - 1 do
    for j = 0  to m - 2 do
      Printf.fprintf out "%d, " seed.sch.(i).(j)
    done;
    Printf.fprintf out "%d\n" seed.sch.(i).(m-1)
  done;
  Printf.fprintf out "\n%f\n" (seed.fit)