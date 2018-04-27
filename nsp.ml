type nspi = {n : int; d : int; s : int; cvg : int array array; prf : int array array array; maxfit : float}
type seed = {sch : int array array; fit : float ref}

let copy (s : seed) : seed = 
  {sch=(Array.copy s.sch); fit= ref 0.0}

let readFile (fname : string) : nspi = 
  let f = open_in fname in
  let szs = Array.of_list (List.map int_of_string (String.split_on_char ' ' (input_line f))) in
  let ns = szs.(0) in (*Number of nurses*)
  let ds = szs.(1) in (*Number of days*)
  let ss = szs.(2) in (*Number of shifts*)
  ignore (input_line f);
  let coverage = Array.make_matrix ds ss 0 in (*Covrage matrix*)
  let ack = ref 1.0 in
  for i=0 to ds - 1 do
    let tmp = Array.of_list (List.map int_of_string (String.split_on_char ' ' (input_line f))) in
    for j=0 to ss - 1 do
      Array.set coverage.(i) j tmp.(j);
      ack := !ack *. (float_of_int (coverage.(i).(j) + 1) ** 2.0)
    done
  done;
  ignore (input_line f);
  let preference = Array.make_matrix ns ds (Array.make ss 0) in (*Preference matrix*)
  for i=0 to ns - 1 do
    let tmp = Array.of_list (List.map int_of_string (String.split_on_char ' ' (input_line f))) in
    for j=0 to ds - 1 do
      for k=0 to ss -1 do
        Array.set preference.(i).(j) k tmp.(j*ss + k);
      done
    done
  done;
  let mf = float_of_int (4*ns*ds) *. !ack in
  {n=ns;d=ds;s=ss;cvg=coverage;prf=preference;maxfit=mf}
