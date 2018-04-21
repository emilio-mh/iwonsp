type nspi = {n : int; d : int; s : int; cvg : int array array; prf : int array array}
type seed = int array array

let copy (i : Nsp.nspi) : Nsp.nspi = 
  let nc = Array.copy i.cvg in
  let np = Array.copy i.prf in
  {n=i.n, d=i.d, s=i.s, cvg=nc, prf=np}

let readFile (fname : string) : nspi = 
  let f = open_in fname in
  let szs = Array.of_list (List.map int_of_string (String.split_on_char ' ' (read_line f))) in
  let ns = szs.(0) in (*Number of nurses*)
  let ds = szs.(1) in (*Number of days*)
  let ss = szs.(2) in (*Number of shifts*)
  read_line f;
  let coverage = Array.make_matrix d s 0 in (*Covrage matrix*)
  for i=0 to ds - 1 do
    let tmp = Array.of_list (List.map int_of_string (String.split_on_char ' ' (read_line f))) in
    for j=0 to ss - 1 do
      Array.set coverage.(i) j tmp.(j) 
    done
  done
  read_line f;
  let preference = Array.make_matrix ns ds (Array.make ss 0) in (*Preference matrix*)
  for i=0 to ns - 1 do
    let tmp = Array.of_list (List.map int_of_string (String.split_on_char ' ' (read_line f))) in
    for j=0 to ds - 1 do
      for k=0 to ss -1 do
        Array.set preference.(i).(j).(k) tmp.(j*ss + k)
      done
    done
  done;
  {n=ns,d=ds,s=ss,cvg=coverage,prf=preference}


