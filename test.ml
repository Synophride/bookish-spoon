
let tests_failed= ref 0;;

let global_num_of_tests = ref 0;;

let assert_equals a b loc =
  global_num_of_tests := !global_num_of_tests +1;
  if not (a = b) then
    (
      Printf.printf "Test failed at location %s \n" loc;
      tests_failed := !tests_failed + 1 
    )
;;
      
let assert_true a loc =
  global_num_of_tests := !global_num_of_tests +1;
  if not (a) then
    (
      Printf.printf "Test failed at location %s \n" loc;
      tests_failed := !tests_failed + 1 
    )
;;
let assert_false a loc =
  global_num_of_tests := !global_num_of_tests +1;
  if (a) then
    (
      Printf.printf "Test failed at location %s \n" loc;
      tests_failed := !tests_failed + 1 
    )
;;
