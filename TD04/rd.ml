type mycomplex = C of float * float

let make_complex x y = C(x,y)
let realpart c = 
  match c with 
  | C(x, _) -> x

let imagpart c = 
  match c with 
  | C(_, y) -> y 

let c_origin = make_complex 0. 0. 
let c_i = make_complex 0. 1. 
let p_12 = make_complex 1. 2. 

let c_sum c1 c2 =
  let r1, r2 = realpart c1, realpart c2 in 
  let i1, i2 = imagpart c1, imagpart c2 in 
  make_complex (r1+.r2)(i1+.i2)

let c_diff c1 c2 = make_complex (realpart c1 -. realpart c2) (imagpart c1 -. imagpart c2)
let c_opp c = make_complex (-.realpart c)(-.imagpart c)
let c_mul c1 c2 = 
  let r1, i1 = realpart c1, imagpart c1 in
  let r2, i2 = realpart c2, imagpart c2 in
  make_complex (r1 *. r2 -. i1 *. i2) (r1 *. i2 +. i1 *. r2)
let c_abs c = 
  sqrt ((realpart c) ** 2. +. (imagpart c) ** 2.) 
let c_sca lambda c = make_complex (lambda *. realpart c)(lambda *. imagpart c)
let c_exp c = 
  let r, i = realpart c, imagpart c in
  let exp_r = exp r in
  make_complex (exp_r *. cos i) (exp_r *. sin i)

let translate c (vector : mycomplex) = c_sum c vector 

let rotate0 c angle =
  let cos_theta = cos angle in
  let sin_theta = sin angle in
  let rotation_factor = make_complex cos_theta sin_theta in
  c_mul c rotation_factor


let rotate c angle center = 
  let translated_c = c_diff c center in 
  let rotated_c = rotate0 translated_c angle in 
  c_sum rotated_c center


  let test_mycomplex () =
    begin
  (* realpart *)
    assert (let c = make_complex 3. 4. in realpart c = 3.0);
  
  (* imagpart *)
    assert (let c = make_complex 3. 4. in imagpart c = 4.0);
  
  (* c_abs *)
  assert (let i = make_complex 0. 1. in c_abs i = 1.);
  
      assert (let c = make_complex 3. 4. in c_abs c = 5.);
  (* c_sum *)
  assert (let o = make_complex 0. 0. and i = make_complex 0. 1. in c_sum o i = i);
  (* c_sca *)
  assert (let c = make_complex 3. 4. in c_sum c c = c_sca 2. c);
  assert (let c = make_complex 3. 4. in c_sca 2. c = C(6., 8.));
  (* c_dif *)
  assert (let c = make_complex 3. 4. and o = make_complex 0. 0. in c_diff c c = o);
  
  (* c_mul *)
  assert (let c = make_complex 3. 4. in c_mul c c = C(-7., 24.));
  (* c_opp *)
  assert (let c = make_complex 3. 4. in c_opp c = C(-3., -4.));
  
  (* c_exp *)
  assert (let c = make_complex 3. 4. in c_exp c = C(-13.1287830814621582, -15.2007844630679543));
  
  (* translate *)
  assert (let c = make_complex 3. 4. and v = make_complex 2. 3. in translate c v = C(5., 7.));
  
  (* rotate0 *)
  assert (let c = make_complex 3. 4. and angle = 3.14159265358979312 /. 4. in rotate0 c angle =  C (-0.707106781186547, 4.94974746830583268));
  
  (* rotate *)
  assert (let c = C(3., 4.) and angle = 3.14159265358979312 /. 4. and center = C(1.,2.) in rotate c angle center =  C (1.00000000000000022, 4.82842712474619));
  end
  
  (* pour tester vos fonctions *)
  (* let _ = test_mycomplex () *)
  