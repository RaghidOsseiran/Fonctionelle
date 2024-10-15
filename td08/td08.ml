type subject = Selfie | Monument | Miroir_d_Eau | Mode | People | Mon_assiette_au_resto
type photo = Photo of int * (subject list)
type album = photo list


let photo_annee (photo: photo) = 
  let Photo(annee, _) = photo in annee;;


let photo_subjects (photo: photo) =
  let Photo(_, photo_list) = photo in photo_list;; 



let has_subject (subject: subject) (photo: photo) : bool = 
  let photo_list = photo_subjects photo in 
  let rec aux_has_subject subject l = 
    match l with
      | [] -> false
      | h :: tail -> if (h == subject) then true else (false || aux_has_subject subject tail) 
  in aux_has_subject subject photo_list;;

let reverse_list l = 
  let rec aux_rev l acc = 
    match l with
    | [] -> acc 
    | h :: tail -> aux_rev tail (h::acc)
  in aux_rev l [];;



let select_by_subject (subject: subject) (album: album) : album = 
  let rec aux_select_by_subject subject album acc = 
    match album with
    | [] -> acc 
    | Photo(_, _) as curr_photo :: tail -> 
        if (has_subject subject curr_photo) then aux_select_by_subject subject tail (curr_photo::acc)
        else aux_select_by_subject subject tail acc
  in reverse_list(aux_select_by_subject subject album []);;


let select_by_date p a = 
  let rec aux_select_by_date p a acc = 
    match a with 
    | [] -> acc 
    | Photo(year, _) as curr_photo :: tail -> 
        if (p year) then aux_select_by_date p tail (curr_photo :: acc) else aux_select_by_date p tail acc 
  in reverse_list(aux_select_by_date p a []);; 


type criteria =
    Subject of subject
  | Date of (int -> bool)
  | Or of criteria * criteria
  | And of criteria * criteria
  | Not of criteria


let res = And(Subject(Selfie), And(Not(Subject(People)), Date(fun x -> x >= 2014)));;



let rec satisfies (criteria : criteria) (photo : photo) = 
  match criteria with 
  | Subject(subj) -> has_subject subj photo
  | Date(f) -> let Photo(date, _) = photo in f date
  | Or(c1, c2) -> (satisfies c1 photo || satisfies c2 photo)
  | And(c1, c2) -> (satisfies c1 photo && satisfies c2 photo)
  | Not(c1) -> not(satisfies c1 photo);;


let select (criteria : criteria) (album : album) : album = 
  let rec select_aux criteria album acc = 
    match album with 
    | [] -> acc
    | Photo(_, _) as curr_photo :: tail -> 
      if satisfies criteria curr_photo 
        then select_aux criteria tail (curr_photo :: acc)
        else select_aux criteria tail acc
  in select_aux criteria album [];;
    


let filter_list l =
  let rec aux_filter l acc = 
    match l with
      | [] -> reverse_list(acc) 
      | h :: tail -> let new_list = List.filter(fun x -> x <> h) l in aux_filter new_list (h::acc) 
  in aux_filter l [];;


let couple_elements (cp_list: ('a * 'a) list) :'a list = 
  let rec aux_couple (cp_list: ('a * 'a)list) acc : 'a list = 
    match cp_list with 
    | [] -> acc 
    | (v1, v2) :: tail -> aux_couple tail (v1::v2::acc)
  in filter_list(aux_couple cp_list []);; 
