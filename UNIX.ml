module Filename =
struct
  type t = string

  let parse s =
    match s with
      | "" | "." | ".." ->
          None
      | _ ->
          let len = String.length s in
          let rec loop i =
            if i >= len then
              Some s
            else
              match s.[i] with
                | '/' | '\000' -> None
                | _ -> loop (i + 1)
          in
          loop 0

  let parse_exn s =
    match parse s with
      | None ->
          invalid_arg ("parse_filename_exn: invalid file name: \"" ^ String.escaped s ^ "\"")
      | Some filename ->
          filename

  let show f = f

  let compare = String.compare
end

type absolute = Absolute
type relative = Relative

type file = File
type dir = Dir

type (_, _) t =
  | Root: (absolute, dir) t
  | Current: (relative, dir) t
  | Parent: ('a, dir) t -> ('a, dir) t
  | Dir: Filename.t * ('a, dir) t -> ('a, dir) t
  | File: Filename.t * ('a, dir) t -> ('a, file) t

type absolute_dir = (absolute, dir) t
type absolute_file = (absolute, file) t
type relative_dir = (relative, dir) t
type relative_file = (relative, file) t

type any =
  | AD of (absolute, dir) t
  | AF of (absolute, file) t
  | RD of (relative, dir) t
  | RF of (relative, file) t

let pack path =
  let pack_absolute (type a) (path: (absolute, a) t) =
    match path with
      | Root -> AD path
      | Parent _ -> AD path
      | Dir _ -> AD path
      | File _ -> AF path
  in
  let pack_relative (type a) (path: (relative, a) t) =
    match path with
      | Current -> RD path
      | Parent _ -> RD path
      | Dir _ -> RD path
      | File _ -> RF path
  in
  let rec pack: 'a 'b 'c. ('a, 'b) t -> ('a, 'c) t -> _ =
    fun (type a) (type b) (type c) (witness: (a, b) t) (path: (a, c) t) ->
      match witness with
        | Root -> pack_absolute path
        | Current -> pack_relative path
        | Parent parent
        | Dir (_, parent)
        | File (_, parent) -> pack parent path
  in
  pack path path

type 'a any_relativity =
  | A of (absolute, 'a) t
  | R of (relative, 'a) t

let pack_relativity path =
  let rec pack: 'a 'b 'c. ('a, 'b) t -> ('a, 'c) t -> 'c any_relativity =
    fun (type a) (type b) (type c) (witness: (a, b) t) (path: (a, c) t) ->
      match witness with
        | Root -> A path
        | Current -> R path
        | Parent parent
        | Dir (_, parent)
        | File (_, parent) -> pack parent path
  in
  pack path path

type 'a any_kind =
  | D of ('a, dir) t
  | F of ('a, file) t

let pack_kind (type a) (type b) (path: (a, b) t) =
  match path with
    | Root -> D path
    | Current -> D path
    | Parent _ -> D path
    | Dir _ -> D path
    | File _ -> F path

let parse str =
  let len = String.length str in
  if len <= 0 then
    None
  else
    let rec continue: 'a. ('a, dir) t -> (('a, dir) t -> any) -> (('a, file) t -> any) ->
      _ -> _ -> _ =
      fun (type a) (parent: (a, dir) t) (make_dir: (a, dir) t -> any)
        (make_file: (a, file) t -> any) start i ->
        if i >= len then
          match String.sub str start (i - start) with
            | "" | "." ->
                Some (make_dir parent)
            | ".." ->
                Some (make_dir (Parent parent))
            | filename ->
                Some (make_file (File (filename, parent)))
        else
          match str.[i] with
            | '\000' ->
                None
            | '/' ->
                (
                  match String.sub str start (i - start) with
                    | "" | "." ->
                        continue parent make_dir make_file (i + 1) (i + 1)
                    | ".." ->
                        continue (Parent parent) make_dir make_file (i + 1) (i + 1)
                    | filename ->
                        continue (Dir (filename, parent)) make_dir make_file (i + 1) (i + 1)
                )
            | _ ->
                continue parent make_dir make_file start (i + 1)
    in
    match str.[0] with
      | '/' ->
          continue Root (fun x -> AD x) (fun x -> AF x) 1 1
      | _ ->
          continue Current (fun x -> RD x) (fun x -> RF x) 0 0

let show path =
  let rec loop: 'a 'b. _ -> ('a, 'b) t -> _ = fun (type a) (type b) acc (path: (a, b) t) ->
    match path with
      | Root ->
          "/" :: acc
      | Current ->
          (
            match acc with
              | [] -> [ "." ]
              | _ -> "./" :: acc
          )
      | Parent Current ->
          (
            match acc with
              | [] -> [ ".." ]
              | _ -> "../" :: acc
          )
      | Parent parent ->
          loop (
            match acc with
              | [] -> [ ".." ]
              | _ -> "../" :: acc
          ) parent
      | Dir (filename, Current) ->
          filename :: "/" :: acc
      | File (filename, Current) ->
          (* acc = [] *)
          [ filename ]
      | Dir (filename, parent) ->
          loop (filename :: "/" :: acc) parent
      | File (filename, parent) ->
          (* acc = [] *)
          loop [ filename ] parent
  in
  loop [] path
  |> String.concat ""

let rec show_ocaml: 'a 'b. ('a, 'b) t -> _ = fun (type a) (type b) (path: (a, b) t) ->
  match path with
    | Root ->
        "Root"
    | Current ->
        "Current"
    | Parent parent ->
        "Parent (" ^ show_ocaml parent ^ ")"
    | Dir (filename, parent) ->
        "Dir (\"" ^ String.escaped filename ^ "\", " ^ show_ocaml parent ^ ")"
    | File (filename, parent) ->
        "File (\"" ^ String.escaped filename ^ "\", " ^ show_ocaml parent ^ ")"

let rec concat: 'a 'b. ('a, dir) t -> (relative, 'b) t -> ('a, 'b) t =
  fun (type a) (type b) (parent: (a, dir) t) (path: (relative, b) t) ->
  let result: (a, b) t =
    match path with
      | Current ->
          parent
      | Parent path_parent ->
          Parent (concat parent path_parent)
      | Dir (filename, path_parent) ->
          Dir (filename, concat parent path_parent)
      | File (filename, path_parent) ->
          File (filename, concat parent path_parent)
  in
  result

let basename (type a) (type b) (path: (a, b) t) =
  match path with
    | Root | Current | Parent _ ->
        None
    | Dir (filename, _) | File (filename, _) ->
        Some filename

let parent (type a) (type b) (path: (a, b) t) =
  match path with
    | Root | Current | Parent _ ->
        None
    | Dir (_, parent) | File (_, parent) ->
        Some parent

let get_cwd () =
  let cwd = Sys.getcwd () in
  match parse cwd with
    | None ->
        raise (Sys_error ("current directory denotes an invalid path: " ^ cwd))
    | Some (AD path) ->
        path
    | Some (AF (File (filename, parent))) ->
        Dir (filename, parent)
    | Some (RD _ | RF _) ->
        raise (Sys_error ("current directory is not absolute: " ^ cwd))

let to_dir (type a) (type b) (path: (a, b) t) =
  let result: (a, dir) t =
    match path with
      | Root -> Root
      | Current -> Current
      | Parent _ -> path
      | Dir _ -> path
      | File (filename, parent) -> Dir (filename, parent)
  in
  result

let rec to_absolute: 'a 'b. ?from: _ -> ('a, 'b) t -> (absolute, 'b) t =
  fun (type a) (type b) ?(from = get_cwd ()) (path: (a, b) t) ->
  let result: (absolute, b) t =
    match path with
      | Root ->
          Root
      | Current ->
          from
      | Parent parent ->
          Parent (to_absolute ~from parent)
      | Dir (filename, parent) ->
          Dir (filename, to_absolute ~from parent)
      | File (filename, parent) ->
          File (filename, to_absolute ~from parent)
  in
  result

(* For [to_relative_dir], as it's much easier to implement with reversed paths. *)
type item =
  | I_parent
  | I_file of Filename.t

let rec items: 'a. _ -> (absolute, 'a) t -> _ = fun (type a) acc (path: (absolute, a) t) ->
  match path with
    | Root ->
        acc
    | Parent parent ->
        items (I_parent :: acc) parent
    | Dir (filename, parent)
    | File (filename, parent) ->
        items (I_file filename :: acc) parent

let to_relative_dir ?(from = get_cwd ()) (path: (_, _) t) =
  match pack_relativity (to_dir path) with
    | R path ->
        Some path
    | A path ->
        let rec remove_prefix from path =
          match from, path with
            | [], _ ->
                Some path
            | _ :: _, []
            | I_parent :: _, I_file _ :: _
            | I_file _ :: _, I_parent :: _ ->
                None
            | I_parent :: from_tail, I_parent :: path_tail ->
                remove_prefix from_tail path_tail
            | I_file from_filename :: from_tail, I_file path_filename :: path_tail ->
                if (from_filename: string) = path_filename then
                  remove_prefix from_tail path_tail
                else
                  None
        in
        match remove_prefix (items [] from) (items [] path) with
          | None ->
              None
          | Some suffix ->
              let rec make acc = function
                | [] ->
                    Some acc
                | I_parent :: tail ->
                    make (Parent acc) tail
                | I_file filename :: tail ->
                    make (Dir (filename, acc)) tail
              in
              make Current suffix

type 'a to_relative_result =
  | R_none
  | R_some of (relative, 'a) t
  | R_empty

let to_relative (type a) ?from (path: (_, a) t): a to_relative_result =
  match pack_relativity path with
    | R path ->
        R_some path
    | A path ->
        let handle_dir path =
          match to_relative_dir ?from path with
            | None ->
                R_none
            | Some path ->
                R_some path
        in
        match path with
          | Root as x ->
              handle_dir x
          | Parent _ as x ->
              handle_dir x
          | Dir _ as x ->
              handle_dir x
          | File (filename, parent) ->
              match to_relative_dir ?from (Dir (filename, parent)) with
                | None ->
                    R_none
                | Some Current ->
                    R_empty
                | Some (Parent _) ->
                    (* [to_relative_dir] only returns a path that ends with [..]
                       if [path] itself ends in [..], in which case it would be
                       a [dir] path. *)
                    assert false
                | Some (Dir (filename, parent)) ->
                    R_some (File (filename, parent))

module Any =
struct

  let show = function
    | AD path -> show path
    | AF path -> show path
    | RD path -> show path
    | RF path -> show path

  let show_ocaml = function
    | AD path -> show_ocaml path
    | AF path -> show_ocaml path
    | RD path -> show_ocaml path
    | RF path -> show_ocaml path

  let concat a b =
    match a, b with
      | (AF _ | RF _), _ | _, (AD _ | AF _) -> None
      | AD a, RD b -> Some (AD (concat a b))
      | AD a, RF b -> Some (AF (concat a b))
      | RD a, RD b -> Some (RD (concat a b))
      | RD a, RF b -> Some (RF (concat a b))

  let basename = function
    | AD path -> basename path
    | AF path -> basename path
    | RD path -> basename path
    | RF path -> basename path

  let parent = function
    | AD path -> (match parent path with None -> None | Some x -> Some (AD x))
    | AF path -> (match parent path with None -> None | Some x -> Some (AD x))
    | RD path -> (match parent path with None -> None | Some x -> Some (RD x))
    | RF path -> (match parent path with None -> None | Some x -> Some (RD x))

  let to_absolute ?from = function
    | AD path -> D path
    | AF path -> F path
    | RD path -> D (to_absolute ?from path)
    | RF path -> F (to_absolute ?from path)

  type local_to_relative_result =
    | R_none
    | R_some of relative any_kind
    | R_empty

  let convert_to_relative_result: _ to_relative_result -> local_to_relative_result = function
    | R_none -> R_none
    | R_some path -> R_some (pack_kind path)
    | R_empty -> R_empty

  type to_relative_result = local_to_relative_result =
    | R_none
    | R_some of relative any_kind
    | R_empty

  let to_relative ?from = function
    | AD path -> convert_to_relative_result (to_relative ?from path)
    | AF path -> convert_to_relative_result (to_relative ?from path)
    | RD path -> R_some (pack_kind path)
    | RF path -> R_some (pack_kind path)

  let to_relative_dir ?from = function
    | AD path -> to_relative_dir ?from path
    | AF path -> to_relative_dir ?from path
    | RD path -> Some path
    | RF path -> Some (to_dir path)

  let to_dir = function
    | AD path -> A path
    | AF path -> A (to_dir path)
    | RD path -> R path
    | RF path -> R (to_dir path)

end

module Any_relativity =
struct

  let pack = function
    | A path -> pack path
    | R path -> pack path

  let show = function
    | A path -> show path
    | R path -> show path

  let show_ocaml = function
    | A path -> show_ocaml path
    | R path -> show_ocaml path

  let concat a b =
    match a with
      | A a -> A (concat a b)
      | R a -> R (concat a b)

  let basename = function
    | A path -> basename path
    | R path -> basename path

  let parent = function
    | A path -> (match parent path with None -> None | Some x -> Some (A x))
    | R path -> (match parent path with None -> None | Some x -> Some (R x))

  let to_absolute ?from = function
    | A path -> path
    | R path -> to_absolute ?from path

  let to_relative_dir ?from = function
    | A path -> to_relative_dir ?from path
    | R path -> Some (to_dir path)

  let to_relative ?from = function
    | A path -> to_relative ?from path
    | R path -> R_some path

  let to_dir = function
    | A path -> A (to_dir path)
    | R path -> R (to_dir path)

end

module Any_kind =
struct

  let pack = function
    | D path -> pack path
    | F path -> pack path

  let show = function
    | D path -> show path
    | F path -> show path

  let show_ocaml = function
    | D path -> show_ocaml path
    | F path -> show_ocaml path

  let concat a b =
    match b with
      | D b -> D (concat a b)
      | F b -> F (concat a b)

  let basename = function
    | D path -> basename path
    | F path -> basename path

  let parent = function
    | D path -> parent path
    | F path -> parent path

  let to_absolute ?from = function
    | D path -> D (to_absolute ?from path)
    | F path -> F (to_absolute ?from path)

  let to_relative ?from = function
    | D path -> Any.convert_to_relative_result (to_relative ?from path)
    | F path -> Any.convert_to_relative_result (to_relative ?from path)

  let to_relative_dir ?from = function
    | D path -> to_relative_dir ?from path
    | F path -> to_relative_dir ?from path

  let to_dir = function
    | D path -> path
    | F path ->  to_dir path

end
