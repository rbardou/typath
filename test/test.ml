open Typath.UNIX

let fail x = Printf.ksprintf (fun s -> prerr_endline ("Test failed: " ^ s); exit 1) x

let test_invalid_filename () =
  let test s =
    match Filename.parse s with
      | None ->
          ()
      | Some path ->
          fail "%S parsed as filename %S while it shouldn't parse" s (Filename.show path)
  in
  test "";
  test "\000";
  test ".";
  test "..";
  test "/";
  test "abcd/efg";
  test "abc\000";
  ()

let test_invalid_path () =
  let test s =
    match parse s with
      | None ->
          ()
      | Some path ->
          fail "%S parsed as path %S while it shouldn't parse" s (Any.show path)
  in
  test "";
  test "\000";
  test "/\000";
  test "\000/";
  test "a/b/c/\000";
  test "abc\000xyz";
  test "\000xyz";
  test "abc\000";
  ()

let f = Filename.parse_exn

let parse s =
  match parse s with
    | None ->
        fail "failed to parse %S" s
    | Some u ->
        u

let test_string_conversion () =
  let test path strings =
    match strings with
      | [] ->
          invalid_arg "empty list in test_string_conversion"
      | canonical_string :: _ ->
          let shown = show path in
          if shown <> canonical_string then
            fail "%s shows as %S, expected %S" (show_ocaml path) shown canonical_string;
          let test_string str =
            let any = parse str in
            let packed = pack path in
            if (any: any) <> packed then
              fail "%S parsed as %s, expected %s" str (Any.show_ocaml any)
                (Any.show_ocaml packed)
          in
          List.iter test_string strings
  in
  test Root [
    "/";
    "/.";
    "//";
    "/./";
    "//.";
    "///";
    "/./.";
    "//.///./././/.///";
    "//.///./././/.///.";
  ];
  test Current [
    ".";
    "./";
    "./.";
    ".//";
    ".//.//././/.";
  ];
  test (Parent Root) [
    "/..";
    "/./..";
    "/./../";
    "/./.././/.//";
    "/../";
    "///.././";
    "/.//././/./.././";
  ];
  test (Parent Current) [
    "..";
    "./..";
    "./../";
    "./.././/.//";
    "../";
    ".././";
    ".//././/./.././";
  ];
  test (Dir (f "x", Current)) [
    "x/";
    "./x/";
    "./x///./.";
    ".//./////x///./.";
    "x///./.";
  ];
  test (File (f "x", Current)) [
    "x";
    "./x";
    ".///././x";
  ];
  test (File (f "x", Root)) [
    "/x";
    "///./x";
  ];
  test (Dir (f "x", Root)) [
    "/x/";
    "///./x///.//.";
  ];
  test (File (f "y", Dir (f "x", Current))) [
    "x/y";
    "x//y";
    "x/./y";
    "x/././/./y";
    "./x/y";
    ".//././/x/.//./y";
  ];
  test (Dir (f "z", Dir (f "y", Dir (f "x", Current)))) [
    "x/y/z/";
    "x//y///z///";
    "x/./y/z/";
    "x/././/./y/./z/";
    "./x/y/././//./z/.//.//.";
    ".//././/x/.//./y/z///";
  ];
  test (Dir (f "z", Dir (f "y", Dir (f "x", Root)))) [
    "/x/y/z/";
    "//x//y///z///";
    "/.//x/./y/z/";
    "//./x/././/./y/./z/";
    "//././//././x/y/././//./z/.//.//.";
    "//././/././/x/.//./y/z///";
  ];
  test (File (f "x", Parent Root)) [
    "/../x";
    "//.//.//../x";
    "//.././/./x";
    "//.//.//.././/./x";
  ];
  test (File (f "x", Parent Current)) [
    "../x";
    "./../x";
    ".///./../x";
    ".././/././/x";
  ];
  test (Parent (Dir (f "x", Root))) [
    "/x/..";
    "//.//x/./..////";
  ];
  test (Parent (Dir (f "x", Current))) [
    "x/..";
    "x/../";
    "x/./..////";
    ".//.//x/./..////";
  ];
  test (Dir (f ".ssh", Dir (f "user", Dir (f "home", Root)))) [
    "/home/user/.ssh/";
    "/home/user/.ssh/.";
    "/home/user//.ssh/.";
    "/home/user/./.ssh/.";
  ];
  test (File (f ".zshrc", Dir (f "user", Dir (f "home", Root)))) [
    "/home/user/.zshrc";
  ];
  test (File (f "#test.ml#", Dir (f "ocaml", Dir (f "user", Dir (f "home", Root))))) [
    "/home/user/ocaml/#test.ml#";
  ];
  test (
    File (f "test.ml~", Dir (f "ocaml_projects-old", Dir (f "user", Dir (f "home", Root))))
  ) [
    "/home/user/ocaml_projects-old/test.ml~";
    "/home////user/.//ocaml_projects-old/test.ml~";
  ];
  ()

let test_concatenation () =
  let test a b result =
    let continue_with_parsed_b a b =
      let c = concat a b in
      let result_any = parse result in
      let packed_c = pack c in
      if packed_c <> result_any then
        fail "concat (%s) (%s) <> %s" (show_ocaml a) (show_ocaml b)
          (Any.show_ocaml result_any);
      let pseudo_string = show a ^ "/" ^ show b in
      let pseudo_c = parse pseudo_string in
      if packed_c <> pseudo_c then
        fail "concat (%s) (%s) <> parse %S" (show_ocaml a) (show_ocaml b)
          pseudo_string
    in
    let continue_with_parsed_a a =
      match parse b with
        | AD _
        | AF _ ->
            fail "%S is not a relative path, cannot concatenate it" b
        | RD b ->
            continue_with_parsed_b a b
        | RF b ->
            continue_with_parsed_b a b
    in
    match parse a with
      | AF _
      | RF _ ->
          fail "%S is not a directory path, cannot concatenate to it" a
      | AD a ->
          continue_with_parsed_a a
      | RD a ->
          continue_with_parsed_a a
  in
  test "/" "." "/";
  test "." "." ".";
  test ".." "." "..";
  test "x/" "." "x/";
  test "/" ".." "/..";
  test "." ".." "..";
  test ".." ".." "../..";
  test "x/" ".." "x/..";
  test "/" "y" "/y";
  test "." "y" "y";
  test ".." "y" "../y";
  test "x/" "y" "x/y";
  test "/" "y/" "/y/";
  test "." "y/" "y/";
  test ".." "y/" "../y/";
  test "x/" "y/" "x/y/";
  test "/x/" "y/" "/x/y/";
  test "/x/y/" "z/t" "/x/y/z/t";
  test "/x/y/" "." "/x/y/";
  test "/x/y/" "../.." "/x/y/../..";
  test "/x/y/../z/.." "a/../b/c" "/x/y/../z/../a/../b/c";
  test "./x/" "y/" "./x/y/";
  test "./x/y/" "z/t" "./x/y/z/t";
  test "./x/y/" "z/t/" "./x/y/z/t/";
  test "./x/y/" "." "./x/y/";
  test "./x/y/" "../.." "./x/y/../..";
  test "./x/y/../z/.." "a/../b/c" "./x/y/../z/../a/../b/c";
  ()

let show_option show_item = function
  | None -> "None"
  | Some x -> "Some (" ^ show_item x ^ ")"

let test_split () =
  let test full expected_parent expected_basename =
    let expected_parent = Option.map parse expected_parent in
    let expected_basename =
      match expected_basename with
        | None ->
            None
        | Some expected_basename ->
            match Filename.parse expected_basename with
              | None ->
                  fail "%S failed to parse as a filename" expected_basename
              | Some expected_basename ->
                  Some expected_basename
    in
    let full = parse full in
    let parent = Any.parent full in
    if parent <> expected_parent then
      fail "parent %s = %s, expected %s"
        (Any.show full)
        (show_option Any.show parent)
        (show_option Any.show expected_parent);
    let basename = Any.basename full in
    if basename <> expected_basename then
      fail "basename %s = %s, expected %s"
        (Any.show_ocaml full)
        (show_option Filename.show basename)
        (show_option Filename.show expected_basename);
  in
  test "/" None None;
  test "." None None;
  test ".." None None;
  test "x/" (Some ".") (Some "x");
  test "x" (Some ".") (Some "x");
  test "/x" (Some "/") (Some "x");
  test "../x" (Some "..") (Some "x");
  test "../x/" (Some "..") (Some "x");
  test "../x/.." None None;
  test "../x/y" (Some "../x/") (Some "y");
  test "../x/y/" (Some "../x/") (Some "y");
  test "/../x" (Some "/..") (Some "x");
  test "/../x/" (Some "/..") (Some "x");
  test "/../x/.." None None;
  test "/../x/y" (Some "/../x/") (Some "y");
  test "/../x/y/" (Some "/../x/") (Some "y");
  test "/home/user/.ssh/" (Some "/home/user/") (Some ".ssh");
  test "/home/user/.ssh/.." None None;
  test "/home/user/ocaml_projects-old/test.ml~" (Some "/home/user/ocaml_projects-old/")
    (Some "test.ml~");
  test "home/user/.ssh/" (Some "home/user/") (Some ".ssh");
  test "home/user/.ssh/.." None None;
  test "home/user/ocaml_projects-old/test.ml~" (Some "home/user/ocaml_projects-old/")
    (Some "test.ml~");
  ()

let test_to_dir () =
  let test path expected =
    match parse path, parse expected with
      | _, (AF _ | RF _) ->
          fail "%S is not a directory path" expected
      | (AF _ | AD _), RD _ ->
          fail "%S is absolute but %S is relative" path expected
      | (RF _ | RD _), AD _ ->
          fail "%S is relative but %S is absolute" path expected
      | AF p, AD e ->
          if to_dir p <> e then
            fail "to_dir %S = %S, expected %S" path (show (to_dir p)) expected
      | RF p, RD e ->
          if to_dir p <> e then
            fail "to_dir %S = %S, expected %S" path (show (to_dir p)) expected
      | AD p, AD e ->
          if to_dir p <> e then
            fail "to_dir %S = %S, expected %S" path (show (to_dir p)) expected
      | RD p, RD e ->
          if to_dir p <> e then
            fail "to_dir %S = %S, expected %S" path (show (to_dir p)) expected
  in
  test "/" "/";
  test "." ".";
  test ".." "..";
  test "x/" "x/";
  test "x" "x/";
  test "a/b/c" "a/b/c/";
  test "/a/b/c" "/a/b/c/";
  test "a/b/c/" "a/b/c/";
  test "/a/b/c/" "/a/b/c/";
  test "a/b/c/.." "a/b/c/..";
  test "/a/b/c/.." "/a/b/c/..";
  ()

let test_to_absolute () =
  let test path expected =
    let result = Any_kind.pack (Any.to_absolute ~from: (Dir (f "home", Root)) (parse path)) in
    let expected_path = parse expected in
    if result <> expected_path then
      fail "to_absolute %S = %S, expected %S" path (Any.show result) expected
  in
  test "/" "/";
  test "." "/home/";
  test ".." "/home/..";
  test "x/" "/home/x/";
  test "x" "/home/x";
  test "./a/b" "/home/a/b";
  test "./a/b/../c" "/home/a/b/../c";
  test "../a/b" "/home/../a/b";
  test "../a/b/../c" "/home/../a/b/../c";
  ()

let test_to_relative_dir () =
  let test from_s path_s expected_s =
    let from =
      match parse from_s with
        | AD p -> p
        | _ -> fail "%S is not an absolute directory path" from_s
    in
    let path = parse path_s in
    let expected =
      match expected_s with
        | None ->
            None
        | Some expected_s ->
            match parse expected_s with
              | RD p -> Some p
              | _ -> fail "%S is not a relative directory path" expected_s
    in
    let result_dir = Any.to_relative_dir ~from path in
    if result_dir <> expected then
      fail "to_relative_dir ~from: %S %S = %S, expected %S" from_s path_s
        (show_option show result_dir) (show_option (Printf.sprintf "%S") expected_s)
  in
  test "/" "/" (Some ".");
  test "/" "/.." (Some "..");
  test "/" "/x" (Some "x/");
  test "/" "/x/" (Some "x/");
  test "/a/" "/" None;
  test "/a/" "/.." None;
  test "/a/" "/x" None;
  test "/a/" "/x/" None;
  test "/x/" "/x" (Some ".");
  test "/x/" "/x/" (Some ".");
  test "/.." "/.." (Some ".");
  test "/.." "/../x/y/z" (Some "x/y/z/");
  test "/../x/" "/../x/y/z" (Some "y/z/");
  test "/../x/y/" "/../x/y/z" (Some "z/");
  test "/../x/y/../z/" "/../x/y/../z" (Some ".");
  test "/../x/y/z/t/" "/../x/y/z" None;
  test "/../x/y/z/t/x/y/z/" "/../x/y/z" None;
  test "/home/user/" "/home/user/ocaml/typed_path_lib" (Some "ocaml/typed_path_lib/");
  test "/home/user/" "/home/user/ocaml/typed_path_lib/" (Some "ocaml/typed_path_lib/");
  test "/home/user/" "/home/user/ocaml/typed_path_lib/.." (Some "ocaml/typed_path_lib/..");
  test "/home/user/" "/home/user/ocaml/../typed_path_lib/" (Some "ocaml/../typed_path_lib/");
  test "/home/user/" "/home/user/../ocaml/typed_path_lib/" (Some "../ocaml/typed_path_lib/");
  test "/home/user/" "/home/../user/ocaml/typed_path_lib/" None;
  ()

let test_to_relative () =
  let test from_s path_s expected =
    let from =
      match parse from_s with
        | AD p -> p
        | _ -> fail "%S is not an absolute directory path" from_s
    in
    let path = parse path_s in
    let expected =
      match expected with
        | `e ->
            Any.R_empty
        | `n ->
            Any.R_none
        | `s expected_s ->
            match parse expected_s with
              | RD p -> Any.R_some (pack_kind p)
              | RF p -> Any.R_some (pack_kind p)
              | _ -> fail "%S is not a relative path" expected_s
    in
    let result = Any.to_relative ~from path in
    let show_trr (trr: Any.to_relative_result) =
      match trr with
        | R_none -> "R_none"
        | R_some p -> "R_some \"" ^ Any_kind.show p ^ "\""
        | R_empty -> "R_empty"
    in
    if result <> expected then
      fail "to_relative ~from: %S %S = %s, expected %s" from_s path_s
        (show_trr result) (show_trr expected)
  in
  test "/" "/" (`s ".");
  test "/" "/.." (`s "..");
  test "/" "/x" (`s "x");
  test "/" "/x/" (`s "x/");
  test "/a/" "/" `n;
  test "/a/" "/.." `n;
  test "/a/" "/x" `n;
  test "/a/" "/x/" `n;
  test "/x/" "/x" `e;
  test "/x/" "/x/" (`s ".");
  test "/.." "/.." (`s ".");
  test "/.." "/../x/y/z" (`s "x/y/z");
  test "/../x/" "/../x/y/z" (`s "y/z");
  test "/../x/y/" "/../x/y/z" (`s "z");
  test "/../x/y/../z/" "/../x/y/../z" `e;
  test "/../x/y/../z/" "/../x/y/../z/" (`s ".");
  test "/../x/y/z/t/" "/../x/y/z" `n;
  test "/../x/y/z/t/x/y/z/" "/../x/y/z" `n;
  test "/home/user/" "/home/user/ocaml/typed_path_lib" (`s "ocaml/typed_path_lib");
  test "/home/user/" "/home/user/ocaml/typed_path_lib/" (`s "ocaml/typed_path_lib/");
  test "/home/user/" "/home/user/ocaml/typed_path_lib/.." (`s "ocaml/typed_path_lib/..");
  test "/home/user/" "/home/user/ocaml/../typed_path_lib/" (`s "ocaml/../typed_path_lib/");
  test "/home/user/" "/home/user/../ocaml/typed_path_lib/" (`s "../ocaml/typed_path_lib/");
  test "/home/user/" "/home/../user/ocaml/typed_path_lib/" `n;
  ()

let () =
  test_invalid_filename ();
  test_invalid_path ();
  test_string_conversion ();
  test_concatenation ();
  test_split ();
  test_to_dir ();
  test_to_absolute ();
  test_to_relative_dir ();
  test_to_relative ();
  ()
