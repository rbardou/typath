(** UNIX paths. *)

module Filename:
sig
  (** UNIX filenames. *)

  (** UNIX filenames.

      Cannot be [""], ["."] or [".."].
      Cannot contain characters ['/'] or ['\000']. *)
  type t

  (** Convert a string to a UNIX filename. *)
  val parse: string -> t option

  (** Same as [parse_filename], but raise [Invalid_arg] instead of returning [None].

      You should only use this to define constants. *)
  val parse_exn: string -> t

  (** Convert a UNIX filename to a string. *)
  val show: t -> string

  (** Compare two filenames. *)
  val compare: t -> t -> int
end

(** Type annotation for absolute paths. *)
type absolute = Absolute

(** Type annotation for relative paths. *)
type relative = Relative

(** Type annotation for file paths. *)
type file = File

(** Type annotation for directory paths. *)
type dir = Dir

(** Canonical, typed representation of UNIX paths.

    The first type parameter can be either [absolute] or [relative].
    It denotes whether the path starts with ["/"].

    The second type parameter can be either [file] or [dir].
    It denotes whether further items can be appended, and whether the path ends with ["/"].
    Note that paths ending with a [File] can actually denote directories
    (and can thus be converted to directory paths using [to_dir])
    but paths ending with a [Dir] should not be used to denote files.

    This representation is canonical in the sense that:
    - ["."] and ["./"] have the same representation;
    - ["./a"] and ["a"] also have the same representation;
    - there cannot be empty items (["//"]);
    - ["."] can only appear at the beginning.

    This means that one cannot distinguish between some equivalent paths
    that would be different once converted to strings,
    but this limits complexity and makes it easier to reason about paths.

    Constructors:
    - [Root] denotes ["/"];
    - [Current] denotes ["./"];
    - [Parent p] denotes [p ^ "../"];
    - [Dir (f, p)] denotes [p ^ f ^ "/"];
    - [File (f, p)] denotes [p ^ f]. *)
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

(** UNIX paths packed to have no type parameters. *)
type any =
  | AD of (absolute, dir) t
  | AF of (absolute, file) t
  | RD of (relative, dir) t
  | RF of (relative, file) t

(** Pack a path. *)
val pack: (_, _) t -> any

(** UNIX paths packed to have no relativity (absolute or relative) parameter. *)
type 'a any_relativity =
  | A of (absolute, 'a) t
  | R of (relative, 'a) t

(** Pack the relativity of a path. *)
val pack_relativity: (_, 'a) t -> 'a any_relativity

(** UNIX paths packed to have no kind (directory or file) parameter. *)
type 'a any_kind =
  | D of ('a, dir) t
  | F of ('a, file) t

(** Pack the kind of a path. *)
val pack_kind: ('a, _) t -> 'a any_kind

(** Convert a string into a path.

    This can parse all valid UNIX paths, but some information is lost:
    - non-empty strings which start with neither ["/"] nor ["./"] behave as if ["./"]
      was prepended to them
      (for instance ["x"] is parsed as [File ("x", Current)], i.e. ["./x"]);
    - all ["."] path items are ignored except the one at the beginning (if any),
      which is used to decide between [Root] and [Current]
      (for instance ["/./"] is parsed as [Root], i.e. ["/"]);
    - all empty [""] path items are ignored except the one at the end (if any),
      which is used to decide between [Dir] and [File]
      (for instance ["//"] is parsed as [Root], i.e. ["/"]).
    This means that some different strings have the same representation.

    Return [None] for the empty string [""] and for strings which contain
    the null character ['\000']. Return [Some path] for all other strings. *)
val parse: string -> any option

(** Convert a path into a string.

    When several possible string representations are possible, such as:
    - ["./x"] or ["x"];
    - ["../"] or [".."];
    - ["x/../"] or ["x/.."];
    the smallest possible result is used.

    Never returns the empty string [""], as it is not a valid path. *)
val show: (_, _) t -> string

(** Convert a path into a string using OCaml syntax for debugging purposes.

    For instance, [show_ocaml (Filename ("x", Parent Current))]
    is ["Filename (\"x\", Parent (Current))"]. *)
val show_ocaml: (_, _) t -> string

(** Concatenate two UNIX paths.

    Usage: [concat a b]

    Equivalent to [parse (show a ^ "/" ^ show b)]. *)
val concat: ('a, dir) t -> (relative, 'b) t -> ('a, 'b) t

(** Return the filename at the end of a path.

    Return [Some filename] for [Dir (filename, _)] and [File (filename, _)].
    Return [None] for [Parent], [Current] and [Root].

    Note that this is not completely equivalent to [Filename.basename].
    Indeed, [Filename.basename "x/."] returns ["."], but
    [basename_u (parse "x/.")] returns ["x"], because this module represents
    ["x/."] as ["x/"]. *)
val basename: (_, _) t -> Filename.t option

(** Remove the filename at the end of a path.

    Return [Some parent] for [Dir (_, parent)] and [File (_, parent)].
    Return [None] for [Parent], [Current] and [Root].

    Note that this is not completely equivalent to [Filename.dirname].
    Note that [Filename.dirname "x/."] returns ["x"], but
    [parent_u (parse "x/.")] returns ["."], because this module represents
    ["x/."] as ["x/"]. *)
val parent: ('a, 'b) t -> ('a, dir) t option

(** Get the current working directory.

    @raise [Sys_error] if the current working directory cannot be retrieved
    or is not absolute. *)
val get_cwd: unit -> (absolute, dir) t

(** Convert a path to a directory path.

    If the path is already a directory path, return it unchanged.
    Else, convert its [File] into a [Dir]. *)
val to_dir: ('a, 'b) t -> ('a, dir) t

(** Note: there is no [to_file] because paths ending with ["/"] should not be used
    to denote files. *)

(** Convert a path to an absolute path.

    If the path is already absolute, return it unchanged.
    Else, concatenate it with [from] to make it absolute.
    Default value for [from] is [get_cwd ()]. *)
val to_absolute: ?from: (absolute, dir) t -> (_, 'a) t -> (absolute, 'a) t

(** Convert an absolute directory path to a relative directory path.

    Usage: [to_relative_dir path]

    If [path] is a file path, it is converted with {!to_dir} first.

    If [path] is already relative, return it unchanged.
    Else, if [from] is a prefix of [path], return [Some suffix]
    such that [concat from suffix] is equal to [path];
    if [from] is not a prefix of [path], return [None].

    Note that if [from] contains [Parent]s (i.e. [".."] items),
    they must also be present in [path] for [from] to be considered a prefix
    of [path]. [Parent]s are not simplified, as ["a/b/.."] is not necessarily
    equivalent to ["b/.."] (if [a] is a symbolic link). *)
val to_relative_dir: ?from: (absolute, dir) t -> (_, _) t -> (relative, dir) t option

(** Possible results for [to_relative]. *)
type 'a to_relative_result =
  | R_none
  | R_some of (relative, 'a) t
  | R_empty

(** Same as [to_relative_dir], but do not convert file paths to directory paths.

    The result type is a bit more complex to handle the case where
    the result is ["."]. This is a directory path, with type
    [(relative, dir) t] and not [(relative, 'a) t], so it needs a special
    return value ([R_empty]). This case happens when the two paths are equal
    modulo a ['/'] at the end. It does not happen when the two paths
    are both directory paths.

    Otherwise, [to_relative] returns the same as [to_relative_dir]
    with [R_none] instead of [None] and [R_some] instead of [Some]. *)
val to_relative: ?from: (absolute, dir) t -> (_, 'a) t -> 'a to_relative_result

module Any:
sig
  (** Operations on packed paths. *)

  (** Same as [show] but for [any]. *)
  val show: any -> string

  (** Same as [show_ocaml] but for [any]. *)
  val show_ocaml: any -> string

  (** Same as [concat] but for [any] on both sides.

      Return [None] if the left-hand side is not a directory path
      or if the right-hand side is not a relative path. *)
  val concat: any -> any -> any option

  (** Same as [basename] but for [any]. *)
  val basename: any -> Filename.t option

  (** Same as [parent] but for [any]. *)
  val parent: any -> any option

  (** Same as [to_dir] but for [any]. *)
  val to_dir: any -> dir any_relativity

  (** Same as [to_absolute] but for [any]. *)
  val to_absolute: ?from: (absolute, dir) t -> any -> absolute any_kind

  (** Same as [to_relative_dir] but for [any]. *)
  val to_relative_dir: ?from: (absolute, dir) t -> any -> (relative, dir) t option

  (** Same as [to_relative_result] but for [any]. *)
  type to_relative_result =
    | R_none
    | R_some of relative any_kind
    | R_empty

  (** Same as [to_relative] but for [any]. *)
  val to_relative: ?from: (absolute, dir) t -> any -> to_relative_result
end

module Any_relativity:
sig
  (** Operations on paths with packed relativity. *)

  (** Pack the kind of a path with already packed relativity. *)
  val pack: _ any_relativity -> any

  (** Same as [show] but for [any_relativity]. *)
  val show: _ any_relativity -> string

  (** Same as [show_ocaml] but for [any_relativity]. *)
  val show_ocaml: _ any_relativity -> string

  (** Same as [concat] but for [any_relativity] on the left-hand side. *)
  val concat: dir any_relativity -> (relative, 'a) t -> 'a any_relativity

  (** Same as [basename] but for [any_relativity]. *)
  val basename: _ any_relativity -> Filename.t option

  (** Same as [parent] but for [any_relativity]. *)
  val parent: _ any_relativity -> dir any_relativity option

  (** Same as [to_dir] but for [any_relativity]. *)
  val to_dir: _ any_relativity -> dir any_relativity

  (** Same as [to_absolute] but for [any_relativity]. *)
  val to_absolute: ?from: (absolute, dir) t -> 'a any_relativity -> (absolute, 'a) t

  (** Same as [to_relative_dir] but for [any_relativity]. *)
  val to_relative_dir: ?from: (absolute, dir) t -> _ any_relativity ->
    (relative, dir) t option

  (** Same as [to_relative] but for [any_relativity]. *)
  val to_relative: ?from: (absolute, dir) t -> 'a any_relativity -> 'a to_relative_result
end

module Any_kind:
sig
  (** Operations on paths with packed kind. *)

  (** Pack the relativity of a path with already packed kind. *)
  val pack: _ any_kind -> any

  (** Same as [show] but for [any_kind]. *)
  val show: _ any_kind -> string

  (** Same as [show_ocaml] but for [any_kind]. *)
  val show_ocaml: _ any_kind -> string

  (** Same as [concat] but for [any_kind] on the right-hand side. *)
  val concat: ('a, dir) t -> relative any_kind -> 'a any_kind

  (** Same as [basename] but for [any_kind]. *)
  val basename: _ any_kind -> Filename.t option

  (** Same as [parent] but for [any_kind]. *)
  val parent: 'a any_kind -> ('a, dir) t option

  (** Same as [to_dir] but for [any_kind]. *)
  val to_dir: 'a any_kind -> ('a, dir) t

  (** Same as [to_absolute] but for [any_kind]. *)
  val to_absolute: ?from: (absolute, dir) t -> _ any_kind -> absolute any_kind

  (** Same as [to_relative_dir] but for [any_kind]. *)
  val to_relative_dir: ?from: (absolute, dir) t -> _ any_kind ->
    (relative, dir) t option

  (** Same as [to_relative] but for [any_kind]. *)
  val to_relative: ?from: (absolute, dir) t -> _ any_kind -> Any.to_relative_result
end
