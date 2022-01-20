
(** Same as {!Yojson.Basic.t} *)
type json = [
  | `Int of int
  | `Float of float
  | `String of string
  | `Bool of bool
  | `Null
  | `List of json list
  | `Assoc of (string * json) list
]

(** Representation of values from raw data *)
module Data_format : sig
  type t = {
    type_ : [`Json | `Csv | `Tsv | `Dsv of char];

    parse: json option;
    (** How to further parse the data (e.g. for dates).
        See https://vega.github.io/vega-lite/docs/data.html#format *)
  }

  val make :
    ?type_:[`Json | `Csv | `Tsv | `Dsv of char] ->
    ?parse:json ->
    unit -> t

  val to_json : t -> json
end

(** A collection of values *)
module Values : sig
  type t = [
    | `String of string
    | `Array of json array
    | `Obj of json
  ]

  (** Column-oriented API *)
  module Col : sig
    (** A convenience representation of data as record-of-columns,
        which suits OCaml better. It is transformed into an array of objects
        by Vega-lite. *)
    type t

    val floats : float array -> t
    val ints : int array -> t
    val strings : string array -> t
  end

  val col1 : Col.t -> t

  val col2 : string -> Col.t -> string -> Col.t -> t
  (** Pack 2 columns with their associated name.
      @raise Invalid_argument if columns don't have the same length *)

  val cols : (string * Col.t) list -> t
  (** Pack columns with their associated name.
      @raise Invalid_argument if columns don't have the same length *)

  val custom : json -> t

  val to_json : t -> json
end

(** Data to visualize *)
module Data : sig
  type inline = {
    name: string option;
    format_: Data_format.t option;
    values: Values.t;
  }

  type url = {
    url: string;
    name: string option;
    format_ : Data_format.t option;
  }

  type t = [
    | `Url of url
    | `Inline of inline
    | `Name of string
  ]

  (* TODO: data generators
     https://vega.github.io/vega-lite/docs/data.html#data-generators
     *)

  val url :
    ?name:string ->
    ?format_:Data_format.t ->
    string -> t

  val name : string -> t
  (** A name that creates a reference, to be manipulated through
      the Vega-lite API dynamically. *)

  val inline :
    ?name:string ->
    ?format_:Data_format.t ->
    Values.t ->
    t

  val to_json : t -> json
end

(** Mark: the kind of visualization *)
module Mark : sig
  type t = [
    | `Bar
    | `Line
  ]

  val bar : t
  val line : t

  val to_json : t -> json
end

module Encoding : sig
  type channel = [
    | `x | `y | `x2 | `y2
    | `xError | `yError | `xError2 | `yError2
    | `xOffset | `yOffset
    | `other of string
  ]

  (** Type for a field.
      See https://vega.github.io/vega-lite/docs/encoding.html *)
  type field_type = [
    | `quantitative
    | `temporal
    | `ordinal
    | `nominal
    | `other of json
  (* TODO: geojson *)
  ]

  type scale = [
    | `other of json
  ]

  type bin = [
    | `bool of bool
    | `binned (** already binned *)
  ]

  type aggregate = [
    | `mean | `sum | `median | `min | `max | `count | `other of json
  ]

  (* TODO: timeUnit *)
  (* TODO: axis *)
  (* TODO: legend *)
  (* TODO: format *)
  (* TODO: stack *)
  (* TODO: sort *)
  (* TODO: condition *)

  type field_def = {
    field: [`Field of string | `Repeat of string];
    type_: field_type;
    bin: bin;
    aggregate: aggregate option;
    title: string option;
    scale: scale option; (* TODO *)
  }

  type value = json
  type datum = json

  type definition = [
    | `Field of field_def
    | `Field_repeat of string
    | `Value of value
    | `Datum of datum
  ]

  type channel_def

  type t = channel_def list

  val field :
    channel ->
    ?bin:bin ->
    ?title:string ->
    ?aggregate:aggregate ->
    name:string ->
    type_:field_type ->
    unit -> channel_def

  val field_repeat : channel -> string -> channel_def

  val datum : channel -> datum -> channel_def
  val datum_i : channel -> int -> channel_def
  val datum_f : channel -> float -> channel_def
  val datum_s : channel -> string -> channel_def

  val to_json : t -> json
end

(** A (toplevel) visualization of data using a mark and encodings *)
module Viz : sig
  type t = {
    data: Data.t;
    mark: Mark.t;
    encoding: Encoding.t option;
  }

  val make :
    data:Data.t ->
    mark:Mark.t ->
    ?encoding:Encoding.t ->
    unit ->
    t

  val to_json : t -> json
  val to_json_str : t -> string
end
