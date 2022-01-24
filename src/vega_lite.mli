
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

  (** Row oriented API.

      Each row is a record of values (encoded to a json object).
      An example of use would be [Row.(empty |> int "x" 1 |> float "y" 2.)] *)
  module Row : sig
    type t

    val empty : t
    val int : string -> int -> t -> t
    val float : string -> float -> t -> t
    val string  : string -> string -> t -> t
    val to_json : t -> json
  end

  val rows : Row.t array -> t

  val rows_l : Row.t list -> t

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
  type t

  val bar : ?opts:(string*json) list -> unit -> t
  val line : ?opts:(string*json) list -> ?point:bool -> unit -> t
  val point : ?opts:(string*json) list -> unit -> t
  val circle : ?opts:(string*json) list -> unit -> t
  val tick : ?opts:(string*json) list -> unit -> t

  val to_json : t -> json
end

(** Encoding data into the channels expected by the chosen {!Mark.t}.

    For examples, {!Mark.line} expects channels for "x" and "y" to be defined
    and bound to numerical data. *)
module Encoding : sig
  type channel = [
    | `x | `y | `x2 | `y2
    | `xError | `yError | `xError2 | `yError2
    | `xOffset | `yOffset
    | `theta | `theta2 | `radius | `radius2
    | `longitude | `latitude | `longitude2 | `latitude2
    | `angle | `color | `fill | `stroke | `opacity | `fillopacity
    | `strokeOpacity | `shape | `size | `strokeDash | `strokeWidth
    | `text | `tooltip | `href | `description
    | `detail | `order | `facet | `row | `column
    | `other of string
  ]

  (** Type for a field.
      See https://vega.github.io/vega-lite/docs/encoding.html *)
  type field_type = [
    | `quantitative
    | `temporal
    | `ordinal
    | `nominal
    | `geojson
    | `other of json
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

  type field_def
  type value = json
  type datum = json

  type definition = [
    | `Field of field_def
    | `Value of value
    | `Datum of datum
  ]

  type channel_def

  type t = channel_def list

  (** Common parameters to define fields *)
  type 'a field_builder =
    channel ->
    ?bin:bin ->
    ?scale:scale ->
    ?title:string ->
    ?aggregate:aggregate ->
    ?opts:(string * json) list ->
    'a

  (** A field, with a name and type, typically binding a field
      in the data source. *)
  val field :
    (name:string ->
     type_:field_type ->
     unit -> channel_def) field_builder

  val field_repeat_var :
    (string -> channel_def) field_builder
  (** Use the value of the given repeated variable (see {!Viz}) *)

  val field_repeat : (unit -> channel_def) field_builder
  (** Same as [field_repeat_var "repeat"] *)

  val datum : channel -> datum -> channel_def
  val datum_i : channel -> int -> channel_def
  val datum_f : channel -> float -> channel_def
  val datum_s : channel -> string -> channel_def

  val value : channel -> datum -> channel_def
  val value_i : channel -> int -> channel_def
  val value_f : channel -> float -> channel_def
  val value_s : channel -> string -> channel_def

  val to_json : t -> json
end

(** User input. *)
module Input : sig
  type t

  val range : min:int -> max:int -> ?step:int -> unit -> t

  val select : json list -> t
  val select_str : string list -> t

  val checkbox : t

  val radio : string list -> t

  val to_json : t -> json
end

(** User selection.

    https://vega.github.io/vega-lite/docs/selection.html
*)
module Selection : sig
  type t

  type 'a with_opts =
    ?on:[`mouseover] ->
    ?clear: [`mouseup] ->
    ?fields:string list ->
    ?opts:(string * json) list ->
    'a

  val point : (unit -> t) with_opts
  (** Select discret values. *)

  val interval : (unit -> t) with_opts
  (** Continuous range *)

  val to_json : t -> json
end

(** Parameters.

    Parameters provide dynamic behavior based on user input (for example,
    selection).

    https://vega.github.io/vega-lite/docs/parameter.html *)
module Param : sig
  type t

  val input : name:string -> ?value:json -> Input.t -> t
  (** Parameter bound to an input *)

  val select :
    name:string -> ?value:json ->
    ?bind:Input.t ->
    ?bind_by_name:(string * Input.t) list ->
    Selection.t -> t
  (** Selection parameter.
      See https://vega.github.io/vega-lite/docs/selection.html

      @param bind binds input to the results of the selection
      @param bind_by_name binds inputs to the results of the selection
      @raise Invalid_argument if both [bind] and [bind_by_name] are specified.
  *)

  val bind_scales : ?name:string -> unit -> t
  (** Bind selection to scale, to zoom around the plot.
      @param name the parameter name, by default "grid". *)

  val to_json : t -> json
end

(** Configuration *)
module Config : sig
  type t

  val json : json -> t
  val to_json : t -> json
end

(** A (toplevel) visualization of data using a mark and encodings *)
module Viz : sig
  type repeat_binding = {
    var: string;
    values: json list;
  }

  type repeat_spec

  (** The main visualization type. *)
  type t

  (** With options *)
  type 'a with_config =
    ?width:[`container | `int of int] ->
    ?height:[`container | `int of int] ->
    ?title:string ->
    ?config:Config.t ->
    ?params:Param.t list ->
    'a

  val make :
    (data:Data.t ->
     mark:Mark.t ->
     ?encoding:Encoding.t ->
     unit ->
     t) with_config
  (** Make a simple visualization. It can then be composed, if desired,
      using {!layer} or {!repeat}. *)

  val layer : (t list -> t) with_config
  (** Superpose visualizations *)

  val hconcat : (t list -> t) with_config
  (** Horizontal concatenation *)

  val vconcat : (t list -> t) with_config
  (** Vertical concatenation *)

  val concat : (columns:int -> t list -> t) with_config
  (** General concatenation *)

  val bind : var:string -> json list -> repeat_binding
  val bind_i : var:string -> int list -> repeat_binding
  val bind_f : var:string -> float list -> repeat_binding
  val bind_s : var:string -> string list -> repeat_binding

  (** Repeat a visualization across layers, values, columns, rows, or
      bindings *)
  val repeat :
    (?column:string list ->
     ?row:string list ->
     ?layer:string list ->
     ?bind:repeat_binding list ->
     data:Data.t ->
     t -> t) with_config

  (** Repeat a simple list of values. To use these in the spec,
      use {!Encoding.field_repeat}. *)
  val repeat_simple :
    (repeat:string list -> data:Data.t -> t -> t) with_config

  val to_json : t -> json

  val to_json_str : t -> string
  (** Turn into JSON, and then into a pretty-printed json string. *)

  val to_json_file : t -> file:string -> unit
  (** Gets a string using {!to_json_str} and then writes it to file [file] *)
end
