
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
module Data_format = struct
  type t = {
    type_ : [`Json | `Csv | `Tsv | `Dsv of char];
    parse: json option;
  }

  let make ?(type_=`Json) ?parse () : t = {type_; parse}
  let to_json self : json =
    let type_, others =
      match self.type_ with
      | `Json -> `String "json", []
      | `Csv -> `String "csv", []
      | `Tsv -> `String "tsv", []
      | `Dsv c -> `String "dsv", ["delimiter", `String (Printf.sprintf "%c" c)]
    in
    `Assoc (others @ [
      "type", type_;
      "parse", (match self.parse with None -> `Null | Some j -> j);
    ])
end

module Values = struct
  type t = [
    | `String of string
    | `Array of json array
    | `Obj of json
  ]

  module Col = struct
    type t =
      | Float of float array
      | Int of int array
      | String of string array

    let floats a : t = Float a
    let ints a : t = Int a
    let strings a : t = String a

    let get_i_json a i = match a with
      | Float a -> `Float a.(i)
      | Int a -> `Int a.(i)
      | String a -> `String a.(i)

    let len = function
      | Float a -> Array.length a
      | Int a -> Array.length a
      | String a -> Array.length a
  end

  let[@inline] f2j f = `Float f
  let[@inline] i2j x = `Int x
  let[@inline] s2j s = `String s

  let col1 c : t =
    `Array (
      match c with
      | Col.Float a -> Array.map f2j a
      | Col.Int a -> Array.map i2j a
      | Col.String a -> Array.map s2j a
    )

  let col2 n1 c1 n2 c2 : t =
    if Col.len c1 <> Col.len c2 then invalid_arg "col2: incompatible lengths";
    `Array (
      Array.init (Col.len c1)
        (fun i -> `Assoc [
             n1, Col.get_i_json c1 i;
             n2, Col.get_i_json c2 i;
           ])
    )

  let cols cols =
    match cols with
    | [] -> invalid_arg "Values.cols: empty list"
    | (_,c1) :: cols' ->
      let len = Col.len c1 in
      List.iter
        (fun (_,c) -> if Col.len c <> len then invalid_arg "cols: incompatible lengths")
        cols';
      `Array (
        Array.init len
          (fun i ->
             `Assoc (List.map (fun (n,c) -> n, Col.get_i_json c i) cols))
      )

  let custom j : t = `Obj j

  let to_json : t -> json = function
    | `String _ as j -> j
    | `Array a -> `List (Array.to_list a)
    | `Obj j -> j
end

(** Data to visualize *)
module Data = struct
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

  let url ?name ?format_ url : t = `Url {url; name; format_}
  let name s : t = `Name s
  let inline ?name ?format_ values : t =
    `Inline {name; format_; values}

  let to_json : t -> json = function
    | `Url u ->
      `Assoc [
        "url", `String u.url;
        "name", (match u.name with Some n -> `String n | None -> `Null);
        "format", (match u.format_ with Some f -> Data_format.to_json f | None -> `Null);
      ]
    | `Inline i ->
      `Assoc [
        "values", Values.to_json i.values;
        "name", (match i.name with Some n -> `String n | None -> `Null);
        "format", (match i.format_ with Some f -> Data_format.to_json f | None -> `Null);
      ]
    | `Name s -> `Assoc ["name", `String s]
end

module Mark = struct
  type t = [
    | `Bar
    | `Line
  ]

  let bar : t = `Bar
  let line : t = `Line

  let to_json : t -> json = function
    | `Bar -> `String "bar"
    | `Line -> `String "line"
end

module Encoding = struct
  type channel = [
    | `x | `y | `x2 | `y2
    | `xError | `yError | `xError2 | `yError2
    | `xOffset | `yOffset
    | `other of string
  ]

  let json_chan (c:channel) =
    `String (match c with
    | `other s -> s
    | `x -> "x" | `y -> "y" | `x2 -> "x2" | `y2 -> "y2"
    | `xError -> "xError" | `yError -> "yError"
    | `xError2 -> "xError2" | `yError2 -> "yError2"
    | `xOffset -> "xOffset" | `yOffset -> "yOffset")

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

  let json_field_type  : field_type -> json = function
    | `quantitative -> `String "quantitative"
    | `temporal -> `String "temporal"
    | `ordinal -> `String "ordinal"
    | `nominal -> `String "nominal"
    | `other j -> j

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

module Viz = struct
  type t = {
    data: Data.t;
    mark: Mark.t;
  }

  let make ~data ~mark () : t =
    { data; mark; }


  let to_json (self:t) : json =
    `Assoc [
      "$schema", `String "https://vega.github.io/schema/vega-lite/v5.json";
      "data", Data.to_json self.data;
      "mark", Mark.to_json self.mark;
    ]

  let to_json_str self = Yojson.Basic.pretty_to_string @@ to_json self
end

(* check compat with yojson *)
let () =
  let j : json = `Null in
  let _j2 : Yojson.Basic.t = j in
  ()
