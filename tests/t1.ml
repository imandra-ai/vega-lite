
open Vega_lite

let () =
  let data =
    Data.inline ~name:"d1" @@
    Values.(
      col2
        "x" Col.(ints @@ Array.init 20 (fun i -> i))
        "y" Col.(floats @@ Array.init 20 (fun i -> float (i mod 3) +. 0.2))
    )
  in
  let mark = Mark.line in
  let viz = Viz.make ~data ~mark () in
  print_endline @@ Viz.to_json_str viz

