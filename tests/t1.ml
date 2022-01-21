
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
  let encoding = Encoding.(
      [field `x ~name:"x" ~type_:`quantitative ~title:"the xs are here" ();
       field `y ~name:"y" ~type_:`quantitative ~title:"y no float" ();
      ]
    )
  in
  let viz = Viz.make ~data ~mark ~encoding () in
  print_endline @@ Viz.to_json_str viz

