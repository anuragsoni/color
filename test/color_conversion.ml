open Color

module TestColor = struct
  type t = gg

  let pp fmt t = Format.fprintf fmt "%s" (t |> Oklab.from_gg |> Oklab.to_css)

  let equal t1 t2 =
    let to_string t = t |> Oklab.from_gg |> Oklab.to_css in
    to_string t1 = to_string t2

  let col_test = Alcotest.testable pp equal
end

let hsl_rgb_values =
  [
    ((162.4, 0.779, 0.447), (25, 203, 151));
    ((0., 0., 1.), (255, 255, 255));
    ((0., 0., 0.5), (128, 128, 128));
    ((60., 1., 0.375), (191, 191, 0));
  ]

let test_hsl_vs_rgb () =
  List.iter
    (fun ((h, s, l), (r, g, b)) ->
      let hsl = Hsl.v h s l |> Hsl.to_gg in
      let rgb = Rgb.v r g b |> Rgb.to_gg in
      Alcotest.check TestColor.col_test "Can convert from hsl & rgb to Color.t"
        hsl rgb)
    hsl_rgb_values

let colors_rgb_hex =
  [
    (0, 191, 255, "#00bfff");
    (98, 121, 255, "#6279ff");
    (198, 167, 55, "#c6a737");
    (198, 17, 55, "#c61137");
    (252, 7, 16, "#fc0710");
    (18, 157, 126, "#129d7e");
    (98, 167, 255, "#62a7ff");
  ]

let test_color_to_hexstring () =
  let check (r, g, b, h) =
    let c = Rgb.v r g b in
    Alcotest.(check string)
      "Can convert color to hexstring" h
      (to_hexstring (Rgb.to_gg c))
  in
  List.iter check colors_rgb_hex

let test_hexstring_to_color () =
  let check (r, g, b, h) =
    match of_hexstring h with
    | Some c ->
        Alcotest.check TestColor.col_test "Can create color from hexstring"
          (Rgb.v r g b |> Rgb.to_gg)
          c
    | None -> Alcotest.fail "Couldn't convert hexstring to color"
  in
  List.iter check colors_rgb_hex

let oklch_rgb_values =
  [
    (* Black *)
    ((0.0, 0.0, 0.0), (0, 0, 0));
    (* White *)
    ((1.0, 0.0, 0.0), (255, 255, 255));
    (* Pastel pink *)
    ((0.7, 0.197, 348.35), (242, 97, 179));
    (* Pastel orange *)
    ((0.7118, 0.154, 45.53), (239, 127, 71));
    (* Plant green *)
    ((0.5471, 0.157, 145.06), (30, 135, 46));
    (* Salmon *)
    ((0.68, 0.138, 45.53), (221, 122, 74));
    (* Egg yellow *)
    ((0.84, 0.123, 80.47), (244, 194, 103));
  ]

let test_oklch_vs_rgb () =
  List.iter
    (fun ((l, c, h), (r, g, b)) ->
      let ok = Oklch.v l c h |> Oklch.to_gg in
      let rgb = Rgb.v r g b |> Rgb.to_gg in
      Alcotest.check TestColor.col_test
        "Can convert from oklch & rgb to Color.t" rgb ok)
    oklch_rgb_values

let test_oklch_roundtrip () =
  let open Oklch in
  List.iter
    (fun ((l, c, h), _) ->
      let ok = v l c h in
      let ok' = from_gg (to_gg ok) in
      Alcotest.(check string)
        "Can roundtrip oklch values" (to_css ok) (to_css ok'))
    oklch_rgb_values

let tests =
  [
    ("Hsl to RGBA'", `Quick, test_hsl_vs_rgb);
    ("Color to hexstring", `Quick, test_color_to_hexstring);
    ("Hexstring to color", `Quick, test_hexstring_to_color);
    ("Oklch vs RGB", `Quick, test_oklch_vs_rgb);
    ("Oklch roundtrip", `Quick, test_oklch_roundtrip);
  ]
