let round_nearest_lb = ~-.(2. ** 52.)

let round_nearest_ub = 2. ** 52.

let round_nearest t =
  if t >= round_nearest_lb && t <= round_nearest_ub then
    floor (t +. 0.49999999999999994)
  else t

let round x = int_of_float @@ round_nearest x

type hue = UnclippedHue of float

type t = HSLA of hue * float * float * float

module Hsla = struct
  type t = {h: float; s: float; l: float; a: float}
end

module Rgba' = struct
  type t = {r: float; g: float; b: float; a: float}
end

module Rgba = struct
  type t = {r: int; g: int; b: int; a: float}
end

let positive_float x y = mod_float (mod_float x y +. y) y

let clip_hue (UnclippedHue x) = if 360.0 = x then x else positive_float x 360.0

let clamp low hi x = min hi (max low x)

let of_rgba red' green' blue' alpha =
  let red = clamp 0 255 red' in
  let blue = clamp 0 255 blue' in
  let green = clamp 0 255 green' in
  let r = float_of_int red /. 255. in
  let g = float_of_int green /. 255. in
  let b = float_of_int blue /. 255. in
  let c_max = max (max red green) blue in
  let c_min = min (min red green) blue in
  let c = c_max - c_min in
  let c' = float_of_int c /. 255. in
  let hue' c =
    if c = 0 then 0.
    else if c_max = red then positive_float ((g -. b) /. c') 6.
    else if c_max = green then ((b -. r) /. c') +. 2.
    else ((r -. g) /. c') +. 4.
  in
  let hue = 60. *. hue' c in
  let lightness = float_of_int (c_max + c_min) /. (255. *. 2.) in
  let saturation =
    if c = 0 then 0. else c' /. (1. -. abs_float ((2. *. lightness) -. 1.))
  in
  HSLA (UnclippedHue hue, saturation, lightness, alpha)

let of_rgb red green blue = of_rgba red green blue 1.

let of_rgba' r g b a =
  of_rgba
    (round @@ (r *. 255.))
    (round @@ (g *. 255.))
    (round @@ (b *. 255.))
    a

let of_rgb' r g b = of_rgba' r g b 1.

let of_hsla h s l a =
  let saturation = clamp 0. 1. s in
  let lightness = clamp 0. 1. l in
  let alpha = clamp 0. 1. a in
  HSLA (UnclippedHue h, saturation, lightness, alpha)

let of_hsl h s l = of_hsla h s l 1.

let of_hexstring s =
  if String.length s = 4 || String.length s = 7 then
    let short = String.length s = 4 in
    let r' = if short then String.sub s 1 1 else String.sub s 1 2 in
    let g' = if short then String.sub s 2 1 else String.sub s 3 2 in
    let b' = if short then String.sub s 3 1 else String.sub s 5 2 in
    let r = int_of_string_opt ("0x" ^ r') in
    let g = int_of_string_opt ("0x" ^ g') in
    let b = int_of_string_opt ("0x" ^ b') in
    match (r, g, b) with
    | Some r, Some g, Some b ->
        if short then
          Some (of_rgb ((16 * r) + r) ((16 * g) + g) ((16 * b) + b))
        else Some (of_rgb r g b)
    | _ -> None
  else None

let to_hsla (HSLA (h, s, l, a)) = {Hsla.h= clip_hue h; s; l; a}

let to_rgba' (HSLA (h, s, l, a)) =
  let norm_hue = clip_hue h /. 60. in
  let chr = (1. -. abs_float ((2. *. l) -. 1.)) *. s in
  let m = l -. (chr /. 2.) in
  let x = chr *. (1. -. abs_float (mod_float norm_hue 2. -. 1.)) in
  let make r g b = {Rgba'.r= r +. m; g= g +. m; b= b +. m; a} in
  if norm_hue < 0. then make 0. 0. 0.
  else if norm_hue < 1. then make chr x 0.
  else if norm_hue < 2. then make x chr 0.
  else if norm_hue < 3. then make 0. chr x
  else if norm_hue < 4. then make 0. x chr
  else if norm_hue < 5. then make x 0. chr
  else if norm_hue < 6. then make chr 0. x
  else make 0. 0. 0.

let to_rgba color =
  let c = to_rgba' color in
  let r = round (255. *. c.r) in
  let g = round (255. *. c.g) in
  let b = round (255. *. c.b) in
  {Rgba.r; g; b; a= c.a}

let to_hexstring color =
  let c = to_rgba color in
  let to_hex n =
    let repr = Printf.sprintf "%x" n in
    if String.length repr = 1 then "0" ^ repr else repr
  in
  "#" ^ to_hex c.r ^ to_hex c.g ^ to_hex c.b

let to_css_hsla (HSLA (UnclippedHue h, s, l, a)) =
  if a = 1. then
    Printf.sprintf "hsl(%.2f, %.2f%s, %.2f%s)" h (s *. 100.) "%" (l *. 100.)
      "%"
  else
    Printf.sprintf "hsla(%.2f, %.2f%s, %.2f%s, %.2f)" h (s *. 100.) "%"
      (l *. 100.) "%" a

let to_css_rgba color =
  let color' = to_rgba color in
  if color'.a = 1. then
    Printf.sprintf "rgb(%d, %d, %d)" color'.r color'.g color'.b
  else
    Printf.sprintf "rgba(%d, %d, %d, %.2f)" color'.r color'.g color'.b color'.a

let black = of_hsl 0. 0. 0.

let white = of_hsl 0. 0. 1.

let gray_tone l = of_hsl 0. 0. l

let rotate_hue angle (HSLA (UnclippedHue h, s, l, a)) =
  of_hsla (h +. angle) s l a

let complementary = rotate_hue 180.

let lighten f (HSLA (UnclippedHue h, s, l, a)) = of_hsla h s (l +. f) a

let darken f = lighten (0. -. f)

let saturate f (HSLA (UnclippedHue h, s, l, a)) = of_hsla h (s +. f) l a

let desaturate f = saturate (0. -. f)
