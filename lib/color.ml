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

let to_hsla (HSLA (h, s, l, a)) = {Hsla.h= clip_hue h; s; l; a}

let to_rgba' (HSLA (h, s, l, a)) =
  let h' = clip_hue h /. 60. in
  let chr = (1. -. abs_float ((2. *. l) -. 1.)) *. s in
  let m = l -. (chr /. 2.) in
  let x = chr *. (1. -. abs_float (mod_float h' 2. -. 1.)) in
  if h' < 1. then {Rgba'.r= chr +. m; g= x +. m; b= m; a}
  else if 1. <= h' && h' < 2. then {r= x +. m; g= chr +. m; b= m; a}
  else if 2. <= h' && h' < 3. then {r= m; g= chr +. m; b= x +. m; a}
  else if 3. <= h' && h' < 4. then {r= m; g= x +. m; b= chr +. m; a}
  else if 4. <= h' && h' < 5. then {r= x +. m; g= m; b= chr +. m; a}
  else {r= chr +. m; g= m; b= x +. m; a}

let to_rgba color =
  let c = to_rgba' color in
  let r = round (255. *. c.r) in
  let g = round (255. *. c.g) in
  let b = round (255. *. c.b) in
  {Rgba.r; g; b; a= c.a}
