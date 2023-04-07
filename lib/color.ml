let round x = int_of_float @@ Gg.Float.round x

let positive_float x y = mod_float (mod_float x y +. y) y

let deg_to_rad d = d *. Float.pi /. 180.
let rad_to_deg r = r *. 180. /. Float.pi

type t = Gg.color

module Hsla = struct
  type t = {h: float; s: float; l: float; a: float}
end

module Rgba' = struct
  type t = {r: float; g: float; b: float; a: float}
end

module Rgba = struct
  type t = {r: int; g: int; b: int; a: float}
end

module Oklab = struct
  type t = { l : float; a : float; b : float; alpha : float }
end

module Oklch = struct
  type t = { l : float; c : float; h : float; alpha : float }
end

let of_rgba r g b a = Gg.Color.v_srgbi ~a r g b |> Gg.Color.clamp

let of_rgb red green blue = of_rgba red green blue 1.

let of_rgba' r g b a = Gg.Color.v_srgb ~a r g b |> Gg.Color.clamp

let of_rgb' r g b = of_rgba' r g b 1.

let of_hsla h s l a =
  let clip_hue x = if 360.0 = x then x else positive_float x 360.0 in
  let norm_hue = clip_hue h /. 60. in
  let chr = (1. -. abs_float ((2. *. l) -. 1.)) *. s in
  let m = l -. (chr /. 2.) in
  let x = chr *. (1. -. abs_float (mod_float norm_hue 2. -. 1.)) in
  let make r g b =
    Gg.Color.v_srgb ~a (r +. m) (g +. m) (b +. m) |> Gg.Color.clamp
  in
  if norm_hue < 0. then make 0. 0. 0.
  else if norm_hue < 1. then make chr x 0.
  else if norm_hue < 2. then make x chr 0.
  else if norm_hue < 3. then make 0. chr x
  else if norm_hue < 4. then make 0. x chr
  else if norm_hue < 5. then make x 0. chr
  else if norm_hue < 6. then make chr 0. x
  else make 0. 0. 0.

let of_hsl h s l = of_hsla h s l 1.

let of_oklab ?(alpha = 1.0) l a b =
  (* From https://bottosson.github.io/posts/oklab/#converting-from-linear-srgb-to-oklab *)
  let l' = l +. (0.3963377774 *. a) +. (0.2158037573 *. b) in
  let m = l -. (0.1055613458 *. a) -. (0.0638541728 *. b) in
  let s = l -. (0.0894841775 *. a) -. (1.2914855480 *. b) in

  let l' = l' *. l' *. l' in
  let m = m *. m *. m in
  let s = s *. s *. s in

  let r = (4.0767416621 *. l') -. (3.3077115913 *. m) +. (0.2309699292 *. s) in
  let g = (-1.2684380046 *. l') +. (2.6097574011 *. m) -. (0.3413193965 *. s) in
  let b = (-0.0041960863 *. l') -. (0.7034186147 *. m) +. (1.7076147010 *. s) in
  Gg.Color.v r g b alpha |> Gg.Color.clamp

let of_oklch ?(alpha = 1.0) l c h =
  let h = deg_to_rad h in
  let a = c *. cos h in
  let b = c *. sin h in
  of_oklab ~alpha l a b

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

let to_rgba' t =
  let color = Gg.Color.to_srgb t in
  { Rgba'.r= Gg.Color.r color
  ; g= Gg.Color.g color
  ; b= Gg.Color.b color
  ; a= Gg.Color.a color }

let to_rgba color =
  let c = to_rgba' color in
  let r = round (255. *. c.r) in
  let g = round (255. *. c.g) in
  let b = round (255. *. c.b) in
  {Rgba.r; g; b; a= c.a}

let to_hsla t =
  let rgba = to_rgba t in
  let red, green, blue, alpha = (rgba.r, rgba.g, rgba.b, rgba.a) in
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
  {Hsla.h= hue; s= saturation; l= lightness; a= alpha}

let to_oklab t : Oklab.t =
  (* From https://bottosson.github.io/posts/oklab/#converting-from-linear-srgb-to-oklab *)
  let r, g, b, alpha =
    let open Gg.Color in
    (r t, g t, b t, a t)
  in

  let l = (0.4122214708 *. r) +. (0.5363325363 *. g) +. (0.0514459929 *. b) in
  let m = (0.2119034982 *. r) +. (0.6806995451 *. g) +. (0.1073969566 *. b) in
  let s = (0.0883024619 *. r) +. (0.2817188376 *. g) +. (0.6299787005 *. b) in

  let l = Float.cbrt l in
  let m = Float.cbrt m in
  let s = Float.cbrt s in

  {
    l = (0.2104542553 *. l) +. (0.7936177850 *. m) -. (0.0040720468 *. s);
    a = (1.9779984951 *. l) -. (2.4285922050 *. m) +. (0.4505937099 *. s);
    b = (0.0259040371 *. l) +. (0.7827717662 *. m) -. (0.8086757660 *. s);
    alpha;
  }

let to_oklch t : Oklch.t =
  let ok = to_oklab t in
  {
    l = ok.l;
    c = sqrt ((ok.a ** 2.0) +. (ok.b ** 2.0));
    h = rad_to_deg (Float.atan2 ok.b ok.a);
    alpha = ok.alpha;
  }

let to_hexstring color =
  let c = to_rgba color in
  let to_hex n =
    let repr = Printf.sprintf "%x" n in
    if String.length repr = 1 then "0" ^ repr else repr
  in
  "#" ^ to_hex c.r ^ to_hex c.g ^ to_hex c.b

let to_css_hsla t =
  let {Hsla.h; s; l; a} = to_hsla t in
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

let to_css_oklab t =
  let ok = to_oklab t in
  let pp_alpha () = function 1.0 -> "" | f -> Printf.sprintf " / %.2f" f in
  Printf.sprintf "oklab(%.2f %.2f %.2f%a)" ok.l ok.a ok.b pp_alpha ok.alpha

let to_css_oklch t =
  let ok = to_oklch t in
  let pp_alpha () = function 1.0 -> "" | f -> Printf.sprintf " / %.2f" f in
  Printf.sprintf "oklch(%.2f %.2f %.2fdeg%a)" ok.l ok.c ok.h pp_alpha ok.alpha

let black = Gg.Color.black

let white = Gg.Color.white

let gray_tone l = of_hsl 0. 0. l

let rotate_hue angle t =
  let {Hsla.h; s; l; a} = to_hsla t in
  of_hsla (h +. angle) s l a

let complementary = rotate_hue 180.

let lighten f t =
  let {Hsla.h; s; l; a} = to_hsla t in
  of_hsla h s (l +. f) a

let darken f = lighten (0. -. f)

let saturate f t =
  let {Hsla.h; s; l; a} = to_hsla t in
  of_hsla h (s +. f) l a

let desaturate f = saturate (0. -. f)

let brightness t =
  let {Rgba'.r; g; b; _} = to_rgba' t in
  ((299. *. r) +. (587. *. g) +. (114. *. b)) /. 1000.

let relative_luminance t =
  let {Rgba'.r; g; b; _} = to_rgba' t in
  let convert c =
    if c <= 0.03928 then c /. 12.92 else ((c +. 0.055) /. 1.055) ** 2.4
  in
  let r' = convert r in
  let g' = convert g in
  let b' = convert b in
  (0.2126 *. r') +. (0.7152 *. g') +. (0.0722 *. b')

let contrast_ratio t1 t2 =
  let l1 = relative_luminance t1 in
  let l2 = relative_luminance t2 in
  if l1 > l2 then (l1 +. 0.05) /. (l2 +. 0.05) else (l2 +. 0.05) /. (l1 +. 0.05)

let light t = brightness t > 0.5

let readable t1 t2 = contrast_ratio t1 t2 > 4.5

let text_color t = if light t then black else white
