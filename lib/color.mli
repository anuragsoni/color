(** This module deals with colors. *)

type gg = Gg.color
(** Standard representation of a color. *)

module type ColorRep = sig
  type t
  type param

  val v : ?alpha:float -> param -> param -> param -> t
  val to_gg : t -> gg
  val from_gg : gg -> t
  val to_css : t -> string
end

module Rgb : ColorRep with type param = int
(** Classic RedGreenBlue (& optional alpha) representation, closely matching the way screens display colors. 
    - r, g, and b are integers, between 0 and 255
    - alpha is the transparency, between 0.0 (full transparency) and 1.0 (full opacity, default) *)

module Rgb_float : ColorRep with type param = float
(** Same as [Rgb], but with float values.
    - r, g, and b are floats between 0.0 and 1.0
    - alpha is the transparency, between 0.0 (full transparency) and 1.0 (full opacity, default) *)

module Hsl : ColorRep with type param = float
(** Hue, Saturation, Lightness. More intuitive to work with than [Rgb].
    - hue is in degrees, a float value between 0.0 and 360.0
    - saturation, lightness and alpha are float values between 0.0 and 1.0 *)

module Oklab : ColorRep with type param = float
(** A color space that is ok. See {{: https://bottosson.github.io/posts/oklab/} the author's blog} for more info.
    - lightness is a float between 0.0 and 1.0
    - a and b are floats values (theoretically unbounded but in practice do not exceed ~±0.5)
    - alpha is the transparency, between 0.0 (full transparency) and 1.0 (full opacity, default) *)

module Oklch : ColorRep with type param = float
(** The polar version of [Oklab]. See {{: https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/oklch} mozilla's page on it} for more info.
    Arguably the easiest space to work with, as it closely match our perception of colors.
    - lightness is a float between 0.0 and 1.0
    - c is chroma, minimum is 0.0 and practical maximum value is ~0.5
    - h is hue in degrees, a float value between 0.0 and 360.0
    - alpha is the transparency, between 0.0 (full transparency) and 1.0 (full opacity, default) *)

val of_hexstring : string -> gg option
(** Parse a hexadecimal color code. Handles short format like [#rgb] or
    long format [#rrggbb]. Short format [#abc] corresponds to long format
    [#aabbcc]. *)

val to_hexstring : gg -> string
(** Converts a color to its hexadecimal representation. The alpha channel
    is not represented. *)

val black : gg
(** Pure black *)

val white : gg
(** Pure white *)

val gray_tone : float -> gg
(** Creates a gray tone from light values (0.0 -> black, 1.0 -> white) *)

val rotate_hue : float -> gg -> gg
(** Rotates the hue of a Color by some angle (in degrees) *)

val complementary : gg -> gg
(** Gets complementary color by rotating hue by 180° *)

val lighten : float -> gg -> gg
(** Lightens a color by adding an amount to the lightness channel *)

val darken : float -> gg -> gg
(** Darkens a color by subtracting an amount to the lightness channel *)

val intensify : float -> gg -> gg
(** Increases the chroma of a color *)

val desintensify : float -> gg -> gg
(** Decreases the chroma of a color *)

val lightness : gg -> float
(** The perceived lightness of any color, with 0. for the darkest black and 1. for the lightest white. *)

val contrast_ratio : gg -> gg -> float
(** Contrast ratio between two colors. It is a value that
    can range from 1. to 21. {: https://www.w3.org/TR/2008/REC-WCAG20-20081211/#contrast-ratiodef} W3 reference} *)

val is_light : gg -> bool
(** Checks whether a color is perceived as a light color. *)

val readable : gg -> gg -> bool
(** Checks if text of one color is readable on a background
    of the second color. A minimum contrast ratio of 4.5 is
    recommended to ensure that text is readable on a given
    background. {{: https://www.w3.org/TR/WCAG20-TECHS/G18.html} W3 reference} *)

val text_color : gg -> gg
(** Returns a readable foreground text color
    (picks between black or white) for a given background color *)

val random : ?alpha:float -> ?light:float -> ?chroma:float -> unit -> gg
(** Picks a color with a random hue.
    Uses a default "full-light" chroma and lightness, and full opacity.*)
