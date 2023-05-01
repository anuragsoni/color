## color: converts between different color formats

Library that converts between different color formats. Right now it deals with RGB[A], HSL[A], OkLab, and Oklch formats. All those format convert to and from Gg.Color.t

The goal for this library is to provide easy handling of colors on the web, when working
with `js_of_ocaml`.

## Examples

```ocaml
# Color.to_hexstring (Color.Rgb.v 12 121 229 |> Color.Rgb.to_gg);;
- : "#0c79e5"
```

The library uses the color type from [Gg](https://github.com/dbuenzli/gg) as a bridge between other models.

```ocaml
# Color.Hsl.from_gg Gg.Color.red |> Color.Hsl.to_css ;;
- : string = "hsl(0.00, 100.00%, 50.00%)"

# Color.Rgb.from_gg Gg.Color.red |> Color.Rgb.to_css ;;
- : string = "rgb(255, 0, 0)"

# Color.to_hexstring (Color.complementary Gg.Color.red);;
- : string = "#00ffff"
```

## Credit

Based on [purescript-colors](https://github.com/sharkdp/purescript-colors)
