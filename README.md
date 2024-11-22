## Color: conversions between different color formats

Color is a library that converts between different color formats.
Right now it deals with RGB[A], HSL[A], OkLab, and Oklch formats. All those format convert to and from Gg.Color.t (from [Gg](https://github.com/dbuenzli/gg)).

The goal for this library is to provide easy handling of colors on the web, when working
with `js_of_ocaml`.

## Examples

The library uses the color type from [Gg](https://github.com/dbuenzli/gg) as a bridge between other models.

```ocaml
# Color.Rgb.(v 12 121 229 |> to_gg) |> Color.to_hexstring ;;
- : "#0c79e5"
# Gg.Color.red |> Color.Hsl.from_gg |> Color.Hsl.to_css ;;
- : string = "hsl(0.00 100.00% 50.00%)"

# Gg.Color.red |> Color.Rgb.from_gg |> Color.Rgb.to_css ;;
- : string = "rgb(255 0 0)"

# Gg.Color.red |> Color.complementary |> Color.to_hexstring ;;
- : string = "#00a9db"
```

## Credit

Based on [purescript-colors](https://github.com/sharkdp/purescript-colors)
