(lang dune 2.0)

(generate_opam_files true)

(source
 (github anuragsoni/color))

(package
 (name color)
 (synopsis "Convert between different color formats")
 (description
  "Library that converts between different color formats. Right now it deals with RGB[A], HSL[A], OkLab, and Oklch formats. All those format convert to and from [Gg.Color.t]")
 (authors "Anurag Soni <anuragsoni.13@gmail.com>")
 (maintainers
  "Anurag Soni <anuragsoni.13@gmail.com>"
  "Ambre Austen Suhamy <ambre@tarides.com>")
 (license MIT)
 (depends
  (ocaml
   (>= 4.05.0))
  dune
  gg
  (alcotest :with-test)))
