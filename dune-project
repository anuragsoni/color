(lang dune 2.0)

(name color)

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
   (>= 4.08.0))
  (gg
   (>= 1.0.0))
  (alcotest :with-test)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    (= 0.26.2)
    :with-dev-setup))))
