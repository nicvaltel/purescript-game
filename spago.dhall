{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "purescript-game-engine"
, dependencies = [ 
    "console", 
    "effect", 
    "prelude", 
    "partial", 
    "canvas", 
    "maybe", 
    -- "numbers",
    "foldable-traversable",
    -- "integers",
    -- "lists",
    -- "psci-support",
    "quickcheck",
    -- "random",
    "signal",
    -- "transformers",
    "tuples",
    "arrays",
    -- "web-dom",
    -- "web-html",
    -- "web-events",
    "refs",
    "aff",
    "unsafe-coerce",
    "either",
    "exceptions",
    -- "fetch",
    "concurrent-queues",
    "affjax",
    "affjax-web",
    "http-methods",
    "datetime",
    "argonaut-core",
    "argonaut-codecs",
    -- "newtype",
    "ordered-collections",
    "now",
    "numbers",
     ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
