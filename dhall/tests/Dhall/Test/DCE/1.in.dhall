let x =
      { used = { keep = 1, drop = "unused" }
      , alsoUnused = { big = "tree" }
      }

in  x.used.keep
