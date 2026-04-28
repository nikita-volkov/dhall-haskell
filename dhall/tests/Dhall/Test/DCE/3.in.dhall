let x =
      { keep = 1
      , drop =
          { nested = "unused"
          , also = 2
          }
      }

in  x.{ keep }
