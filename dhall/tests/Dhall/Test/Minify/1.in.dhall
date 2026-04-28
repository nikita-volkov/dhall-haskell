let Deps =
      { Sdk = { module = 1 }
      , Fixtures = { dead = 2 }
      }

let Sdk = Deps.Sdk

in  Sdk.module