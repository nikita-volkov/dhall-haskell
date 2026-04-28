let deps =
      { sdk =
          { module = 1
          , Fixtures = { dead = 2 }
          }
      }

let sdk = deps.sdk

in  sdk.module
