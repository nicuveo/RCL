-- module

module RCL.Config where



-- imports

import           RCL.Types



-- exported types

data Config = Config {
  restURL :: URL,
  authURL :: URL,
  apiKey  :: APIKey,
  secret  :: Secret
  }
