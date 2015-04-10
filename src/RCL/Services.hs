-- module

module RCL.Services where



-- imports

import           RCL.Types



-- exported types

data Services m = Services {
  querant :: QueryURL -> m RawData
  }
