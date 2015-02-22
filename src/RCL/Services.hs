-- module

module RCL.Services where



-- imports

import RCL.Query
import RCL.Types



-- exported types

data Services m = Services {
  querant :: QueryURL -> m RawData
  }
