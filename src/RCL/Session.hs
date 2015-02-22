-- module

module RCL.Session where



-- exported types

type APIKey = String
type Secret = String
type Frob   = String
type Token  = String

data Session = Session {
  apiKey :: APIKey,
  secret :: Secret,
  token  :: Token,
  frob   :: Frob
  }
