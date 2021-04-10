{-# LANGUAGE NamedFieldPuns #-}

data UnnamedConfiguration
  = UnnamedConfiguration
      String
      String
      String
      Bool
      Bool
      String
      String
      Integer
  deriving (Eq, Show)

getUnnamedUserName (UnnamedConfiguration un _ _ _ _ _ _ _) = un

getUnnamedLocalHost (UnnamedConfiguration _ lh _ _ _ _ _ _) = lh

getUnnamedRemoteHost (UnnamedConfiguration _ _ rh _ _ _ _ _) = rh

getUnnamedIsGuest (UnnamedConfiguration _ _ _ ig _ _ _ _) = ig

-- and so on

data Configuration = Configuration
  { username :: String,
    localhost :: String,
    remoteHost :: String,
    isGuest :: Bool,
    isSuperUser :: Bool,
    currentDir :: String,
    homeDir :: String,
    timeConnected :: Integer
  }

directoryExists _ = True

changeDir :: Configuration -> String -> Configuration
changeDir cfg newDir =
  if directoryExists newDir
    then cfg {currentDir = newDir}
    else error "directory does not exist"

postWorkingDir :: Configuration -> String
postWorkingDir = currentDir

getUserName (Configuration un _ _ _ _ _ _ _) = un

getHostData Configuration {localhost = lh, remoteHost = rh} = (lh, rh)

getHostDataBetter Configuration {localhost, remoteHost} = (localhost, remoteHost)

initCFG =
  Configuration "nobody" "nowhere" "nowhere" False False "/" "/" 0

initCFG' =
  Configuration
    { username = "nobody",
      localhost = "nowhere",
      remoteHost = "nowhere",
      isGuest = False,
      isSuperUser = False,
      currentDir = "/",
      homeDir = "/",
      timeConnected = 0
    }
