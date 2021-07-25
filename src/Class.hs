module Class (MLog, MCache, MIOCache, MTrans, IBot, IUpdate, IAPI) where

-- Re-export all classes
import Interface.Cache.Class (MCache, MIOCache)
import Interface.Log.Class (MLog)
import Interface.MTrans (MTrans)
import Messenger.API.Class (IAPI)
import Messenger.Bot.Class (IBot)
import Messenger.Update.Class (IUpdate)