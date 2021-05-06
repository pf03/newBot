{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

--importPriority = 60
module App where
--import Control.Monad.State.Lazy

import Types  --100



--здесь только те классы, которые используются для разделения логики вк и телеграм, этот модуль импортируется квалифицированнно
--другие классы в одноименном модуле

--достаточно одного типа для определения полного экземпляра
--нужно быть осторожным, чтобы типы не были одинаковыми!!
class (Update update) => Main pointer init update  | pointer -> init , init -> update, update -> pointer where 
  --общий интерфейс
  getInit :: pointer -> T init
  getUpdateId :: init -> UpdateId
  setUpdateId :: init -> UpdateId -> init
  getUpdates:: init -> T ([update], init)
  sendMessage:: update -> [Label] -> T ()
   --getEntity :: update -> Entity
   
--универсальные функции для работы с обновлениями, типа линз
class Update update where 
  setMessage :: update -> Message -> update
  getMessage :: update -> Maybe Message
  getCommand :: update -> Maybe Command
  getChatId :: update -> ChatId

--скорей нужно переименовать в ClassTypes или Common

class API api where 
  apiName :: api -> String 
  getPath :: Token -> api -> Path













