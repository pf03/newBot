module Interface.Class 
-- ( module Interface.Class, --не очень хорошая идея так как лучше сущности импортировать квалифицированно или хотя бы с алиасами
--  module Cache, 
--  module Error, 
--  module Log ) 

where

--этот модуль может реэкспортировать все остальные классы!!!

--Our modules
import Interface.Cache as Cache
import Interface.Error as Error
import Interface.Log as Log


class (MIOCache m, MIOError m, MLog m) => MT m