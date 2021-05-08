module Interface.MT 
-- ( module Interface.Class, --не очень хорошая идея так как лучше сущности импортировать квалифицированно или хотя бы с алиасами
--  module Cache, 
--  module Error, 
--  module Log ) 

where

--этот модуль может реэкспортировать все остальные классы!!!

--Our modules
import Interface.MCache as Cache
import Interface.MError as Error
import Interface.MLog as Log


class (MIOCache m, MIOError m, MLog m) => MT m