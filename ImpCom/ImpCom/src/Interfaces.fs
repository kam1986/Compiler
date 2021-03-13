module Interfaces

type ILanguage<'high,'low> =
   abstract member Interpret : 'stack -> 'low // 

/// Interpret the language under the memory model   
let Interpret memorymodel (language : ILanguage<_,_>) =  language.Interpret memorymodel
