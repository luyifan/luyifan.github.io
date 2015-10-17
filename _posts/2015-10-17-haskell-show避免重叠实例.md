---
layout: post 
category: haskell
tags: [ read , haskell ]
---
{% include JB/setup %}
#Haskell
* OverlappingInstances 和 TypeSynonymInstances 语言扩展是 GHC 特有的，Haskell 98 并不支持。 然而，Haskell 98 中的 Show 类型类在转化 Char 列表和 Int 列表时却用了不同的方法。它用了一个聪明但简单的小技巧。
* Show 类型类定义了转换单个值的 show 方法和转换列表的 showList 方法。 showList 默认使用中括号和逗号转换列表。
* [a] 的 Show 实例使用 showList 实现。Char 的 Show 实例提供了一个特殊的 showList 实现， 它使用双引号，并转义非 ASCII 打印字符。
* 结果是，如果有人想对 [Char] 应用 show，编译器会选择 showList 的实现，并使用双引号正确转换这个字符串。这样，换个角度看问题，我们就能避免 OverlappingInstances 扩展了。

{% highlight haskell %}
-- file: ShowOverlapSolved.hs
import Data.List ( intercalate )
class MyShow a where 
	myShow :: a -> String
	myShowList :: [a] -> String
	-- default implementation
	myShowList xs = "[" ++ ( intercalate "," $ map myShow xs ) ++ "]"

instance MyShow Char where 
	myShow c = "'" ++ [c] ++ "'"
	-- overriding the default
	myShowList xs = "\"" ++ xs ++ "\""

instance MyShow Int where 
	myShow  = show 
	-- use the default myShowList

instance (MyShow a) =>  MyShow [a] where
	myShow = myShowList 
-- *Main Data.Ratio> myShow '1'
-- "'1'"
-- *Main Data.Ratio> myShow "123"
-- "\"123\""
-- *Main Data.Ratio> (myShow (123::Int))
-- "123"
-- 这里主要要解决show [Char] 和 show [Int]是不同的，show "123" -> "\"123\""
-- 而show [1,2,3] -> "[1,2,3]" 使用括号包围，而不是双引号
-- 所有需要采用不同的处理方式，而又不能定义两个类型类实例，所以一个采用默认的，一个采用重载的处理
myPrint :: (MyShow a) => a -> IO ()
myPrint = putStrLn . myShow

main :: IO ()
main = do
		myPrint (1 :: Int)
		myPrint ([1,2,3] :: [Int])
		myPrint 'x'
		myPrint ['x', 'y', 'z']
		myPrint "xyz"

{% endhighlight %}