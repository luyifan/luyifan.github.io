---
layout: post 
category: haskell 
tags: [ read , haskell ]
---
{% include JB/setup %}
#Real World Haskell函数式编程
{% highlight haskell %}
{% endhighlight %}
###中缀函数
* 如果一个**`函数`**或**`构造器`**带两个或更多的参数，我们可以选择使用中缀形式，即我们把函数（名称）放在它的第一个和第二个参数之间。这允许我们把函数作为中缀操作符来使用。
{% highlight haskell %}
-- file: ch04/Plus.hs
a `plus` b = a + b
data a `Pair` b = a `Pair` b
                  deriving (Show)
-- we can use the constructor either prefix or infix
foo = Pair 1 2
bar = True `Pair` "quux"
{% endhighlight %}

###List
* 一个列表不会明确地存储它本身的长度。因此length函数的工作方式是遍历整个列表。
* 只关心一个列表是不是为空时，调用length不是一个好的策略,选择调用null函数
* any和all
{% highlight haskell %}
ghci> :type all
all :: (a -> Bool) -> [a] -> Bool
ghci> all odd [1,3,5]
True
ghci> all odd [3,1,4,1,5,9,2,6,5]
False
ghci> all odd []
True
ghci> :type any
any :: (a -> Bool) -> [a] -> Bool
ghci> any even [3,1,4,1,5,9,2,6,5]
True
ghci> any even []
False
{% endhighlight %}

* break提取列表中使谓词失败的元素组成二元组的首项，而span提取列表中使谓词成功的元素组成二元组的首项。
{% highlight haskell %}
ghci> :type span
span :: (a -> Bool) -> [a] -> ([a], [a])
ghci> span even [2,4,6,7,9,10,11]
([2,4,6],[7,9,10,11])
ghci> :type break
break :: (a -> Bool) -> [a] -> ([a], [a])
ghci> break even [1,3,5,6,8,9,10]
([1,3,5],[6,8,9,10])
{% endhighlight %}

* filter函数带着一个谓词，返回列表中使谓词成功的每一个元素。
{% highlight haskell %}
ghci> :type filter
filter :: (a -> Bool) -> [a] -> [a]
ghci> filter odd [2,4,1,3,6,8,5,7]
[1,3,5,7]
{% endhighlight %}
* 一次性处理多个列表，zip函数把两个列表压缩成一个单一的由二元组组成的列表。结果列表和被处理的两个列表中较短的那个等长。（译注：言下之意是较长的那个列表中的多出来的元素会被丢弃）
{% highlight haskell %}
ghci> :type zip
zip :: [a] -> [b] -> [(a, b)]
ghci> zip [12,72,93] "zippity"
[(12,'z'),(72,'i'),(93,'p')]
{% endhighlight %}
更有用的是zipWith函数，它带两个列表作为参数并为从每个列表中抽取一个元素而组成的二元组提供一个函数，最后生成与较短的那个列表等长的新列表。
{% highlight haskell %}
ghci> :type zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
ghci> zipWith (+) [1,2,3] [4,5,6]
[5,7,9]
{% endhighlight %}
如果想把三个列表压缩在一起，要调用zip3或者zipWith3，可以类推到zip7和zipWith7


###显示递归
1. 尾递归函数
  * 如果输入非空，这个函数做的最后一件事，就是递归地调用自身  
2. 结构递归（structural recursion）
  * 非递归情形（列表为空）被称为“基本情形”（有时也叫终止情形）。当对函数进行递归调用时，计算最终会回到基本情形上。在数学上，这也称为“归纳情形”。

###其他循环表示方式(库函数)
1. map 
{% highlight haskell %}
Prelude> :type map
map :: (a -> b) -> [a] -> [b]
{% endhighlight %}

{% highlight haskell %}
-- file: ch04/rewrite_by_map.hs

import Data.Char (toUpper)

square2 xs = map squareOne xs
    where squareOne x = x * x

upperCase2 xs = map toUpper xs
{% endhighlight %}

2. fold折叠
* `折叠（fold）操作处于（完全通用化的）尾递归和（只做一件事的）列表处理函数之间的中间地带`
* 它对一个列表中的所有元素做某种处理，并且一边处理元素，一边更新累积器，最后在处理完整个列表之后，返回累积器的值。
* 左折叠foldl	
{% highlight haskell %}
-- file: ch04/foldl.hs
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl step zero (x:xs) = foldl step (step zero x) xs
foldl _ zero []        = zero
{% endhighlight %}
foldl 函数接受一个步骤（step）函数，一个累积器的初始化值，以及一个列表作为参数。步骤函数每次使用累积器和列表中的一个元素作为参数，并计算出新的累积器值，这个过程称为步进（stepper）。然后，将新的累积器作为参数，再次进行同样的计算，直到整个列表处理完为止。
{% highlight haskell %}
-- file: ch04/niceSum.hs
niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs
{% endhighlight %}
惰性求值
{% highlight haskell %}
niceSum [1, 2, 3]
    == foldl (+) 0                   (1:2:3:[])
    == foldl (+) (0 + 1)             (2:3:[])
    == foldl (+) ((0 + 1) + 2)       (3:[])
    == foldl (+) (((0 + 1) + 2) + 3) []
    == (((0 + 1) + 2) + 3)
{% endhighlight %}
* 右折叠foldr
{% highlight haskell %}
-- file: ch04/foldr.hs
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr step zero (x:xs) = step x (foldr step zero xs)
foldr _ zero []        = zero
{% endhighlight %}

{% highlight haskell %}
-- file: ch04/niceSumFoldr.hs
niceSumFoldr :: [Int] -> Int
niceSumFoldr xs = foldr (+) 0 xs
{% endhighlight %}
惰性求值，和foldl区别
{% highlight haskell %}
niceSumFoldr [1, 2, 3]
    == foldr (+) 0 (1:2:3[])
    == 1 +           foldr (+) 0 (2:3:[])
    == 1 + (2 +      foldr (+) 0 (3:[]))
    == 1 + (2 + (3 + foldr (+) 0 []))
    == 1 + (2 + (3 + 0))
{% endhighlight %}

* filter 还可以通过 foldr 来定义：
{% highlight haskell %}
-- file: ch04/myFilter.hs
myFilter p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys
{% endhighlight %}
所有可以用 foldr 定义的函数，统称为主递归（primitive recursive）。很大一部分列表处理函数都是主递归函数。比如说， map 就可以用 foldr 定义

####<font color="red">foldl也可以用foldr表<font>
{% highlight haskell %}
-- file: myFoldl.hs
myFoldl f zero xs = (foldr step id xs) zero
	where step x g a = g ( f a x )
-- myFoldl (+) 0 [1, 2] =
--
-- (foldr step id [1, 2]) 0
-- step 1 (foldr step id [2]) 0
-- step 1 (step 2 (foldr step id [])) 0
-- step 1 (step 2 id) 0
-- (step 2 id) ((+) 0 1)
-- id ((+) ((+) 0 1) 2)
-- 3
{% endhighlight %}

##左折叠、惰性和内存泄漏
{% highlight haskell %}
foldl (+) 0 (1:2:3:[])
          == foldl (+) (0 + 1)             (2:3:[])
          == foldl (+) ((0 + 1) + 2)       (3:[])
          == foldl (+) (((0 + 1) + 2) + 3) []
          ==           (((0 + 1) + 2) + 3)
{% endhighlight %}
除非被显式地要求，否则最后的表达式不会被求值。在表达式被求值之前，它会被保存在块里面。对块中表达式的求值在一个`内部栈`中进行。使用栈来执行一些可能无限大的操作，是一种常见优化和保护技术。这种用法减少了操作系统显式的上下文切换，而且就算计算量超出栈可以容纳的范围，那么最坏的结果就是栈崩溃，而如果直接使用系统内存，一旦请求超出内存可以容纳的范围，可能会造成整个程序崩溃，甚至影响系统的稳定性。
对于一个更大的表达式,尽管它并不是真的非常庞大， foldl 的执行会失败
{% highlight haskell %}
ghci> foldl (+) 0 [1..1000000]
*** Exception: stack overflow
{% endhighlight %}
对于小的表达式来说， foldl 可以给出正确的答案，但是，因为过度的块资源占用，它运行非常缓慢。我们称这种现象为`内存泄漏(space leak)`：代码可以正确地执行，但它占用了比实际所需多得多的内存。
Data.List模块定义了一个 `foldl'` 函数，它和 foldl 的作用类似，唯一的区别是， foldl' 并不创建块

###匿名（lambda）函数
1. 在 Haskell 中，匿名函数以反斜杠符号 \ 为开始，后跟函数的参数（可以包含模式），而函数体定义在 -> 符号之后。其中， \ 符号读作 lambda 。
*  lambda 函数的定义只能有`一条语句`
{% highlight haskell %}
-- file: ch04/isInAny2.hs
import Data.List (isInfixOf)
isInAny2 needle haystack = any (\s -> needle `isInfixOf` s) haystack
{% endhighlight %}

###部分函数应用和柯里化
 * -> 只有一种作用：它表示一个函数接受一个参数，并返回一个值。在 Haskell 中，**`所有函数都只接受一个参数`**。dropWhile :: (a -> Bool) -> [a] -> [a]看上去一个接受两个参数的函数，但实际上它是一个接受一个参数的函数，而这个函数的返回值是另一个`函数`，这个被返回的函数也只接受一个参数。
 * 传入参数的数量，少于函数所能接受参数的数量，这种情况被称为`函数的部分应用（partial application of the function)`,部分函数应用被称为`柯里化（currying）`
{% highlight haskell %}
-- file: ch04/isInAny3.hs
import Data.List (isInfixOf)
isInAny3 needle haystack = any (isInfixOf needle) haystack
{% endhighlight %}
 * Haskell 提供了一种方便的符号快捷方式，用于对中序函数进行部分应用：使用括号包围一个操作符，通过在括号里面提供左操作对象或者右操作对象，可以产生一个部分应用函数。这种类型的部分函数应用称之为`节（section）`
{% highlight haskell %}
Prelude> (1+) 2
3
Prelude> map (*3) [24, 36]
[72,108]
Prelude> map (2^) [3, 5, 7, 9]
[8,32,128,512]
{% endhighlight %}
###As-模式
* 模式 xs@(_:xs') 被称为 as-模式，它的意思是：如果输入值能匹配 `@` 符号右边的模式（这里是 (_:xs') ），那么就将这个值绑定到 @ 符号左边的变量中（这里是 xs ）。
* ， as-模式还有其他作用：它可以对`输入数据进行共享`，而不是复制它。在 noAsPattern 函数的定义中，当 (x:xs) 匹配时，在函数体里需要复制一个 (x:xs) 的副本。这个动作会引起内存分配。虽然这个分配动作可能很廉价，但它并不是免费的。当使用 suffixes 函数时，我们通过变量 xs 重用匹配了 as-模式的输入值，因此就避免了内存分配。
{% highlight haskell %}
-- file: ch04/suffixes.hs
suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes [] = []
-- file: noAsPattern.hs
noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) : noAsPattern xs
noAsPattern [] = []
{% endhighlight %}
###组合函数
* (.) 操作符就可以组合起两个函数：
* (.) 操作符是`右关联`的
{% highlight haskell %}
*Main Data.List> :type (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
{% endhighlight %}

###内存泄漏和严格求值
1. 通过 seq 函数避免内存泄漏
* 非惰性求值的表达式为严格的（strict）,特殊的 seq 函数来绕过 Haskell 默认的非严格求值：
{% highlight haskell %}
Prelude> :type seq
seq :: a -> b -> b
{% endhighlight %}
seq函数,它强迫（force）求值传入的第一个参数，然后返回它的第二个参数。
* 正确地产生 seq 的作用，表达式中`被求值的第一个`必须是 seq ：
{% highlight haskell %}
-- 错误：因为表达式中第一个被求值的是 someFunc 而不是 seq
-- 所以 seq 的调用被隐藏了在 someFunc 调用之下
hiddenInside x y = someFunc (x `seq` y)

-- 错误：原因和上面一样
hiddenByLet x y z = let a = x `seq` someFunc y
                    in anotherFunc a z

-- 正确： seq 被第一个求值，并且 x 被强迫求值
onTheOutside x y = x `seq` someFunc y
{% endhighlight %}
* 一个`常见错误`是，将 seq 用在没有关联的两个表达式上面：
{% highlight haskell %}
badExpression step zero (x:xs) =
    seq (step zero x)
            (badExpression step (step zero x) xs)
{% endhighlight %}
step zero x 分别出现在 seq 的第一个参数和 badExpression 的表达式内， seq 只会对第一个 step zero x 求值，而它的结果并不会影响 badExpression 表达式内的 step zero x 。正确的用法应该是用一个 let 结果保存起 step zero x 表达式，然后将它分别传给 seq 和 badExpression

* seq 在遇到像数字这样的值时，它会对值进行求值，但是，一旦 seq 碰到构造器，比如 (:) 或者 (,) ，那么 seq 的求值就会停止。举个例子，如果将 (1+2):[] 传给 seq 作为它的第一个参数，那么 seq 不会对这个表达式进行求值,seq 并不会对 (1+2) 求值，而是在碰到 (1+2):[] 时就直接停止求值。这一表现可能的原因如下：虽然 : 是中序操作符，但它实际上只是函数 (:) ，而 Haskell 的函数总是前序的，因此 (1+2):[] 实际上应该表示为 (:) (1+2) [] ，`seq 在碰到构造器时就会停止求值`
* seq 的使用并不是无成本的，知道这一点很重要：它需要在运行时检查输入值是否已经被求值。必须谨慎使用 seq 。比如说，上面定义的 strictPair ，尽管它能顺利对元组进行强制求值，但它在求值元组所需的计算量上，加上了一次模式匹配、两次 seq 调用和一次构造新元组的计算量。如果我们检测这个函数的性能的话，就会发现它降低了程序的处理速度。
{% highlight haskell %}
strictPair (a,b) = a `seq` b `seq` (a,b)
strictList (x:xs) = x `seq` x : strictList xs
strictList []     = [
{% endhighlight %}
* 即使不考虑性能的问题， seq 也不是处理内存泄漏的万能药