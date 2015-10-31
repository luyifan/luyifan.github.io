---
layout: post 
category: haskell 
tags: [ read , haskell ]
---
{% include JB/setup %}
#Real World Haskell Chapter1-3

###Haskell类型系统
* 强类型
* 静态类型（在编译期知道类型）
* 类型推导

###Haskell纯度
* 将不纯函数和纯函数隔离

###Haskell多态
* 参数多态 List :: [a]
* 没有子类多态
* 没有强制多态（允许值在类型之间进行隐式的转换）


###Haskell惰性求值
1. Haskell的变量，只是用来作为表达式的代替，不能变化值
2. nonstrict evaluation 非严格求值（惰性求值)
	+ isOdd ( 1+2) 不会先求值为3，而是保证如果需要的话，是可以计算出来。
3. Haskell 函数返回的结果可能是一个块（被延迟计算的表达式）

###Haskell类型定义
{% highlight haskell %}
data BookInfo = Book Int String [String]
                deriving (Show)
{% endhighlight %}

* BoookInfo是类型构造器
* Book是值构造器
* 类型构造器只能出现在类型的定义，或者类型签名当中。而值构造器只能出现在实际的代码中
* 类型别名 
{% highlight haskell %}
type CustomerID = Int
{% endhighlight %}
类型别名只是为已有类型提供了一个新名字，创建值的工作还是由原来类型的值构造器进行

* 记录语法:我们在定义一种数据类型的同时，就可以定义好每个成分的访问器。
{% highlight haskell %}
-- file: ch03/BookStore.hs
data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)
{% endhighlight %}

###newtype
1. newtype 声明的作用是重命名`现有类型`，并给它一个新身份
2. newtype 和 type 区别
* newtype 关键字的存在是为了隐藏类型的本性
{% highlight haskell %}
newtype UniqueID = UniqueID Int
    deriving (Eq)
type UniqueID2 = Int 
{% endhighlight %}
* 编译器会把 UniqueID 当成和 Int 不同的类型,而UniqueID2为Int类型
3. data和newtype区别  
* newtype 只能有`一个值构造器`， 并且这个构造器`只能且必须有一个字段`。
* data 关键字创建的类型在运行时有一个簿记开销， 如记录某个值是用哪个构造器创建的。而 newtype 只有一个构造器，所以不需要这个额外开销。
* newtype 的构造器只在编译时使用，`运行时甚至不存在`， 用 newtype 定义的类型和用 data 定义的类型在匹配 `undefined` 时会有不同的行为。
{% highlight haskell %}
data DataInt = D Int
    deriving (Eq, Ord, Show)
newtype NewtypeInt = N Int
    deriving (Eq, Ord, Show)
*Main> case (D undefined) of D _ -> 1
1
*Main> case undefined of D _ -> 1
*** Exception: Prelude.undefined
*Main> case (N undefined) of N _ -> 1
1
*Main> case undefined of N _ -> 1
1   
{% endhighlight %}
没有崩溃！由于运行时不存在构造器，匹配 `N _` 实际上就是在匹配通配符 `_`：由于通配符总可以被匹配，所以表达式是不需要被求值的。

###Maybe Either 
{% highlight haskell %}
data Maybe a = Nothing
             | Just a
               deriving (Eq, Ord, Read, Show)

data Either a b = Left a
                | Right b
                  deriving (Eq, Ord, Read, Show)
{% endhighlight %}

 
###Haskell模式匹配
* 模式匹配的过程就像是逆转一个值的构造（construction）过程，因此它有时候也被称为解构（deconstruction）。
* 易犯错误
{% highlight haskell %}
-- file: ch03/BogusPattern.hs
data Fruit = Apple | Orange

apple = "apple"

orange = "orange"

whichFruit :: String -> Fruit

whichFruit f = case f of
                 apple  -> Apple
                 orange -> Orange
{% endhighlight %}                
这段代码的意思似乎是要检查 f 的值是 apple 还是 orange。

换一种写法可以让错误更加显而易见。（译注：原文的例子太晦涩，换了评论中一个较清楚的例子）
{% highlight haskell %}
-- file: ch03/BogusPattern.hs
whichFruit2 :: String -> Fruit
whichFruit2 apple = Apple
whichFruit2 orange = Orange
{% endhighlight %}
就是这里，显然 apple 并不是指在上层定义的那个变量名为 apple 的值，而是当前作用域的一个模式变量。
上面那个函数的正确写法应该如此：
{% highlight haskell %}

-- file: ch03/BogusPattern.hs
betterFruit f = case f of
                  "apple"  -> Apple
                  "orange" -> Orange
{% endhighlight %}

###守卫实现条件求值
* 我们首先使用了模式匹配已确保值的形式是对的，然后使用了一个守卫来比较其中的一个值的条件是否满足。
{% highlight haskell %}
-- file: ch03/BadTree.hs
nodesAreSame (Node a _ _) (Node b _ _)
    | a == b     = Just a
nodesAreSame _ _ = Nothing
{% endhighlight %}

* otherwise 看上去像是守卫表达式的某种特殊语法，但实际上它只是一个被绑定为值 True 的普通变量
{% highlight haskell %}
-- file: ch03/Lending.hs
lend3 amount balance
     | amount <= 0            = Nothing
     | amount > reserve * 0.5 = Nothing
     | otherwise              = Just newBalance
    where reserve    = 100
          newBalance = balance - amount
{% endhighlight %}

###<font color="red">容易忽略的细节</font>
1. 函数应用的优先级比操作符要高
{% highlight haskell %}
Prelude> (compare 2 3) == LT
True
Prelude> compare 2 3 == LT
True
{% endhighlight %}

2. Haskell 的函数应用是左关联的
	* 表达式 a b c d 等同于 (((a b) c) d)
3. 类型签名中的 -> 符号是右关联的
	* 使用括号可以清晰地标示这个类型签名是怎样被解释的：
{% highlight haskell %}
file: ch02/Take.hs
take :: Int -> ([a] -> [a])
{% endhighlight %}
从这个新的类型签名可以看出， take 函数实际上只接受一个 Int 类型的参数，并返回另一个函数，这个新函数接受一个列表作为参数，并返回一个同类型的列表作为这个函数的结果。


