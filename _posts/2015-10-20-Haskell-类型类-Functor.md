---
layout: post 
category: haskell
tags: [ read , haskell ]
---
{% include JB/setup %}
#Haskell

###Functor

* 只定义了一个函数 `fmap`
{% highlight haskell %}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
{% endhighlight %}
* fmap 当做某种`提升函数`，就像我们在 Avoiding boilerplate with lifting(fix link) 一节中介绍的那样。 它接受一个参数为`普通`值 a -> b 的`函数`并把它提升为一个参数为容器 f a -> f b 的函数。 其中 f 是`容器的类型`。
*  `map` 作为 `fmap` 对`列表`的实现
{% highlight haskell %}
instance Functor [] where
    fmap = map
instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)
{% endhighlight %}
* Functor 的定义限制了我们能用 fmap 做什么。例如，如果`一个类型有且仅有一个``类型`参数，我们才能给它实现 Functor 实例,我们不能给 Either a b 或者 (a, b) 写 fmap 实现，因为它们有两个类型参数。 我们也不能给 Bool 或者 Int 写，因为它们没有类型参数。
* 我们`不`能给类型定义添加`任何约束` 
{% highlight haskell %}
data Foo a = Foo a
instance Functor Foo where
    fmap f (Foo a) = Foo (f a)
--error
data Eq a => Bar a = Bar a
instance Functor Bar where
    fmap f (Bar a) = Bar (f a)
{% endhighlight %}
只有当 a 是 Eq 类型类的成员时，它才能被放进 Bar。 然而，这个约束却让我们无法给 Bar 写 Functor 实例

####类型定义加约束不好
* 实质效果是强迫你给`每一个`用到这种`类型值`的`函数`加`类型约束`,即使这个函数不需要使用这个约束的性质。
* `不`要在类型定义上加`类型约束`，在`需要`它们的`函数`上加, 这还有一个更深远的好处：`类型签名`更准确地表示了每个函数的`真正需求`。


####fmap中缀
* Control.Applicative 模块包含了作为 fmap 别名的 `(<$>)` 操作符。 
{% highlight haskell %}
ghci> (1+) `fmap` [1,2,3] ++ [4,5,6]
[2,3,4,4,5,6]
ghci>:info (++)
(++) :: [a] -> [a] -> [a] 	-- Defined in ‘GHC.Base’
infixr 5 ++
ghci>:info (<$>)
(<$>) :: Functor f => (a -> b) -> f a -> f b
  	-- Defined in ‘Data.Functor’
infixl 4 <$>
{% endhighlight %}
我们这样使用 fmap 一个可能的原因是可以省略第二个参数的括号,fmap中缀操作符形式`优先级`比++低,可以不用括号

####Either Int b类型的Functor实例
{% highlight haskell %}
instance Functor (Either Int) where
    fmap _ (Left n) = Left n
    fmap f (Right r) = Right (f r)
{% endhighlight %}
Haskell 98 类型系统不能保证检查这种实例的`约束`会`终结`。非终结的约束检查会导致`编译器`进入`死循环`，所以这种形式的实例是被禁止的。需要加下面语句才行
{% highlight haskell %}
{-# LANGUAGE FlexibleInstances #-}
{% endhighlight %}


###Functor思考
* Functor 当成统一的、行为规范的对象。
1. Functor 必须保持身份（preserve identity）。也就是说，应用 fmap id 应该返回相同的值。
2.  Functor 必须是可组合的。 也就是说，把两个 fmap 组合使用效果应该和把函数组合起来再用 fmap 相同。
{% highlight haskell %}
ghci> (fmap even . fmap length) (Just "twelve")
Just True
ghci> fmap (even . length) (Just "twelve")
Just True
{% endhighlight %}
另一种看待这两条规则的方式是 Functor 必须保持结构（shape）。集合的结构不应该受到 Functor 的影响，只有对应的值会改变。
{% highlight haskell %}
ghci> fmap odd (Just 1)
Just True
ghci> fmap odd Nothing
Nothing
{% endhighlight %}

* 写Functor实例的准则，形式化
{% highlight haskell %}
fmap id       ==  id
fmap (f . g)  ==  fmap f . fmap g
{% endhighlight %}


