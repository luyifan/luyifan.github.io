---
layout: post 
category: haskell
tags: [ read , haskell ]
---
{% include JB/setup %}
#Haskell

{% highlight haskell %}
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = (10 - sum products) `mod` 10
	where products = mapEveryOther (*3) (reverse ds)
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f,id])
-- zipWith :: ( a -> b -> c ) -> [a] -> [b] -> [c]
-- ($) :: ( a -> b ) -> a -> b 
-- then same as follow ($) :: (x -> y) -> x -> y 
-- so (x->y) is a , x is b , y is c 
-- so zipWith ($) :: [x -> y ] -> [x] -> [y]
-- the same as follow [ a -> b ] -> [a] -> [b]
-- cycle :: [a] -> [a] 
-- cycle [ a -> b ] 
-- 接受有限的函数串，创造无限循环的函数串
-- 和zipWith结合使用，可以使得长度为后面接的串的长度
{% endhighlight %}

###数组
* `Data.Array`,该类型的数组都是`不可变`的。不可变的数组的值只能在它被创建的时候填充一次，之后它的内容就无法被修改了。如果要“修改”数组中的单个元素，整个数组都要被`复制`一次，被修改的元素将在复制的过程中被设置为新的值。
* 数组的`类型`由它所包含`数据的类型`以及`索引的类型`共同决定。比如， String 组成的一维数组的类型为 Array Int String ，而二维 String 数组的类型则是 Array (Int, Int) String。`索引值`的`类型`可以为`Ix类型`的`任意成员`。
* 借助`(!)`运算符通过索引访问元素了
{% highlight haskell %}
ghci> :m +Data.Array
ghci> :type listArray
listArray :: (Ix i) => (i, i) -> [e] -> Array i e
ghci> let a = listArray (0,14) ['a'..]
ghci> a ! 2
'c'
ghci> let a = listArray ('a', 'h') [97..]
ghci> a ! 'e'
101
{% endhighlight %}
* 由于数组构造函数允许我们`随意`指定数组的`边界`，因此我们就没必要像C程序员一样只能用从0开始的索引值了
{% highlight haskell %}
ghci> let a = listArray (-9,5) ['a'..]
ghci> a ! (-2)
'h'
{% endhighlight %}
* 多维数组
{% highlight haskell %}
ghci> let a = listArray ((0,0,0), (9,9,9)) [0..]
ghci> a ! (4,3,7)
437
{% endhighlight %}
* `elems` 取出数组中值的列表
{% highlight haskell %}
ghci>:type elems
elems :: Ix i => Array i e -> [e]
{% endhighlight %}

* `bounds` 函数返回在创建数组时用来指定`边界的元组`。 `indices` 函数返回数组中各个`索引值`组成的列表。
* `ixmap` 可以从原数组中取出一部分,比方说，我们可以从一个二维数组中取出一个一维数组表示的行。
{% highlight haskell %}
ghci>:type ixmap
ixmap:: (Ix j, Ix i) => (i, i) -> (i -> j) -> Array j e -> Array i e
row :: (Ix a, Ix b) => b -> Array (a,b) c -> Array a c
row j a = ixmap (l,u) project a
where project i = (i,j)
        ((l,_), (u,_)) = bounds a
{% endhighlight %}

###数组与惰性
* 这里的数组为`非严格求值`,Haskell也提供了严格求值的数组

###列表推导式(list comprehension) 
{% highlight haskell %}
ghci> [ (a,b) | a <- [1,2], b <- "abc" ]
[(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c')]
{% endhighlight %}

* 竖线右侧的每一个`生成器表达式(generator expression)`组合，都会代入到竖线左侧的表达式中求值。生成表达式绑定了左侧的变量a，a又用“<-”绑定到右侧的元素列表。正如上面的例子展示的，生成表达式的组合将按照`深度优先`的顺序遍历：先是第一个列表的第一个元素分别与第二个列表中的每个元素分别组合，以此类推。
* 列表推导式的右侧为生成器指定`guard`。guard是一个 Bool 表达式。如果guard的值为`False`, 则该元素被`跳过`。
{% highlight haskell %}
ghci> [ (a,b) | a <- [1..6], b <- [5..7], even (a + b ^ 2) ]
[(1,5),(1,7),(2,6),(3,5),(3,7),(4,6),(5,5),(5,7),(6,6)]
{% endhighlight %}

* 可以用`let`表达式绑定`本地变量`(local variable)。
{% highlight haskell %}
ghci> let vowel = (`elem` "aeiou")
ghci> [ x | a <- "etaoin", b <- "shrdlu", let x = [a,b], all vowel x ]
["eu","au","ou","iu"]
{% endhighlight %}

* 生成器表达式中的某个模式匹配`失败`了，那么也`不`会有`错误`发生，只会`跳过`未匹配的列表元素。
{% highlight haskell %}
ghci> [ a | (3,a) <- [(1,'y'),(3,'e'),(5,'p')] ]
"e"
{% endhighlight %}

###Map和Set代替不可变的数组
* 不可变的数组,每次更新，都需复制，构建散列表效率极低
* Haskell标准库提供了两种采用平衡树实现的集合类型:`Data.Map`用于键/值对， `Data.Set` 用于集合
* Data.Map 模块提供了参数化类型 Map k a,将`键类型k`映射到`关联值类型a`
* Map 的`键`是`严格求值`的，但是`映射值`却是`非严格求值`
* Map 类型对`映射值`采用的`惰性求值策略`往往是`内存泄漏的源头`
* 如果需要创建一个空的 Map ,可以使用 `empty` 函数。如果要创建包含一个键/值对的 Map ，则应该使用 `singleton` 函数。
* 查找函数`lookup`,返回值中的类型参数m通常是Maybe类型
{% highlight haskell %}
ghci> :type M.lookup
M.lookup :: (Ord k, Monad m) => k -> M.Map k a -> m a
ghci> let m = M.singleton "foo" 1 :: M.Map String Int
ghci> case M.lookup "bar" m of { Just v -> "yay"; Nothing -> "boo" }
"boo"
{% endhighlight %}
* 添加一个键值对，最有用的函数是`insert`和`insertWith’` 。insert函数就是简单的在map中插入键/值对，如果该键已经存在，则覆盖其关联的任何值。insertWith' 函数会额外接受一个 组合函数(combining function) 。如果map中没有指定的键，就把该键/值对原封不动插入。否则，就先对新旧两个映射值应用组合函数，把应用的结果作为新的映射值更新到map中。函数名最后的`钩号`暗示我们 insertWith' 将对组合函数`严格求值`。这个设计帮你`避免`了`内存泄漏`。
{% highlight haskell %}
ghci> :type M.insert
M.insert :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
ghci> M.insert "quux" 10 m
fromList [("foo",1),("quux",10)]
ghci> M.insert "foo" 9999 m
fromList [("foo",9999)]
ghci> :type M.insertWith'
M.insertWith' :: (Ord k) => (a -> a -> a) -> k -> a -> M.Map k a -> M.Map k a
ghci> M.insertWith' (+) "zippity" 10 m
fromList [("foo",1),("zippity",10)]
ghci> M.insertWith' (+) "foo" 9999 m
fromList [("foo",10000)]
{% endhighlight %}
* `delete` 函数从map中删除指定键。如果键不存在的话， delete 会将map原封不动返回。
{% highlight haskell %}
ghci> :type M.delete
M.delete :: (Ord k) => k -> M.Map k a -> M.Map k a
ghci> M.delete "foo" m
fromList []
{% endhighlight %}
* `union`这个函数是“左偏”(left biased)的：如果两个map包含相同的键，返回map中将包含左侧map中对应的关联值。
{% highlight haskell %}
ghci> m `M.union` M.singleton "quux" 1
fromList [("foo",1),("quux",1)]
ghci> m `M.union` M.singleton "foo" 0
fromList [("foo",1)]
{% endhighlight %}

{% highlight haskell %}

{% endhighlight %}

{% highlight haskell %}

{% endhighlight %}


 