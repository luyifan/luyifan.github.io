---
layout: post 
category: haskell
tags: [ read , haskell ]
---
{% include JB/setup %}
#Haskell IO

###IO 

* IO something 类型的所有东西都是一个IO动作，你可以保存它但是什么都不会发生（惰性）
* IO () 表明没有返回值。这和Java或C里面的 void 类似。
* I/O动作可以被创建，赋值和传递到任何地方，但是它们只能在另一个I/O动作里面被执行。
{% highlight haskell %}
ghci> :type putStrLn
putStrLn :: String -> IO ()
ghci> :type getLine
getLine :: IO String
ghci> let writefoo = putStrLn "foo"
ghci> writefoo
foo
{% endhighlight %}
* 输出 foo 不是 putStrLn 的返回值，而是它的`副作用`，把 foo 写到终端上
*  getLine 保存了`一个I/O动作`。当这个动作运行了你会得到一个 String 。 `<-` 运算符是用来从`运行I/O动作`中抽出`结果`，并且保存到一个变量中。
* do 是用来定义一串动作的方便方法, `do代码块`的值是`最后一个动作执行的结果`,在do代码块中,`let` 得到`纯代码`的结果,`非纯代码`的结果使用`<-`
* Haskell中， `return` 是和 <- 相反。也就是说， return 接受一个`纯`的值，把它包装进`IO`

###惰性I/O
* hGetContents 函数，这个函数类型是 Handle -> IO String 。这个返回的 String 表示 Handle 所给文件里的所有数据。它返回的 String 是惰性估值的。在你调用 hGetContents 的时刻，实际上没有读任何东西
* readFile 和 writeFile 都不提供一个句柄给你操作，所以没有东西要去 hClose 。 readFile 在内部使用 hGetContents ，底下的句柄在返回的 String 被垃圾回收或者所有输入都被消费之后就会被关闭。 writeFile 会在供应给它的 String 全部被写入之后关闭它底下的句柄。
* `输入的惰性`, 对 putStr 或者 writeFile 的调用会强制一次性把整个输入字符串载入到内存中吗，直接全部写出？答案是`否定`的。 putStr （以及所有类似的输出函数）在它变得可用时才写出数据。他们也`不需要`保存`已经写`的数据，所以只要程序中没有其他地方需要它，这块内存就可以`立即释放`。在某种意义上，你可以把这个在 readFile 和 writeFile 之间的 String 想成一个连接它们`两个的管道`。数据从`一头进去`，通过某种方式传递，然后从`另外一头流出`。

###interact
* interact 函数的类型是 (String -> String) -> IO () 。也就是说，它接受一个参数：一个类型为 String -> String 的函数。 `getContents` 的结果传递给这个函数，也就是，`惰性读取标准输入`。这个函数的结果会发送到`标准输出`。
* interact惰性过滤器
{% highlight haskell %}
-- file: ch07/filter.hs
main = interact (unlines . filter (elem 'a') . lines)
{% endhighlight %}
得到包含“a”的两行

###动作（Actions）
* 动作类似于函数，它们在定义的时候不做任何事情，而在它们被调用时执行一些任务。I/O动作被定义在 IO Monad, Haskell里使用动作这个工具来使用IO
*  mapM_ 的函数是一个Monad操作，这个操作对列表中的每一项都执行。 mapM_ 扔掉了函数的结果，但是如果你想要Monad的结果，你可以用 mapM 返回一个I/O结果的列表。
{% highlight haskell %}
ghci> :type mapM
mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
ghci> :type mapM_
mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
{% endhighlight %}

###串联化（Sequencing)
* `do` 代码块实际上是把操作连接在一起的`快捷记号`。有两个运算符可以用来代替 do 代码块： `>>` 和 `>>=` 。在 ghci 看一下它们的类型：
{% highlight haskell %}
ghci> :type (>>)
(>>) :: (Monad m) => m a -> m b -> m b
ghci> :type (>>=)
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
{% endhighlight %}
{% highlight haskell %}
main = do
       putStrLn "Greetings!  What is your name?"
       inpStr <- getLine
       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
--equal
main =
    putStrLn "Greetings!  What is your name?" >>
    getLine >>=
    (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")
{% endhighlight %}

###惰性I/O的副作用
* 当我们说没有副作用的时候，我们意思是，Haskell中的`纯代码不`能`运行`那些能`触发副作用的命令`。纯函数不能修改全局变量，请求I/O，或者运行一条关闭系统的命令
* 然而有些时候间接可能产生副作用，比如当你有从 hGetContents 拿到一个 String ，你把它传给一个纯函数，这个函数不知道这个 String 是由`硬盘`文件上来的。这个函数表现地还是和原来一样，但是处理那个 String 的时候可能造成`环境发出I/O命令`。纯函数是`不会`发出I/O命令的，它们作为处理正在运行的纯函数的一个`结果`，就和交换内存到磁盘的例子一样

###缓冲区模式
1. NoBuffering
2. LineBuffering 
3. BlockBuffering 
* 这个对于交互程序不能用，因为它会阻塞输入直到一整块数据被读取。
* BlockBuffering 接受一个 Maybe 类型的参数： 如果是 Nothing ， 它会使用一个`自定的缓冲区大小`，或者你可以使用一个像 Just 4096 的设定
4. 查看设定缓冲区模式
* hGetBuffering和hSetBuffering
