---
layout: post 
category: haskell
tags: [ read , haskell ]
---
{% include JB/setup %}
#Haskell IO

###catch
{% highlight haskell %}
import Prelude hiding (catch)
import Control.Exception(catch, finally,IOException)
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do
       tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
       (tempfile, temph) <- openTempFile tempdir pattern
       finally (func tempfile temph)
               (do hClose temph
                   removeFile tempfile)

{% endhighlight %}
{% highlight haskell %}
Prelude>:type catch
catch :: Exception e => IO a -> (e -> IO a) -> IO a
Prelude>:type finally
finally :: IO a -> IO b -> IO a
{% endhighlight %}

* catch 如果第一个IO动作没有出现异常，那么就作为catch的IO动作，否则根据异常运用另一个IO动作
* finally 无论第一个IO动作结果如何，都在第一个IO动作运行之后运行第二个IO动作，不过结果使用第一个IO动作的返回结果







