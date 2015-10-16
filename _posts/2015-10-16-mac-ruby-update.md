---
layout: post 
category: life 
tags: [ beginner , life ]
---
{% include JB/setup %}
##Mac OS Yosemite update ruby
* brew install openssl 编译失败
couldn't find "stdlib.h"
	{% highlight bash %}
	xcode-select --install 
	{% endhighlight %}	
if 没有 /usr/include,建立软链接
	{% highlight bash %}
	sudo ln -s /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/usr/include/ /usr/include
	{% endhighlight %}
	
* 编译ruby时configure出错
configure: error: something wrong with CFLAGS="-g -O2 "
	{% highlight bash %} 
	CC=/usr/bin/gcc ruby-install ruby 版本号
	{% endhighlight %}
	