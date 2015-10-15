---
layout: page
title: Welcome To Maxluyifan's World!
tagline: Happy Every Day 
---
{% include JB/setup %}
## Post List 
<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>



