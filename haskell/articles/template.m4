dnl
define(LHOME,    <a href=@{HomeR}>$1</a>)dnl
define(LABOUT,   <a href=@{AboutR}>$1</a>)dnl
define(LARTICLE, <a href=@{ArticleR "$1"}>$2</a>)dnl
dnl
define(FIGURE, <div class="figure">)dnl
define(LABEL,  <p class="label">)dnl
define(IMG,    <img class="picture" src=@{ArticleDataR `ARTICLE' "$1"}>)dnl
dnl TODO handle video, tables, code ...
dnl
define(SECTION,    <h2>)dnl
define(SUBSECTION, <h3>)dnl
dnl
