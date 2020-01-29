# M4D Modal for Dvorak

<kbd>h</kbd> Head, <kbd>t</kbd> Tail, <kbd>n</kbd> Next, <kbd>p</kbd> Prev

基本的光标移动，同时会取消选择。在Dvorak上面对应的位置很好按，保留n, p是因为有很多模式下面没有必要启用模式编辑，这样n, p的上下移动体验是一致的，不需要做任何处理。

---
<kbd>w</kbd> Word, <kbd>m</kbd> Mark Word Back

w选择下一个词；m选择当前词，已有词选择时选择前一个词。在region激活的时候，<kbd>i</kbd>在region的开头插入，<kbd>a</kbd>在region的末尾插入，所以选了一个词就相当于同时拿到了词的开头和末尾两个位置。可以省掉Vim里面的<kbd>e</kbd>。

---
<kbd>e</kbd> Expression

选择当前表达式（symbol或list），已有选择时选择下一个。region激活时可以使用<kbd>x</kbd>来`exchange-mark-and-point`，遇到当前block的边界的时候会自动的调转光标的方向。用来实现原本的<kbd>C-M-f</kbd>, <kbd>C-M-b</kbd>的功能，配合删除实现<kbd>C-M-k</kbd>的功能。

---
<kbd>b</kbd> Block

选择当前的block（list)，已有选择时选择更外层的block，类似expand-region的行为。用来实现原本的<kbd>C-M-n</kbd>，长按的话就会变成选择整个defun，对于eval各个层级的表达式很有帮助。

---
<kbd>l</kbd> Line

选择当前的行，已有选择时向光标的方向扩展一行，L键在Dvorak上面的位置不是很好，所以多用数字会好一些。<kbd>5l</kbd>选择5行。可以代替<kbd>C-a</kbd>(<kbd>lx</kbd>)和<kbd>C-e</kbd>(<kbd>l</kbd>)。

---
<kbd>f</kbd> Flip

选择当前block中光标右侧的内容，已有选择时反转，光标始终在外侧。主要有几个用途：
1. 用来清空block中的全部内容或一部分内容。相比于Vim中常用的类似<kbd>dt)</kbd>的方式，这个方式比较容易保障按键顺手，比如<kbd>fc</kbd>, <kbd>fk</kbd>。
2. 用来在当前block某端追加内容，比如<kbd>fa</kbd>和<kbd>fi</kbd>。

---
<kbd>B</kbd> Buffer

选择整个Buffer，还可以代替<kbd>M-></kbd> (<kbd>B</kbd>)和<kbd>M-<</kbd> (<kbd>Bx</kbd>)。

---
>其中<kbd>i</kbd>, <kbd>a</kbd>, <kbd>x</kbd>, <kbd>k</kbd>是常用的配合的按键。
>基本所有的常用组合指令，或者说常做的操作，都避开了同手同指，邻指跨行，同手跨多列。
