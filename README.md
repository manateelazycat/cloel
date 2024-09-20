# Cloel
Cloel 是一个结合 Clojure 和 Elisp 协同编程框架， 主要是利用 Clojure 的软件生态和多线程能力来扩展 Emacs。

Cloel 的优势有：
1. 速度快： 耗时代码分离到外部 Clojure 进程， 避免大量计算卡住 Emacs
2. 多线程： 利用 Clojure 的多线程技术， 保证所有发送到 Clojure 的请求都可以立即响应
3. Lisp 风格： Clojure 天生的 Lisp 风格， 用外部语言写插件也是同样的享受
4. 生态优势： 借助 Clojure， 我们可以访问庞大而成熟的 JVM 软件包生态， 通过 libpython-clj， 甚至可以访问 Python 软件生态， 大大的扩展 Emacs 的能力
5. 低门槛： 开箱即用的编程框架， 用最少代码编写 Emacs 的 Clojure 扩展

# 安装
### 1. 安装 Clojure
请查看[官方手册](https://clojure.org/guides/install_clojure) 来安装 Clojure

### 2. 安装 Elisp 依赖
[parseedn](https://github.com/clojure-emacs/parseedn): 解析 Clojure 发送的 [EDN](https://github.com/edn-format/edn) 数据

### 3. 安装 Cloel Elisp 部分
用 `git clone` 下载此仓库， 并替换下面配置中的 load-path 路径, 把下面代码加入到你的配置文件 ~/.emacs 中：

```elisp
(add-to-list 'load-path "<path-to-cloel>")

(require 'cloel)
```

### 4. 安装 Cloel Clojure 部分
首先 cd 到 cloel 的目录， 执行下面的命令来安装 Cloel 的 Clojure 库， 主要用于创建 Clojure Server 进程:

```bash
cd cloel ; clojure -X:jar ; clojure -X:install
```

### 5. 测试
Cloel 自带 Demo 用于快速测试

1. 首先 M-x 执行 `load-file`, 并选中 cloel/demo/app.el
2. 然后 M-x 执行 `cloel-demo-test`， 如果 minibuffer 显示 `Cloel rocks!` 的字样， 证明 Cloel 安装成功 

如果没有 `Cloel rocks!` 的字样， 可以查看 buffer `*cloel-demo-clojure-server*` 来查看错误

# 开发
运行完 Demo 后， 请 grep cloel 目录下的 `STEP` 关键字， 根据 `STEP` 关键字的序号可以快速理解 Demo 程序的脉络。

下面是开发 Clojure 插件需要了解的 API, 注意下面 `app` 是应用名称， 实际使用需要把 `app` 换成你的应用名称：
* cloel-register-app: 注册应用的名字和 Clojure 文件的路径， 比如应用叫 `app`, `cloel-register-app` 会自动生成 `cloel-app-` 开头的很多帮助函数
* cloel-app-start-process: 启动应用的 Clojure 进程
* cloel-app-stop-process: 结束应用的 Clojure 进程
* cloel-app-restart-process: 重启应用的 Clojure 进程
* cloel-app-send-message: 从 Elisp 发送消息给 Clojure 进程， 异步的
* cloel-app-call-clojure: 从 Elisp 调用 Clojure 函数， 异步的
* cloel/elisp-eval-async: 从 Clojure 调用 Elisp 函数， 异步的
* cloel/elisp-message: 从 Clojure 发送消息给 Emacs， 并在 minibuffer 中显示, 异步的
* cloel/elisp-eval-sync: 从 Clojure 获取 Elisp 函数计算结果， 同步的
* cloel/elisp-get-var: 从 Clojure 获取 Elisp 变量值， 同步的
