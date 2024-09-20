English | [简体中文](./README.zh-CN.md)

# Cloel

Cloel 是一个结合 Clojure 和 Elisp 的协同编程框架， 利用 Clojure 的生态和多线程能力来扩展 Emacs。

主要优势:
1. 速度快： 耗时代码在外部 Clojure 进程执行， 避免卡住 Emacs
2. 多线程： 利用 Clojure 的多线程， 保证快速响应
3. Lisp 风格： 用 Clojure 写插件也能保持 Lisp 风格
4. 强大生态： 可访问 JVM 和 Python 的软件包生态
5. 低门槛： 开箱即用的框架， 简化 Emacs 的 Clojure 扩展开发

## 安装

1. 安装 Clojure, 参考[官方手册](https://clojure.org/guides/install_clojure)

2. 安装 Elisp 依赖： [parseedn](https://github.com/clojure-emacs/parseedn)

3. 安装 Cloel Elisp 部分：
   克隆仓库并添加到 Emacs 配置：
   ```elisp
   (add-to-list 'load-path "<path-to-cloel>")
   (require 'cloel)
   ```

4. 安装 Cloel Clojure 部分：
   ```bash
   cd cloel
   clojure -X:jar
   clojure -X:install
   ```

5. 测试：
   - 执行 `M-x load-file` 选择 `cloel/demo/app.el`
   - 执行 `M-x cloel-demo-test`
   - 如果显示 "Cloel rocks!" 则安装成功, 没有成功请查看 `*cloel-demo-clojure-server*` buffer 以反馈报错信息

## 开发
主要 API, 将 `app` 替换为你的应用名:

- `cloel-register-app`: 注册应用, 需要输入应用名和 Clojure 代码的路径， 应用名以 `app` 为例
- `cloel-app-start/stop/restart-process`: 管理 Clojure 进程
- `cloel-app-send-message`: Elisp 异步发送消息给 Clojure 
- `cloel-app-call-clojure`: Elisp 异步调用 Clojure 函数
- `cloel/start-server`: 启动 Clojure 进程
- `alter-var-root`: 在应用端重载 Cloel 核心的回调处理函数， 具体请看 `demo/app.clj`
- `cloel/elisp-eval-async`: Clojure 异步调用 Elisp 函数
- `cloel/elisp-message`: Clojure 异步发送消息到 Emacs
- `cloel/elisp-eval-sync`: Clojure 同步调用 Elisp 函数
- `cloel/elisp-get-var`: Clojure 同步获取 Elisp 变量值

开发时可通过 grep "STEP" 关键字来理解 Demo 程序结构。 