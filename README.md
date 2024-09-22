English | [简体中文](./README.zh-CN.md)

# Cloel

Cloel is a collaborative programming framework that combines Clojure and Elisp, leveraging Clojure's ecosystem and multi-threading capabilities to extend Emacs.

Key advantages:
1. Speed: Time-consuming code runs in an external Clojure process, avoiding Emacs freezes
2. Multi-threading: Utilizes Clojure's multi-threading to ensure fast responsiveness
3. Lisp style: Write plugins in Clojure while maintaining a Lisp style
4. Powerful ecosystem: Access to JVM and Python software package ecosystems
5. Low barrier: Out-of-the-box framework, simplifying Clojure extension development for Emacs

## Installation

1. Install Clojure, refer to the [official guide](https://clojure.org/guides/install_clojure)

2. Install [parseedn](https://github.com/clojure-emacs/parseedn)

3. Install Cloel Elisp part:
   - Clone the repository and add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "<path-to-cloel>")
   (require 'cloel)
   ```

4. Install Cloel Clojure part:
   ```bash
   cd cloel
   clojure -X:jar
   clojure -X:install
   ```

5. Test:
   - Execute `M-x load-file` and select `cloel/demo/app.el`
   - Run `M-x cloel-demo-test`
   - If "Cloel rocks!" is displayed, installation is successful. If not, check the `*cloel-demo-clojure-server*` buffer for error messages

## Version Update
After each update of Cloel, you need to execute `clojure -X:jar; clojure -X:install` to update the Clojure code. 

## Development
During development, you can use the grep "STEP" keyword to understand the structure of the Demo program.

Below are the API details, replace `app` with your application name:

Elisp API:
- `cloel-register-app`: Register an application, requires input of the application name and the path to the Clojure code, using `app` as an example for the application name
- `cloel-app-start/stop/restart-process`: Manage the Clojure process
- `cloel-app-send-message`: Elisp asynchronously sends messages to Clojure
- `cloel-app-call-async`: Elisp asynchronously calls Clojure functions
- `cloel-app-call-sync`: Elisp synchronously calls Clojure functions

Clojure API:
- `cloel/start-server`: Start the Clojure process
- `cloel/elisp-show-message`: Clojure asynchronously displays messages in the minibuffer
- `cloel/elisp-get-var`: Clojure synchronously gets Elisp variable values
- `cloel/elisp-eval-async`: Clojure asynchronously calls Elisp functions
- `cloel/elisp-eval-sync`: Clojure synchronously calls Elisp functions

Interfaces that can be reloaded in Clojure:
- `handle-client-async-call`: Interface for handling Elisp asynchronous calls to Clojure functions
- `handle-client-sync-call`: Interface for handling Elisp synchronous calls to Clojure functions
- `handle-client-message`: Interface for handling Elisp asynchronously sending messages to Clojure
- `handle-client-connected`: Interface for when the Client connects to the Clojure process

In Clojure, we can use `alter-var-root` to reload the above interface implementations on the application side. For specifics, please refer to `demo/app.clj`

## Ecosystem Applications
[reorder-file](https://github.com/manateelazycat/reorder-file): Automatically reorders numbering within files
