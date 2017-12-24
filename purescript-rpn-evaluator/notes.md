# 2017-12-24 Creating a PureScript project with yarn

Prerequisites:

```sh
yarn global add bower
yarn global add bower-away
yarn global add psc-package
yarn global add pulp
yarn global add purescript
```

Raw console dump:

```sh
14:41:58 erszcz @ x2 : /tmp
$ mkdir my-psc-project
14:42:05 erszcz @ x2 : /tmp
$ cd my-psc-project/
14:42:08 erszcz @ x2 : /tmp/my-psc-project
$ yarn init 
yarn init v1.3.2
question name (my-psc-project): 
question version (1.0.0): 
question description: 
question entry point (index.js): 
question repository url: 
question author: 
question license (MIT): 
question private: 
success Saved package.json
Done in 2.80s.
14:42:13 erszcz @ x2 : /tmp/my-psc-project
$ ls
package.json
14:42:15 erszcz @ x2 : /tmp/my-psc-project
$ pulp init
* Generating project skeleton in /tmp/my-psc-project
bower cached        https://github.com/purescript/purescript-console.git#3.0.0
bower validate      3.0.0 against https://github.com/purescript/purescript-console.git#*
bower cached        https://github.com/purescript/purescript-prelude.git#3.1.1
bower validate      3.1.1 against https://github.com/purescript/purescript-prelude.git#*
bower cached        https://github.com/purescript/purescript-eff.git#3.1.0
bower validate      3.1.0 against https://github.com/purescript/purescript-eff.git#^3.0.0
bower install       purescript-console#3.0.0
bower install       purescript-prelude#3.1.1
bower install       purescript-eff#3.1.0

purescript-console#3.0.0 bower_components/purescript-console
└── purescript-eff#3.1.0

purescript-prelude#3.1.1 bower_components/purescript-prelude

purescript-eff#3.1.0 bower_components/purescript-eff
└── purescript-prelude#3.1.1
bower cached        https://github.com/purescript/purescript-psci-support.git#3.0.0
bower validate      3.0.0 against https://github.com/purescript/purescript-psci-support.git#*
bower install       purescript-psci-support#3.0.0

purescript-psci-support#3.0.0 bower_components/purescript-psci-support
└── purescript-console#3.0.0
14:42:25 erszcz @ x2 : /tmp/my-psc-project
$ ls
bower_components/  bower.json  package.json  src/  test/
14:43:34 erszcz @ x2 : /tmp/my-psc-project
$ bower-away 

# Update package.json

Changes need to be made in package.json. Please run following to preview them:

$ bower-away --diff

And then apply them by running:

$ bower-away --apply

Please call bower-away once more when you're done with this!

14:43:44 erszcz @ x2 : /tmp/my-psc-project
$ bower-away --diff
{
  "name" : "my-psc-project",
  "version" : "1.0.0",
  "main" : "index.js",
  "license" : "MIT",
  "dependencies" : {
    "@bower_components/purescript-console" : "purescript/purescript-console#^3.0.0",
    "@bower_components/purescript-eff" : "purescript/purescript-eff#^3.0.0",
    "@bower_components/purescript-prelude" : "purescript/purescript-prelude#^3.0.0",
    "@bower_components/purescript-psci-support" : "purescript/purescript-psci-support#^3.0.0"
  },
  "engines" : {
    "yarn" : ">= 1.0.0"
  },
  "scripts" : {
    "postinstall" : "node -e \"try { require('fs').symlinkSync(require('path').resolve('node_modules/@bower_components'), 'bower_components', 'junction') } catch (e) { }\""
  }
}
14:43:50 erszcz @ x2 : /tmp/my-psc-project
$ bower-away --apply

# Remove old components directory

Now, please remove original components directory:
$ rm -rf bower_components

Please call bower-away once more when you're done with this!

14:43:56 erszcz @ x2 : /tmp/my-psc-project
$ rm -rf bower_components/
14:44:05 erszcz @ x2 : /tmp/my-psc-project
$ ls
bower.json  package.json  src/  test/
14:44:06 erszcz @ x2 : /tmp/my-psc-project
$ bower-away 

# Install dependencies with Yarn

Now install dependencies again with:
$ yarn

If you encounter issues during installation, please try:
$ yarn --ignore-engines

If it also fails, you can try following:
$ yarn --ignore-engines --ignore-scripts && yarn postinstall

You can use this command from now on to install both npm and bower dependencies!

Please call bower-away once more when you're done with this!

14:44:18 erszcz @ x2 : /tmp/my-psc-project
$ yarn 
yarn install v1.3.2
info No lockfile found.
[1/5] Validating package.json...
[2/5] Resolving packages...
[3/5] Fetching packages...
[4/5] Linking dependencies...
[5/5] Building fresh packages...
success Saved lockfile.
$ node -e "try { require('fs').symlinkSync(require('path').resolve('node_modules/@bower_components'), 'bower_components', 'junction') } catch (e) { }"
Done in 5.34s.
14:44:46 erszcz @ x2 : /tmp/my-psc-project
$ ls
bower_components@  bower.json  node_modules/  package.json  src/  test/  yarn.lock
14:45:14 erszcz @ x2 : /tmp/my-psc-project
$ bower-away 

# Remove bower.json and old bower components directory

As a last step, please remove bower.json and .bowerrc

Please call bower-away once more when you're done with this!

14:45:18 erszcz @ x2 : /tmp/my-psc-project
$ rm bower.json 
14:45:35 erszcz @ x2 : /tmp/my-psc-project
$ ls -l bower_components/
total 16K
drwxrwxr-x 3 erszcz erszcz 4.0K Dec 24 14:44 purescript-console/
drwxrwxr-x 3 erszcz erszcz 4.0K Dec 24 14:44 purescript-eff/
drwxrwxr-x 4 erszcz erszcz 4.0K Dec 24 14:44 purescript-prelude/
drwxrwxr-x 3 erszcz erszcz 4.0K Dec 24 14:44 purescript-psci-support/
14:45:38 erszcz @ x2 : /tmp/my-psc-project
$ ls -l bower_components 
lrwxrwxrwx 1 erszcz erszcz 50 Dec 24 14:44 bower_components -> /tmp/my-psc-project/node_modules/@bower_components/
14:45:40 erszcz @ x2 : /tmp/my-psc-project
$ bower-away ^C
14:45:49 erszcz @ x2 : /tmp/my-psc-project
$ rm .bowerrc
rm: cannot remove '.bowerrc': No such file or directory
14:45:51 erszcz @ x2 : /tmp/my-psc-project
$ bower-away 

# Done

Your project is now converted to Yarn! Thank you for using Bower!

You should find all bower components in node_modules/@bower_components

The postinstall script should also link it to old location of components

It is advisable to remove postinstall script and point your tools
to point to node_modules/@bower_components instead, though.

You may also consider creating separate directory for front-end project with separate package.json

14:45:56 erszcz @ x2 : /tmp/my-psc-project
$ yarn 
yarn install v1.3.2
[1/5] Validating package.json...
[2/5] Resolving packages...
error Package "@bower_components/purescript-console@" doesn't have a "version".
info Visit https://yarnpkg.com/en/docs/cli/install for documentation about this command.
14:46:05 erszcz @ x2 : /tmp/my-psc-project
$ 
14:46:08 erszcz @ x2 : /tmp/my-psc-project
$ ls
bower_components@  node_modules/  package.json  src/  test/  yarn.lock
14:46:11 erszcz @ x2 : /tmp/my-psc-project
$ psc-package init
Initializing new project in current directory
Using the default package set for PureScript compiler version 0.11.7
(Use --source / --set to override this behavior)
Updating 1 packages...
Updating prelude
14:46:21 erszcz @ x2 : /tmp/my-psc-project
$ cat psc-package.json 
{
    "name": "my-psc-project",
    "set": "psc-0.11.7",
    "source": "https://github.com/purescript/package-sets.git",
    "depends": [
        "prelude"
    ]
}14:46:24 erszcz @ x2 : /tmp/my-psc-project
$ psc-package update
Updating 1 packages...
Update complete
14:46:28 erszcz @ x2 : /tmp/my-psc-project
$ pulp build
* Building project in /tmp/my-psc-project
Error 1 of 2:

  in module Main
  at src/Main.purs line 4, column 1 - line 4, column 31

    Module Control.Monad.Eff was not found.
    Make sure the source file exists, and that it has been provided as an input to the compiler.


  See https://github.com/purescript/documentation/blob/master/errors/ModuleNotFound.md for more information,
  or to contribute content related to this error.

Error 2 of 2:

  in module Main
  at src/Main.purs line 5, column 1 - line 5, column 48

    Module Control.Monad.Eff.Console was not found.
    Make sure the source file exists, and that it has been provided as an input to the compiler.


  See https://github.com/purescript/documentation/blob/master/errors/ModuleNotFound.md for more information,
  or to contribute content related to this error.


* ERROR: Subcommand terminated with exit code 1
14:46:35 erszcz @ x2 : /tmp/my-psc-project
$ vim psc-package.json 
14:46:49 erszcz @ x2 : /tmp/my-psc-project
$ pulp build
* Building project in /tmp/my-psc-project
purs compile: No files found using pattern: .psc-package/psc-0.11.7/console/v3.0.0/src/**/*.purs
purs compile: No files found using pattern: .psc-package/psc-0.11.7/eff/v3.1.0/src/**/*.purs
Error 1 of 2:

  in module Main
  at src/Main.purs line 4, column 1 - line 4, column 31

    Module Control.Monad.Eff was not found.
    Make sure the source file exists, and that it has been provided as an input to the compiler.


  See https://github.com/purescript/documentation/blob/master/errors/ModuleNotFound.md for more information,
  or to contribute content related to this error.

Error 2 of 2:

  in module Main
  at src/Main.purs line 5, column 1 - line 5, column 48

    Module Control.Monad.Eff.Console was not found.
    Make sure the source file exists, and that it has been provided as an input to the compiler.


  See https://github.com/purescript/documentation/blob/master/errors/ModuleNotFound.md for more information,
  or to contribute content related to this error.


* ERROR: Subcommand terminated with exit code 1
14:46:51 erszcz @ x2 : /tmp/my-psc-project
$ psc-package update
Updating 3 packages...
Updating eff
Updating console
Update complete
14:46:57 erszcz @ x2 : /tmp/my-psc-project
$ pulp build 
* Building project in /tmp/my-psc-project
Compiling Data.Boolean
Compiling Data.Show
Compiling Control.Semigroupoid
Compiling Data.NaturalTransformation
Compiling Control.Category
Compiling Data.Function
Compiling Data.Void
Compiling Data.Unit
Compiling Data.HeytingAlgebra
Compiling Data.Semiring
Compiling Data.Semigroup
Compiling Data.Eq
Compiling Data.Functor
Compiling Data.Ring
Compiling Data.Ordering
Compiling Data.BooleanAlgebra
Compiling Control.Apply
Compiling Data.CommutativeRing
Compiling Data.Ord.Unsafe
Compiling Data.Ord
Compiling Data.EuclideanRing
Compiling Control.Applicative
Compiling Data.DivisionRing
Compiling Control.Bind
Compiling Data.Field
Compiling Control.Monad
Compiling Data.Bounded
Compiling Control.Monad.Eff
Compiling Prelude
Compiling Control.Monad.Eff.Uncurried
Compiling Control.Monad.Eff.Class
Compiling Control.Monad.Eff.Console
Compiling Control.Monad.Eff.Unsafe
Compiling Main
* Build successful.
14:47:02 erszcz @ x2 : /tmp/my-psc-project
$ pulp run
* Building project in /tmp/my-psc-project
* Build successful.
Hello sailor!
14:47:05 erszcz @ x2 : /tmp/my-psc-project
$ 
```
