# `flatrapp/shell`
flatr is a software providing management tools for living communities.

This project is a web-frontend speaking the flatr-API written in [Elm](https://elm-lang.org).
It is, together with [`core`](https://github.com/flatrapp/core), the reference implementation of the flatr API.

# WARNING: This software is still heavily in development, will have bugs and won't do what you expect it to do!

# Setup
The Elm source can be transpiled to JavaScript for execution in your favorite browser.
We use yarn + webpack for an easier build process and JavaScript / CSS dependency management.

Please make sure that you've installed `yarn` and the elm command line tools
and that these are accessible from your shell by typing `yarn -v` and `elm -v`.

```
git clone https://github.com/flatrapp/shell.git && cd shell
yarn
elm package install
./node_modules/webpack/bin/webpack.js
```

In case the build was successful, output files can be found in `./dist`.
Serve these with your webserver (while slow and unreliable,
`python -m SimpleHTTPServer` / `python3 -m http.server` should get you started).

In case you are using the awesome NixOS / Nix package manager, a working
nix-expression for an even easier build-process and dependency-management is
on it's way... Stay tuned! Remeber to check out [NixOS](https://nixos.org/) if you
haven't already.

# Special thanks to
* [Evan Czaplicki](https://github.com/evancz), the
  [Elm Software Foundation](http://foundation.elm-lang.org/) and all contributors for
  such an awesome language
* [Magnus Rundberget](https://github.com/rundis) and all contributors for the amazing
  [elm-bootstrap](https://github.com/rundis/elm-bootstrap) library
* [Daniel Schaefer](https://github.com/JohnAZoidberg) for going only midly crazy
  when working with me
* All of the contributors to [this repo](https://github.com/flatrapp/shell/graphs/contributors)
  and the entire flatr ecosystem


# License
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see [https://www.gnu.org/licenses/](https://www.gnu.org/licenses/).
For more information, see [`LICENSE`](https://github.com/flatrapp/shell/blob/master/LICENSE).
