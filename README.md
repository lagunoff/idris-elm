The [Elm architecture](https://guide.elm-lang.org/architecture/)
ported on [Idris Programming language](https://www.idris-lang.org/)

## Building

1. Clone the library on your local file system with `git clone`

```sh
$ git clone https://github.com/lagunoff/idris-elm.git
```

2. Install the library using idris built-in package manager

```sh
$ cd ./idris-elm
$ idris --install
```

3. Create new project, see
   [examples/hello-world](examples/hello-world) for guidance

Besides standard `*.ipkg` and `Main.idr` you need
[`Runtime.js`](src/Elm/Runtime.js) and `index.html` in your project
root directory. [`Runtime.js`](src/Elm/Runtime.js) this file contains
virtual DOM implementation and other runtime utilities taken from
original elm libraries. It has to be injected in `index.html` before
main program, produced by idris.

4. Run idris

```sh
$ idris --build <your-project>.ipkg --codegen JavaScript
```

## Examples

<table>
  <tbody>
    <tr>
      <td>Hello world</td>
      <td>
	    <a href="https://github.com/lagunoff/idris-elm/tree/master/examples/hello-world" target="_blank">source</a> |
		<a href="http://lagunoff.github.io/idris-elm-hello-world/" target="_blank">demo<a>
	  </td>
    </tr>
    <tr>
      <td>TodoMVC</td>
      <td>
	    <a href="https://github.com/lagunoff/idris-elm/tree/master/examples/todomvc" target="_blank">source</a> |
		<a href="http://lagunoff.github.io/todomvc-idris-elm/" target="_blank">demo<a>
	  </td>
    </tr>
  </tbody>
</table>
