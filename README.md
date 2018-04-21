<h1 align="center">Lua Parser in Rust</h1>

<div align="center">
	<a href="https://travis-ci.org/LPGhatguy/lua-parser">
		<img src="https://api.travis-ci.org/LPGhatguy/lua-parser.svg?branch=master" alt="Travis-CI build status" />
	</a>
</div>

<hr />

This is the foundations for an extensible, style-preserving Lua parser written in Rust. I want to use it for a number of future projects:

* Static analysis tool to replace luacheck
* Style checker and reformatter like gofmt or rustfmt (maybe named "Stylua")
* Static typing like TypeScript or Flow
* Documentation parser/generator like Rustdoc, more robust than LDoc
* A tool like [Google's Rerast for Rust](https://github.com/google/rerast) or [Facebook's codemod](https://github.com/facebook/codemod)

## Goals
* 100% style and whitespace preservation
	* You should be able to read and overwrite your entire project and have zero changes
* Solid AST with support for Lua 5.1+
* Foundation for static analysis and strong typing
* Support for language extensions without breaking existing tools
	* The AST should be able to downlevel emit to any normal version of Lua
	* The project should either:
		* Leverage Rust's type system (`non_exhaustive` patterns, especially) to guarantee that tools can be recompiled with forks of this project with zero changes.
		* Or, use a technique similar to an Entity Component System to implement extended tokens and AST nodes.

## TODO
* Pick a cool name
* Finish grammar
* Write the tools using this project

## Notes
* [Simple explanation of precedence climbing/Pratt parsing](https://news.ycombinator.com/item?id=13915458) for precedence.

## License
This project is available under the terms of The Mozilla Public License, version 2.0. Details are available in [LICENSE](LICENSE).