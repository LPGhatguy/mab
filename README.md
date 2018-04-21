<h1 align="center">Lua Parser in Rust</h1>

<div align="center">
	<a href="https://travis-ci.org/LPGhatguy/lua-parser">
		<img src="https://api.travis-ci.org/LPGhatguy/lua-parser.svg?branch=master" alt="Travis-CI build status" />
	</a>
</div>

<hr />

This is the foundations for an extensible Lua parser written in Rust. I want to use it for a number of future projects:

* Static analysis tool to replace luacheck
* Style checker/reformatter, maybe named "Stylua"
* Static typing like TypeScript/Flow
* Documentation parser/generator
* A tool like [Google's Rerast for Rust](https://github.com/google/rerast) or [Facebook's codemod](https://github.com/facebook/codemod)

## TODO
* Compress parser using macros
* Flesh out grammar
* Implement LKG system like TypeScript

## Notes
* [Simple explanation of precedence climbing/Pratt parsing](https://news.ycombinator.com/item?id=13915458) for precedence.

## License
This project is available under the terms of The Mozilla Public License, version 2.0. Details are available in [LICENSE](LICENSE).