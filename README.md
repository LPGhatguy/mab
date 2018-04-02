# Lua Parser
I want to build a Lua parser that can be the foundation for a number of future projects for Lua:
* Style checker/reformatter (`stylua`)
* Static typing a la TypeScript
* Documentation parser/generator
* Code mods like Facebook!

## Context/TODO
* Rewrite parser units to return Ok/None/Err to report actual errors
	* Should this be a custom enum, or cram both None/Err cases into Result's Err?
		* This feels kind of gross.
* Flesh out grammar

## Notes
[This trick](https://news.ycombinator.com/item?id=13915458) for implementing precedence -- simple explanation of precedence climbing/Pratt parsing.

## License
This project is available under the terms of The Mozilla Public License, version 2.0. Details are available in [LICENSE](LICENSE).