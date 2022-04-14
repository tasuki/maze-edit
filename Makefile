release:
	elm make src/Main.elm --optimize --output=mazedit.js

develop:
	find . -name '*.elm' | entr elm make src/Main.elm --debug --output=mazedit.js

serve:
	php -S localhost:8000
