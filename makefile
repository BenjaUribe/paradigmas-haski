all:
	ghc -package gloss -o game_bin Main.hs Game.hs
	@echo '#!/bin/bash' > game
	@echo 'cd "$$(dirname "$$0")"' >> game
	@echo 'cleanup() { killall -9 ffplay 2>/dev/null; pkill -9 ffplay 2>/dev/null; }' >> game
	@echo 'trap cleanup EXIT' >> game
	@echo './game_bin "$$@"' >> game
	@chmod +x game

run: all
	./game

clean:
	rm -f *.hi *.o game game_bin