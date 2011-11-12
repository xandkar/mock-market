BEAM_DIR=./ebin
INCLUDE_DIR=./include
SRC_DIR=./src

compile:
	mkdir -p $(BEAM_DIR)
	erlc -I $(INCLUDE_DIR) -o $(BEAM_DIR) $(SRC_DIR)/*

clean:
	rm -rf $(BEAM_DIR)/*.beam
