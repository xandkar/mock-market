BEAM_DIR=./ebin
INCLUDE_DIR=./include
SRC_DIR=./src
DATA_DIR=./data


compile:
	mkdir -p $(BEAM_DIR)
	erlc -I $(INCLUDE_DIR) -o $(BEAM_DIR) $(SRC_DIR)/*


clean:
	rm -f $(BEAM_DIR)/*.beam
	rm -f $(BEAM_DIR)/erl_crash.dump
	rm -f $(DATA_DIR)/*.dat
