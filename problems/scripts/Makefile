include ../../.env

all:
	api_key=${api_key} ./get-best-results.py | tee ../best-results.txt

fetch:
	api_key=${api_key} ./get-best-results.py --fetch | tee ../best-results.txt
