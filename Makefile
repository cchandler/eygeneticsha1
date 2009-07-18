all:
	erlc sha1.erl
	erlc hamming.erl
	erlc eygenetic.erl
	erlc eygenetic_sup.erl
	erlc eygenetic_app.erl
run: all
	erl -e "l(sha1). l(hamming). l(eygenetic)."
