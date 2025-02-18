test-components:
	sbcl --non-interactive --eval "(asdf:test-system \"in.bitspook.web-components/tests\")"

test:
	sbcl --non-interactive --eval "(asdf:test-system \"in.bitspook.cl-ownpress/tests\")"
