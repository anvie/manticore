

TARGETS = \
	manticore.zip

manticore.zip:
	rm -rf manticore
	mkdir manticore
	cp etc/manticore.sh manticore/manticore
	cp etc/manticore.bat manticore/manticore.bat
	chmod +x manticore/manticore
	cp target/manticore*.min.jar manticore/manticore.jar
	zip -r $@ manticore
	rm -rf manticore


clean:
	rm -f $(TARGETS)

.PHONY: clean