

TARGETS = \
	manticore.zip

manticore.zip:
	rm -rf manticore
	mkdir manticore
	cp etc/manticore.sh manticore/manticore
	cp etc/manticore.bat manticore/manticore.bat
	cp etc/csv2bin.sh manticore/csv2bin
	cp etc/csv2bin.bat manticore/csv2bin.bat
	chmod +x manticore/manticore
	chmod +x manticore/csv2bin
	cp target/manticore*.min.jar manticore/manticore.jar
	zip -r $@ manticore
	rm -rf manticore


clean:
	rm -f $(TARGETS)

.PHONY: clean