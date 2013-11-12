

TARGETS = \
	manticore.zip

manticore.zip:
	sbt proguard
	rm -rf manticore
	mkdir manticore
	cp etc/manticore.sh manticore/manticore
	cp etc/manticore.bat manticore/manticore.bat
	cp etc/csv2bin.sh manticore/csv2bin
	cp etc/csv2bin.bat manticore/csv2bin.bat
	cp etc/flatleg.bat manticore/flatleg.bat
	cp etc/flatleg.sh manticore/flatleg
	chmod +x manticore/manticore
	chmod +x manticore/csv2bin
	chmod +x manticore/flatleg
	cp target/manticore*.min.jar manticore/manticore.jar
	zip -r $@ manticore
	rm -rf manticore


clean:
	rm -f $(TARGETS)
	rm -f target/*.jar

.PHONY: clean