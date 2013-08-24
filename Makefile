VER = v2-1-0
VERSION = 2.1.0

APP0 = copris
APP = $(APP0)-$(VER)
JAR = $(APP).jar
JARALL = $(APP0)-all-$(VER).jar
ZIP = $(APP).zip
SUGAR = sugar-v2-1-0.jar
SAT4J = org.sat4j.core.jar
JARS = lib/$(SUGAR):lib/$(SAT4J)
SRCS = src/jp/kobe_u/*.scala src/jp/kobe_u/copris/*.scala src/jp/kobe_u/copris/sugar/*.scala
WEBPAGE = http://bach.istc.kobe-u.ac.jp/copris/
WEBTITLE = Copris: Constraint Programming in Scala

DOCTITLE = Copris version $(VERSION) Core API Specification
SCALADOC  = scaladoc \
	-d docs/api \
	-doc-title '$(DOCTITLE)' \
	-doc-version '$(VERSION)' \
	-classpath classes:$(JARS) \
	-sourcepath src
#	-doc-source-url 'http://bach.istc.kobe-u.ac.jp/copris/current/src/€{FILE_PATH}.scala'

all: scalac jar scaladoc zip

scalac:
	rm -rf classes/*
	fsc -reset
	fsc -sourcepath src -d classes -cp $(JARS) -optimise $(SRCS)
#	scalac -sourcepath src -d classes -cp $(JARS) -optimise $(SRCS)

jar:
	jar cf ../$(JAR) -C classes .
	cp -p ../$(JAR) lib/
	cd lib; jar xf $(JAR) jp; jar xf $(SUGAR) jp; jar xf $(SAT4J) org; jar cf ../../$(JARALL) jp org; rm -r jp org
	cp -p ../$(JARALL) lib/

scaladoc:
	rm -rf docs/api/*
	$(SCALADOC) $(SRCS)

zip:
	rm -f ../$(ZIP)
	rm -rf $(APP)
	mkdir $(APP)
	cp -pr Makefile src docs examples $(APP)
	mkdir $(APP)/lib
	rm -f $(APP)/lib/copris*.jar $(APP)/examples/classes/*
	cp -pr ../$(JAR) ../$(JARALL) $(APP)/lib
	cp -pr lib/$(SUGAR) lib/$(SAT4J) $(APP)/lib
	find $(APP) \( -name .svn -o -name CVS -o -name .cvsignore -o -name '*~' \) -exec rm -r '{}' '+'
	zip -q -r ../$(ZIP) $(APP)
	rm -rf $(APP)

clean:
	rm -rf classes/*
	rm -rf docs/api/*
