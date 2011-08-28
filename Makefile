VER = v1-0-0
VERSION = 1.0.0

APP0 = copris
APP = $(APP0)-$(VER)
JAR = lib/$(APP).jar
ZIP = $(APP).zip

WEBPAGE = http://bach.istc.kobe-u.ac.jp/copris/
WEBTITLE = Copris: Constraint Programming in Scala
JARS = lib/sugar-v1-15-0.jar:lib/org.sat4j.core.jar
SRCS = src/jp/kobe_u/*.scala src/jp/kobe_u/copris/*.scala src/jp/kobe_u/copris/sugar/*.scala

DOCTITLE = Copris version $(VERSION) Core API Specification
SCALADOC  = scaladoc \
	-d docs/api \
	-doc-title '$(DOCTITLE)' \
	-doc-version '$(VERSION)' \
	-classpath classes:$(JARS) \
	-sourcepath src

all: scalac jar scaladoc zip

scalac:
	rm -rf classes/*
	fsc -sourcepath src -d classes -cp $(JARS) -optimise $(SRCS)
#	scalac -sourcepath src -d classes -cp $(JARS) -optimise $(SRCS)

jar:
	jar cf $(JAR) -C classes .

scaladoc:
	rm -rf docs/api/*
	$(SCALADOC) $(SRCS)

zip:
	rm -f $(ZIP)
	rm -rf $(APP)
	mkdir $(APP)
	cp -pr Makefile src lib docs examples $(APP)
	rm -f $(APP)/lib/org.sat4j.core.jar $(APP)/examples/classes/*
	find $(APP) \( -name .svn -o -name CVS -o -name .cvsignore -o -name '*~' \) -exec rm -r '{}' '+'
	zip -q -r $(ZIP) $(APP)
	rm -rf $(APP)

copy:
	scp -p copris-v1-0-0.zip lib/copris-v1-0-0.jar docs/index.html bach:html/copris/
	ssh bach 'cd html/copris; rm -rf copris-v1-0-0; unzip copris-v1-0-0.zip'

clean:
	rm -rf classes/*
	rm -rf docs/api/*
	rm -rf $(ZIP) $(JAR)
