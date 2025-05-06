
ASSN = 3
CLASS= cs143
CLASSDIR= /afs/ir/class/cs143
LIB= -L/usr/pubsw/lib -lfl 
AR= gar
ARCHIVE_NEW= -cr
RANLIB= gar -qs

SRC= semant.cc semant.h cool-tree.h cool-tree.handcode.h good.cl bad.cl README
CSRC= semant-phase.cc symtab_example.cc handle_flags.cc utilities.cc stringtab.cc dumptype.cc tree.cc cool-tree.cc handle_files.cc
TSRC= mycoolc mysemant
CGEN=
HGEN=
LIBS= lexer parser cgen
CFIL= semant.cc ${CSRC} ${CGEN}
LSRC= Makefile
OBJS= ${CFIL:.cc=.o} ast-parse.o ast-lex.o
OUTPUT= good.output bad.output


CPPINCLUDE= -I. -I./src -I./include

FFLAGS = -d8 -ocool-lex.cc
BFLAGS = -d -v -y -b cool --debug -p cool_yy
ASTBFLAGS = -d -v -y -b ast --debug -p ast_yy

CC=g++
CFLAGS=-g -Wall -Wno-unused -Wno-write-strings -Wno-deprecated ${CPPINCLUDE} -DDEBUG
FLEX=flex ${FFLAGS}
BISON= bison ${BFLAGS}

DEPS := ${OBJS:.o=.d}

-include ${DEPS}

SEMANT_OBJS := ${filter-out symtab_example.o,${OBJS}}

semant:  ${SEMANT_OBJS}
	${CC} ${CFLAGS} ${SEMANT_OBJS} ${LIB} -o semant

${OUTPUT}: semant
	@rm -f ${OUTPUT}
	./mysemant good.cl >good.output 2>&1 
	-./mysemant bad.cl >bad.output 2>&1 

symtab_example:
	${CC} ${CFLAGS} symtab_example.cc ${LIB} -o symtab_example

.cc.o:
	${CC} ${CFLAGS} -c $<

ast-parse.cc ast-parse.hh : src/ast.y
	bison ${ASTBFLAGS} -o ast-parse.cc $<

ast-lex.cc : src/ast.flex
	${LEX} ${LEXFLAGS} -o$@ $<

dotest:	semant good.cl bad.cl
	@echo "\nRunning semantic checker on good.cl\n"
	-./mysemant good.cl
	@echo "\nRunning semantic checker on bad.cl\n"
	-./mysemant bad.cl

submit: semant
	$(CLASSDIR)/bin/pa_submit PA3 .

clean:
	rm -f semant ${OBJS} symtab_example ${DEPS} ast-lex.cc ast-parse.cc ast-parse.hh ast-parse.output

# build rules

%.o : %.cc
	${CC} ${CFLAGS} -MMD -c $< -o $@

%.o : src/%.cc
	${CC} ${CFLAGS} -MMD -c $< -o $@

semant.o: semant.h

.DEFAULT_GOAL := semant

# extra dependencies 

