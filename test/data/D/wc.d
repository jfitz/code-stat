import std.stdio : writefln, File;
import std.algorithm : map, fold, splitter;
import std.range : walkLength;
import std.typecons : Yes;
import std.uni : byCodePoint;

struct Line {
	size_t chars;
	size_t words;
}

struct Output {
	size_t lines;
	size_t words;
	size_t chars;
}

Output combine(Output a, Line b) pure nothrow {
	return Output(a.lines + 1, a.words + b.words, a.chars + b.chars);
}

Line toLine(char[] l) pure {
	return Line(l.byCodePoint.walkLength, l.splitter.walkLength);
}

void main(string[] args) {
	auto f = File(args[1]);
	Output o = f
		.byLine(Yes.keepTerminator)
		.map!(l => toLine(l))
		.fold!(combine)(Output(0, 0, 0));

	writefln!"%u %u %u %s"(o.lines, o.words, o.chars, args[1]);
}