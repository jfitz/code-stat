import std.conv, std.regex, std.range, std.file, std.stdio;
import std.string : format;

void main(string[] argv)
{
    immutable ratio = 1.5824;  // UK pounds to US dollar as of this writing
    auto toDollars(Captures!string price)
    {
        real value = to!real(price["integer"]);
        if (!price["fraction"].empty)
            value += 0.01*to!real(price["fraction"]);
        return format("$%.2f",value * ratio);
    }
    string text = std.file.readText(argv[1]);
    auto converted = replaceAll!toDollars(text,
            regex(r"£\s*(?P<integer>[0-9]+)(\.(?P<fraction>[0-9]{2}))?","g"));
    write(converted);
}
