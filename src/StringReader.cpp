#include "StringReader.h"
#include <cassert>

StringReader::StringReader()
{
    str_pos = 0;
}

StringReader::StringReader(std::string inputString)
{
    str_pos = 0;
    setString(inputString);
}

StringReader::~StringReader()
{
    //dtor
}

void StringReader::setString(std::string inputString)
{
    rd_string = inputString;
    end_reached = false;
}

std::string StringReader::word(bool truncateEnd)
{
    std::string result = getTokens(" \n\t", truncateEnd);
    while (result == " " || result == "\n" || result == "\t")
    {
        result = getTokens(" \n\t", truncateEnd);
    }
    return(result);
}

std::string StringReader::line(bool truncateEnd)
{
    return getTokens("\n", truncateEnd);
}

std::string StringReader::getTokens(const char *stop_chars, bool truncateEnd)
{
    size_t found_pos = rd_string.find_first_of(stop_chars, str_pos);

    if (rd_string[str_pos] == '\"') {
        //Find the next quote
        found_pos = rd_string.find("\"", str_pos+1);
        //Check to see if the quote is escaped
        int numBackslashes = 0;
        int countBack = 1;
        while (found_pos >= countBack && rd_string[found_pos-countBack] == '\\') {
            numBackslashes++;
            countBack++;
        }
        //While the quote is escaped
        while (numBackslashes % 2 == 1) {
            //find the next quote
            found_pos = rd_string.find("\"", found_pos+1);
            //Check to see if it's escaped
            numBackslashes = 0;
            countBack = 1;
            while (found_pos >= countBack && rd_string[found_pos-countBack] == '\\') {
                numBackslashes++;
                countBack++;
            }
        }
    }

    if (found_pos == str_pos)                                   //We are at the endline
    {
        std::string stop_char(1, rd_string[str_pos]);
        str_pos++;
        return stop_char;
    } else if (found_pos == std::string::npos)                         //We are at the end of the file
    {
        //End of String
        end_reached = true;
        std::cout << "Reached end of file!\n";
        return "";
    } else {

        if (truncateEnd)                                       //If we want to get rid of the delimiting character, which is the default, don't add the last char. Note we have to increase str_pos by one manually later
            found_pos -= 1;

        if (rd_string[str_pos] == '\"')
            found_pos++;

        std::string string_section;

        for (; str_pos <= found_pos; str_pos++)
        {
            string_section += rd_string[str_pos];
        }

        // if (str_pos <= found_pos) {
        //     string_section = rd_string.substr(str_pos, found_pos+1);
        //     str_pos = found_pos+1;
        // }
        // std::cout << string_section << " - " << str_pos << " - " << found_pos << std::endl;

        if (truncateEnd)                                       //Ok, we didn't add the last char, but str_pos now points at that char. So we move it one ahead.
            str_pos++;
        return string_section;
    }
}

std::string StringReader::truncateEnd(std::string to_truncate)
{
    std::string to_return = "";
    for (unsigned int i = 0; i < to_truncate.length()-1; i++)
        to_return = to_return + to_truncate[i];
    return to_return;
}

void StringReader::test()
{
    {
        StringReader reader("\"x\"");
        assert(reader.word() == "\"x\"");
        assert(reader.word() == "");
    }

    {
        StringReader reader("\"y\" ;\n");
        assert(reader.word() == "\"y\"");
        assert(reader.word() == ";");
        assert(reader.word() == "");
    }

    {
        StringReader reader("Goal = greeting ;\n"
                            "greeting = \"hello\" | greeting \"world\" ;\n");
        assert(reader.word() == "Goal");
        assert(reader.word() == "=");
        assert(reader.word() == "greeting");
        assert(reader.word() == ";");
        assert(reader.word() == "greeting");
        assert(reader.word() == "=");
        assert(reader.word() == "\"hello\"");
        assert(reader.word() == "|");
        assert(reader.word() == "greeting");
        assert(reader.word() == "\"world\"");
        assert(reader.word() == ";");
        assert(reader.word() == "");
    }

    {
        StringReader reader("one   # pretend this is a comment\n"
                            "    two\n");
        assert(reader.word() == "one");
        assert(reader.word() == "#");
        assert(reader.line() == "pretend this is a comment");
        assert(reader.word() == "two");
        assert(reader.word() == "");
    }

    {
        // Quoted strings can span lines.
        StringReader reader("x = \"\n \" ;\n");
        assert(reader.word() == "x");
        assert(reader.word() == "=");
        assert(reader.word() == "\"\n \"");
        assert(reader.word() == ";");
        assert(reader.word() == "");
    }

    {
        // Strings may contain backslash-escaped quote characters.
        StringReader reader(    "\"abc\\\"def\\\\\\\\\\\" \"\n");
        assert(reader.word() == "\"abc\\\"def\\\\\\\\\\\" \"");
        assert(reader.word() == "");
    }

    {
        // A backslash-escaped backslash can be the last character in a string.
        StringReader reader(    "\"\\\\\" \n");
        assert(reader.word() == "\"\\\\\"");
        assert(reader.word() == "");
    }

    std::cout << "StringReader tests pass\n";
}
