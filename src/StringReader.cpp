#include "StringReader.h"

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
    std::vector<std::string> stop_chars;
    stop_chars.push_back(" ");
    stop_chars.push_back("\n");
    stop_chars.push_back("\t");
    

    std::string result = getTokens(stop_chars, truncateEnd);
    while (result == " " || result == "\n" || result == "\t")
    {
        result = getTokens(stop_chars, truncateEnd);
    }
    return(result);
}

std::string StringReader::line(bool truncateEnd)
{
    std::vector<std::string> stop_chars;
    stop_chars.push_back("\n");
    return getTokens(stop_chars, truncateEnd);
}

std::string StringReader::getTokens(std::vector<std::string> stop_chars, bool truncateEnd)
{
    int found_pos, new_found_pos;
    std::string stop_char;

    found_pos = rd_string.find(stop_chars[0], str_pos);
    stop_char = stop_chars[0];

    for (unsigned int i = 1; i < stop_chars.size(); i++)
    {
        new_found_pos = rd_string.find(stop_chars[i], str_pos);
        
        //Ok, if the position we found is closer than what we have and is not the end of file, OR the position we are at is the end of file
        //assign the new found position to the currrent found position
        if ( ((new_found_pos <= found_pos) && (new_found_pos != std::string::npos)) || found_pos == std::string::npos )
        {
            found_pos = new_found_pos;
            stop_char = stop_chars[i];
        }
    }

    if (rd_string[str_pos] == '\"') {
        //See if we have an even or odd number of backslashes (that is, this quote is not or is escaped)
        int numBackslashes = 0;
        int countBack = 1;
        while (str_pos-countBack >= 0 && rd_string[str_pos-countBack] == '\\') {
            numBackslashes++;
            countBack++;
        }
        //If the quote is not escaped
        if (numBackslashes % 2 == 0) {
            //Find the next quote
            found_pos = rd_string.find("\"", str_pos+1);
            //Check to see if the quote is escaped
            numBackslashes = 0;
            countBack = 1;
            while (found_pos-countBack >= 0 && rd_string[found_pos-countBack] == '\\') {
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
                while (found_pos-countBack >= 0 && rd_string[found_pos-countBack] == '\\') {
                    numBackslashes++;
                    countBack++;
                }
            }
        }
    }

    if (found_pos == str_pos)                                   //We are at the endline
    {
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
