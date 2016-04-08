#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "expat.h"

static char const * const
EXAMPLE_XML = "<presence from='group_1@muc.localhost/apple' to='alice@localhost/res45' xml:lang='en'>"
              "  <x xmlns='http://jabber.org/protocol/muc#user'>"
              "    <item jid='alice@localhost/res45' affiliation='owner' role='moderator'/>"
              "    <status code='110'/><status code='201'/>"
              "  </x>"
              "</presence>"
              "<message from='group_1@muc.localhost' to='alice@localhost/res45' type='groupchat'>"
              "  <subject/>"
              "  <body/>"
              "</message>";

int main(int argc, const char *argv[])
{
    int retval = 0;
    int res, errcode, errpos;
    char * errstring;
    XML_Parser parser;
    parser = XML_ParserCreate("UTF-8");
    res = XML_Parse(parser, EXAMPLE_XML, strlen(EXAMPLE_XML), true);
    if (!res) {
        errcode = XML_GetErrorCode(parser);
        errstring = (char *)XML_ErrorString(errcode);
        errpos = XML_GetCurrentByteIndex(parser);
        printf("errcode:   %d\n"
               "errstring: %s\n"
               "errpos:    %d\n"
               "rest:      %s\n", errcode, errstring, errpos, EXAMPLE_XML+errpos);
        retval = 1;
        goto exit;
    }

exit:
    XML_ParserFree(parser);
    return retval;
}
