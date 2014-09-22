#include <iostream>
#include <string>
#include <vector>

using std::cout;
using std::endl;
using std::make_pair;
using std::pair;
using std::string;
using std::vector;

vector<pair<const char *,const char *>> tokenize_string2(const string &text)
{
    vector<pair<const char *, const char *>> tokens;
    const char * from = text.data();
    const char * to = text.data();
next_token:
    while (*to != '\0' && *to != ' ')
        { ++to; }
    if (*to == ' ' && from != to)
        { tokens.push_back(make_pair(from, to)); }
    while (*to == ' ')
        { ++to; }
    if (*to != '\0') {
        from = to;
        goto next_token;
    }
    if (*to == '\0' && from != to)
        { tokens.push_back(make_pair(from, to)); }
    if (*to == '\0')
        { return tokens; }
}

int main()
{
    vector<pair<const char *,const char *>> v;

#ifdef CORRECT
    // a) correct
    string s = "ala    ma kota, a kot ma ale";
    v = tokenize_string2(s);
#else
    // b) buggy
    {
        string s = "ala ma kota, a kot ma ale";
        v = tokenize_string2(s);
    }
#endif

    for (auto i = begin(v); i < end(v); i++) {
        string s2(i->first, i->second - i->first);
        cout << s2 << "\n";
    }

    return 0;
}
