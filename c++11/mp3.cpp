//Having to include so many different header files to do basic
//things like open a file, use strings, vectors and tuples, etc,
//is still annoying.
#include <fstream>
#include <vector>
#include <map>
#include <tuple>
#include <string>

//To use C++11 lambdas with Boost lambdas we define this.
//Warning! Bugs in Boost < 1.51 and with compilers that are not
//standard compliant. Gcc 4.7+ should work. In Boost 1.52 and
//later we may not need the define at all.
#define BOOST_RESULT_OF_USE_DECLTYPE

#include <boost/format.hpp> // String formating
#include <boost/algorithm/string.hpp> // Join list of strings
#include <boost/filesystem.hpp> // Iterate over files and directories
#include <boost/range/adaptors.hpp> // Adaptors!
#include <boost/range/algorithm/copy.hpp> // We just need one algorithm

//Like from ... import * in Python. Beware namespace collisions.
//Don't open namespaces directly in a header file.
using namespace std;
using namespace boost::filesystem;
using namespace boost::adaptors;

typedef map<string, string> propMap; // Modelling mp3 metadata String Key/Val
//Extension to FileInfo functor
typedef map<path, function<propMap(string)>> extLookup;

//Size of Mp3 metadata section. Located at the end of the file.
const int kTailSize = 128; 

string stripnulls(string s){
  using namespace boost::algorithm; // Example of local "using"
  erase_all(s, "\0"); //Remove the null bytes
  trim(s); //Trim whitespace in both ends of the string
  return s;
}

string ord(string s){
  //Chars are signed by default, so we need a cast to treat
  //them as unsigned, like the Python code
  int i = static_cast<unsigned char>(s[0]);
  //Boost format is a type safe version of sprintf, which also
  //avoids the problem with preallocating a buffer 
  return (boost::format("%1%") % i).str(); 
}

//In C++11, passing functions around is much easier.
//Everything that behaves like a function from string to string
//can be stored in a function<string(string)>, including objects
//with overloaded call semantics, aka functors.
typedef function<string(string)> StrToStr;

//In the olden days we could not initialize a map inline like this
//Key -> metadata mapping. Unfortunately tuples cannot be
//{}-initialized when nested.
const map<const string, const tuple<int, int, StrToStr>> TagDataMap {
  {"title"   , make_tuple( 3,   30, stripnulls)},
  {"artist"  , make_tuple( 33,  30, stripnulls)},
  {"album"   , make_tuple( 63,  30, stripnulls)},
  {"year"    , make_tuple( 93,   4, stripnulls)},
  {"comment" , make_tuple( 97,  29, stripnulls)},
  {"genre"   , make_tuple(127,   1, ord)}};

propMap Mp3FileInfo(string p){
  propMap ret {{"name", p}};

  ifstream f(p, ios::binary);
  if(f.fail()) return ret;

  string sbuf;
  sbuf.resize(kTailSize);
  f.seekg(-kTailSize, ios::end);
  f.read(&sbuf[0], kTailSize);

  if(sbuf.substr(0,3) != "TAG") return ret;

  int start, length;
  StrToStr mapfun;
  //for loops over collections are finally convenient to use.
  for(auto td : TagDataMap){
    //"tie" is tuple deconstruction and assignment, just like in Python
    tie (start, length, mapfun) = td.second; 
    ret[td.first] = mapfun(sbuf.substr(start, length));
  }

  return ret;
}

vector<propMap> listDirectory(string directory, extLookup exts){
  directory_iterator startd(directory), endd;
  auto files = make_iterator_range(startd, endd);
  vector<propMap> retmap;
  
  for(path p : files){
    auto x = exts.find(p.extension());
    if(x != exts.end()){
      retmap.push_back(x->second(p.string()));
    }
  }
  /*
  I actually think that for loops are too general, and should
  be used as seldom as possible. Range algorithms like filter
  and transform tell the reader (and compiler) exactly what I
  am doing to my data and leaves less room for bugs and misinterpretation.
  I know this is heresy to many C++-ers, though.

  The following is how the above for loop would look in a functional
  list-comprehension style, featuring Boost's range algorithms
  and the new C++11 lambda expression.

  boost::copy(files | filtered([exts](path p){return exts.count(p.extension());}) 
                    | transformed([exts](path p){return exts.find(p.extension())->second(p.string());}),
                    back_inserter(retmap));

  I decided not to go with this anyway, since in this case the
  original for loop is so succinct.
*/

  return retmap;
}

int main(int argc, char* argv[]){
  //A map from file extension to function,
  //replacing the brittle Python introspection method
  extLookup exts = {{".mp3", Mp3FileInfo}};
  //Get a property map for each file in the given directory
  for(propMap pm: listDirectory(argv[1], exts)){
    //"join" is like join in Python and "pm | transformed" pipes
    //the property map through a mapping function that makes
    //strings from the properties.
    cout << join(pm | transformed([](propMap::value_type pv){
          return (boost::format("%1%=%2%") % pv.first % pv.second).str();
        }), "\n");
    cout << "\n\n";
  }
}