#include <boost/program_options.hpp>
namespace po = boost::program_options;

using std::cout;
using std::endl;

typedef std::vector<std::string> string_vector;

int main(int argc, const char *argv[]) {
    po::options_description
        description("Usage: burp [OPTION]... PATTERN FILES");
        // TODO: ... [FILES OR DIRECTORIES] instead of FILES
        //description("Usage: burp [OPTION]... PATTERN [FILES OR DIRECTORIES]");
    description.add_options()
        ("ignore-case,i",   "Ignore case distinctions in PATTERN")
        ("help,h",          "Display this help message")
        ("files",           po::value<string_vector>(),
                            "Input files")
        ("version,v",       "Display the version number");

    po::positional_options_description p;
    p.add("files", -1);
    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv)
                    .options(description)
                    .positional(p)
                    .run(),
                    vm);
    po::notify(vm);

    if (vm.count("help")) {
        std::cout << description;
    }

    if (vm.count("files")) {
        auto files = vm["files"].as<string_vector>();
        for (auto& file : files) {
            cout << "File " << file << endl;
        }
    }

    return 0;
}
