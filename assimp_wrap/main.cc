#include <stdlib.h>
#include <iostream>

#include <ecl/ecl.h>

#include <assimp/IOSystem.hpp>
#include <assimp/DefaultLogger.hpp>
#include <assimp/Importer.hpp>
#include <assimp/Exporter.hpp>
#include <assimp/scene.h>

// we are in C++, we need MAGIC
extern "C" {
	extern void init_lib_GLUE(cl_object);
}

using namespace std;
using namespace Assimp;

int main(int argc, char** argv) {
	// ecl setup
	cl_boot(argc, argv);	
	
	ecl_init_module(NULL, init_lib_GLUE);

	// register ecl tear down
	atexit(cl_shutdown);
	
	for(int i = 0; i < argc; ++i) {
		cout << "arg " << i << ": " << argv[i] << "\n";
	}
	
	
	
	return 0;
}
