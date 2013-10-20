#include <stdlib.h>
#include <iostream>
#include <vector>
#include <math.h>

#include <ecl/ecl.h>

#include <assimp/IOSystem.hpp>
//#include <assimp/DefaultLogger.hpp>
//#include <assimp/Importer.hpp>
#include <assimp/Exporter.hpp>
#include <assimp/scene.h>

#include "exporting.h"

// we are in C++, we need MAGIC
extern "C" {
	extern void init_lib_GLUE(cl_object);
}

using namespace std;

#define DEFUN(name,fun,args) \
	cl_def_c_function(c_string_to_object(name), \
		(cl_objectfn_fixed)fun, \
										args)

vector<exporting_info*> results;

void export_scene(cl_object obj) {
	//cout << "\n\nexport called!!!\n\n";
	exporting_info* info = (exporting_info*) malloc(sizeof(exporting_info));
	bzero(info, sizeof(exporting_info));

	export_scene_impl(obj, info);
	
	// TODO push info into a collection of exporting results ...
	results.push_back(info);
}

int main(int argc, char** argv) {
	// ecl setup
	cl_boot(argc, argv);	
	
	ecl_init_module(NULL, init_lib_GLUE);

	// register ecl tear down
	atexit(cl_shutdown);
	
	DEFUN("export-scene", export_scene, 1);
	
	if(argc == 1)
		exit(1);

	char* filename = argv[1];	
	//	cout << "filename: " << filename << "\n";	
	
	// TODO check if file exists
	
	// Make lisp object to load the filename
	cl_object load_filename = 
		CONS(c_string_to_object("load"), 
				 CONS(make_constant_base_string(filename),
							Cnil));

	//results = new vector<exporting_info*>();

	init();
	
	// execute (load <filename>)
	cl_safe_eval(load_filename, Cnil, Cnil);
	
	// exports deferred to the lisp script	

	cout << "finito...\n";

	return 0;
}
