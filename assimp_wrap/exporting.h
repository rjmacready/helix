#ifndef EXPORTING_H
#define EXPORTING_H

#include <iostream>
#include <vector>

#include <ecl/ecl.h>

#include <assimp/IOSystem.hpp>
#include <assimp/Exporter.hpp>
#include <assimp/scene.h>

using namespace std;

typedef enum {
	Success,
	Init,
	NotAMesh
} exporting_result;

typedef struct {
	aiScene* scene;
	exporting_result result;
} exporting_info;

void init();

void export_scene_impl(cl_object obj, exporting_info* info);

/*class V3;
class Face;
class Mesh;
class Scene;
*/
#endif
