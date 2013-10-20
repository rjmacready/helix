#include "exporting.h"

cl_object type_3d_mesh = Cnil;
cl_object type_3d_face = Cnil;
cl_object type_3d_v3 = Cnil;

void init() {
	// init some stuff
	if(Null(type_3d_mesh)) {
		type_3d_mesh = cl_safe_eval(c_string_to_object("'3d:mesh"), Cnil, Cnil);
	}
}


int is_mesh (cl_object obj) {
	return ecl_to_bool(cl_typep(2, obj, type_3d_mesh));
}

int is_face (cl_object obj) {
	return ecl_to_bool(cl_typep(2, obj, type_3d_face));
}

int is_v3 (cl_object obj) {
	return ecl_to_bool(cl_typep(2, obj, type_3d_v3));
}

/*
 * Convert a cl_object to a double. 
 */
int to_double(cl_object o, double *r) {
	*r = 0.0;
	if(ECL_LONG_FLOAT_P(o)) {
		*r = ecl_long_float(o);
	} else if(ECL_DOUBLE_FLOAT_P(o)) {
		*r = ecl_double_float(o);
	} else if(ECL_SINGLE_FLOAT_P(o)) {
		*r = ecl_single_float(o);
	} else if(ECL_FIXNUMP(o)) {
		*r = (double) fix(o);
	} else {
		cout << "Unexpected\n";
		exit(4);
	} 
	return 0;
}

inline double dequals(double d1, double d2, double epsilon) {
	return (fabs(d1 - d2) <= epsilon);
}

void export_scene_impl(cl_object obj, exporting_info* info) {
	info->result = Init;
	cout << "start export\n";

	if(!is_mesh(obj)) {
		cout << "is not a mesh... \n";
		info->result = NotAMesh;
		return;
	}

	cout << "init scene\n";	
	// TODO extract meshes. 
	// for now, only one expected.

	// extract faces
	cout << "init faces\n";
	
	cl_object call_faces = CONS(c_string_to_object("3d:mesh-faces"), CONS(obj, Cnil));
	cl_object r_faces = cl_safe_eval(call_faces, Cnil, Cnil);
	int n_faces = fix(cl_length(r_faces));
	
	cl_object call_verts = CONS(c_string_to_object("3d:mesh-vertices"), CONS(obj, Cnil));
	cl_object r_verts = cl_safe_eval(call_verts, Cnil, Cnil);
	int n_verts = fix(cl_length(r_verts));

	cl_object face_vertices = c_string_to_object("3d:face-vertices");

	cl_object v3_x = c_string_to_object("3d:v3-x");
	cl_object v3_y = c_string_to_object("3d:v3-y");
	cl_object v3_z = c_string_to_object("3d:v3-z");

	cout << n_faces <<" faces\n";
	cout << n_verts << " distinct verts\n";

	// the constructor is hidden from us, 
	// but its safe to do this
	aiScene* scene = info->scene = (aiScene*) malloc(sizeof(aiScene));
	bzero(scene, sizeof(aiScene));
		
	scene->mMeshes = new aiMesh*[ scene->mNumMeshes = 1];
	
	aiMesh *mesh = scene->mMeshes[0] = new aiMesh();

	// Add distinct vertices
	aiVector3D *vertices = mesh->mVertices = new aiVector3D[ mesh->mNumVertices = n_verts ];
	
	{
		int i_vert;
		cl_object cons_vert;
		for(i_vert = 0, cons_vert = r_verts;
				cons_vert != ECL_NIL;
				++i_vert, cons_vert = ECL_CONS_CDR(cons_vert)
				) {
			
			cl_object o = ECL_CONS_CAR(cons_vert);
			cl_object __o = cl_safe_eval(CONS(v3_x, CONS(o, Cnil)), Cnil, Cnil);
			
			// TODO FIXME only works well for integers!!!			
			// extract x y z
			double x, y, z;
			to_double(cl_safe_eval(CONS(v3_x, CONS(o, Cnil)), Cnil, Cnil), &x);
			to_double(cl_safe_eval(CONS(v3_y, CONS(o, Cnil)), Cnil, Cnil), &y);
			to_double(cl_safe_eval(CONS(v3_z, CONS(o, Cnil)), Cnil, Cnil), &z);

			vertices[i_vert].x = x;
			vertices[i_vert].y = y;
			vertices[i_vert].z = z;
			cout << "#v("<<x<<", "<<y<<", "<<z<<")\n";
		}
	}
	
	aiFace* faces = mesh->mFaces = new aiFace[ mesh->mNumFaces = n_faces];

	{
		// Add faces, set vertices
		int i_face;
		cl_object cons_face;

		for(i_face = 0, cons_face = r_faces;
				cons_face != ECL_NIL; 
				++i_face, cons_face = ECL_CONS_CDR(cons_face)) {
		
			cl_object o = ECL_CONS_CAR(cons_face);
			
			cl_object face_verts = 
				cl_safe_eval(CONS(face_vertices, 
													CONS(o, Cnil)), 
										 Cnil, 
										 Cnil);
			
			int n_face_verts = fix(cl_length(face_verts));
			cout << "face[" << i_face << "]: " << n_face_verts << " verts \n";		
			faces[i_face].mIndices = new unsigned int[ faces[i_face].mNumIndices = n_face_verts];
			
			{
				cl_object cons_face_vert;
				int i_face_vert;
				for(cons_face_vert = face_verts, i_face_vert = 0;
						cons_face_vert != ECL_NIL;
						cons_face_vert = ECL_CONS_CDR(cons_face_vert), ++i_face_vert) {
					
					cl_object o_face_vert = ECL_CONS_CAR(cons_face_vert);
					double x,y,z;

					to_double(cl_safe_eval(CONS(v3_x, CONS(o_face_vert, Cnil)), Cnil, Cnil), &x);
					to_double(cl_safe_eval(CONS(v3_y, CONS(o_face_vert, Cnil)), Cnil, Cnil), &y);
					to_double(cl_safe_eval(CONS(v3_z, CONS(o_face_vert, Cnil)), Cnil, Cnil), &z);
					
					cout << "face with vertice ("<<x<<", "<<y<<", "<<z<<")\n";
					
					// find this vertice in the mesh's collection of vertices.
					int i_mesh_vert;
					int found = 0;
					for(i_mesh_vert = 0;
							i_mesh_vert < n_verts;
							i_mesh_vert++) {
						if(dequals(vertices[i_mesh_vert].x, x, 0.0000001)
							 &&
							 dequals(vertices[i_mesh_vert].y, y, 0.0000001)
							 &&
							 dequals(vertices[i_mesh_vert].z, z, 0.0000001)) {
							found = 1;
							cout << "Found vertice "<<i_face_vert<<" at "<<i_mesh_vert<<"\n";
							faces[i_face].mIndices[i_face_vert] = (unsigned int) i_mesh_vert;
							break;
						}
					}
					if(found == 0) {
						cout << "couldn't find vertice ("<<x<<", "<<y<<", "<<z<<")\n";
						exit(5);
					}
				}
			}
			
		}
	}

	// create materials
	scene->mMaterials = new aiMaterial*[scene->mNumMaterials = 1];
  aiMaterial* pcMat = new aiMaterial();

  aiColor4D clr(0.6f,0.6f,0.6f,1.0f);
  pcMat->AddProperty(&clr,1,AI_MATKEY_COLOR_DIFFUSE);
  scene->mMaterials[0] = pcMat;

  const int twosided =1;
  pcMat->AddProperty(&twosided,1,AI_MATKEY_TWOSIDED);

	// create a node
	aiNode* node = new aiNode();
	node->mName.Set("root_node");
	node->mMeshes = new unsigned int[ node->mNumMeshes = 1 ];
	node->mMeshes[0] = 0;
	
	scene->mRootNode = node;	
	
	// export result
	Assimp::Exporter e;
	e.Export(scene, "collada", "out.dae");
	
	info->result = Success;
}
