#include <string>
#include <vector>
#include <fstream>
#include <cassert>
#include <iostream>
#include <cmath>
#include <random>
#include <sstream>
#include <climits>
#include <omp.h>

#include "EasyBMP.h"
#include "fstream"
#include "matrix.h"


using std::string;
using std::vector;
using std::ifstream;
using std::ostream;
using std::ofstream;
using std::pair;
using std::make_pair;
using std::cout;
using std::cerr;
using std::endl;
using std::tie;
using std::make_tuple;
using std::tuple;
using std::get;
using std::stringstream;
using std::stod;
using std::stoi;

# define PI 3.14159265358979323846



struct array3D
{
	double * arr;
	long width, height, depth;
	long size;
	array3D(){}
	void initialize(long w, long h, long d)
	{
		width=w;
		height=h;
		depth=d;
		arr = new double[w*h*d];
		size = w*h*d;
		for (long i = 0; i < w*h*d; ++i)
		{
			arr[i]=0;
		}
	}
	~array3D()
	{
		
	}

	void destroy()
	{
		delete [] arr;
	}

	double & operator()(long x, long y, long z)
	{			
		return arr[x*height*depth + y*depth + z];
	}

	double & operator[](long indx)
	{
		return arr[indx];
	}

	bool outofRange(long indx)
	{
		return (indx<0 || indx>=size);
	}

	bool outofRange(long x, long y, long z)
	{
		return (x<0||y<0||z<0||x>=width||y>=height||z>=depth);
	}
	
};

struct point
{
	double x,y,z;
	point():x(0),y(0),z(0){}
	point(double a, double b, double  c): x(a),y(b),z(c){}
	point(double a): x(a),y(a),z(a){}
	~point(){}
	point operator +(const point& b)
	{
		return point(this->x+b.x,this->y+b.y,this->z+b.z);
	}
	point operator -(const point& b)
	{
		return point(this->x-b.x,this->y-b.y,this->z-b.z);
	}
	point operator *(double b)//scalar multiplication
	{
		return point(this->x*b,this->y*b,this->z*b);
	}

	point operator /(double b)//scalar ...division
	{
		return point(this->x/b,this->y/b,this->z/b);
	}

	friend point operator *(double, const point&);
	friend ostream& operator<<(ostream& out, const point& p);
	double operator *(point b)//dot product
	{
		return (*this)[0]*b[0]+(*this)[1]*b[1]+(*this)[2]*b[2];
	}
	friend point operator ^(const point& a,const point& b);//cross product
	double length()
	{
		return sqrt((*this)*(*this));
	}
	double & operator [](int i)
	{
		if(i==0) return x;
		if(i==1) return y;
		if(i==2) return z;		
	}

	point& map(double f(double))
	{
		x = f(x);
		y = f(y);
		z = f(z);
		return (*this);
	}

	point norm()
	{		
		return (*this)/(*this).length();
	}

	double operator [](int i) const
	{
		if(i==0) return x;
		if(i==1) return y;
		if(i==2) return z;		
	}
	
};


point operator ^(const point& a,const point& b)//cross product
{
	return point(a[1]*b[2]-a[2]*b[1] , a[2]*b[0]-a[0]*b[2] , a[0]*b[1]-a[1]*b[0]);
}

point operator *(double p, const point& a)
{
	return point(p*a[0],p*a[1],p*a[2]);
}
ostream& operator <<(ostream& out, const point& p)
{
	return out<<"("<<p.x<<" , "<<p.y<<" , "<<p.z<<")";
}

typedef Matrix<point> Image;  

void save_image(const Image &im, const char *path)
{
    BMP out;
    out.SetSize(im.n_cols, im.n_rows);
    RGBApixel p;
    p.Alpha = 255;
    for (int i = 0; i < im.n_rows; ++i) {
        for (int j = 0; j < im.n_cols; ++j) {
            
            p.Red = im(i,j).x; p.Green = im(i,j).y; p.Blue = im(i,j).z;
            out.SetPixel(j, i, p);
        }
    }

    if (!out.WriteToFile(path))
        throw string("Error writing file ") + string(path);
}

Image load_image(const char *path)
{
    BMP in;

    if (!in.ReadFromFile(path))
        throw string("Error reading file ") + string(path);

    Image res(in.TellHeight(), in.TellWidth());

    for (int i = 0; i < res.n_rows; ++i) {
        for (int j = 0; j < res.n_cols; ++j) {
            RGBApixel *p = in(j, i);
            res(i, j) = point(p->Red, p->Green, p->Blue);
        }
    }

    return res;
}


vector<string> split(const string &s, char delim) {
	vector<string> elems;
    std::stringstream ss(s);
    string item;
    while (getline(ss, item, delim)) {
    	if(!item.empty())
        	elems.push_back(item);
    }
    return elems;
}

std::random_device rd;  //Will be used to obtain a seed for the random number engine
std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
std::uniform_real_distribution<> dis(0.0, 1.0);
double rnd()
{     
    return dis(gen);   
}

point rndDir()
{
	double theta = 2*PI* rnd(),z= 2*rnd()-1, x=cos(theta), y = sin(theta);
	point p = point(x,y,0)*sqrt(1-z*z);
	p.z=z;
	return p;
}

struct triangle
{  
	point p1,p2,p3;
	point normal;
	double D;
	double area;
	triangle(){}
	triangle(point a, point b, point c): p1(a), p2(b), p3(c) 
	{		
		normal = (p2-p1)^(p3-p1);
		area= 0.5*normal.length();
		normal=normal/normal.length();
		D=-(normal*p1);
	} 
	friend ostream& operator<<(ostream& out, const triangle& p);
	~triangle(){}
	
};

ostream& operator<<(ostream& out, const triangle& p)
{
	return out<<p.p1<<"<->"<<p.p2<<"<->"<<p.p3<<" normal: "<<p.normal<<" area: "<<p.area;
}

point fromString(string str)
{
	point p;
	vector<string> elems = split(str,' ');
	//elem[0] == "something"
	p[0]= stod( split(elems[1],'/')[0]) ;
	p[1]= stod( split(elems[2],'/')[0]) ;
	p[2]= stod( split(elems[3],'/')[0]) ;
	return p;
}

struct Model
{	

 	
 	int imgW;
	int imgH;

	double wifi_pow;
	point wifi;
	double wifi_radius;

	point c_pos;
	double Xangle,Yangle;
	point up, right, viewdir;
	int outside;

	bool uninit;
	point minp, maxp;
	vector<point> points;

	vector<triangle> bounds;
	
	int iter;

	int radius = 1;

	array3D vspace;
	double voxelsize;	
	int width, height, depth;
	int voxelRes;

	double alpha;
	double gamma = 0.5;
	int AA_level = 1;

	void minmax(point p)
	{
		if(uninit)
		{
			uninit=false;
			minp=p;maxp=p;
		}
		else
		{
			for (int i = 0; i < 3; ++i)
			{
				if(maxp[i]<p[i]) maxp[i]=p[i];
				if(minp[i]>p[i]) minp[i]=p[i];
			}
		}		
	}

	void addVertex(string str)
	{
		point p = fromString(str);
		minmax(p);
		points.push_back(p);
	}

	

	void addTriangle(string str)
	{
		int a,b,c;
		
		vector<string> elems = split(str,' ');
		a= stoi( split(elems[1],'/')[0]) ;
		b= stoi( split(elems[2],'/')[0]) ;
		c= stoi( split(elems[3],'/')[0]) ;	
		
		bounds.push_back( triangle(points[a-1], points[b-1], points[c-1]));
	}

	void initCamera()
	{
		viewdir = viewdir/viewdir.length();
		Xangle = Xangle*PI/180;

		right=viewdir^up;

		double Xspan = tan(Xangle/2);
		double Yspan = Xspan*imgH/imgW;
		Yangle = 2*atan(Yspan);

		up = (up/up.length())*Yspan;
		right = (right/right.length())*Xspan;
		cout<<"view angles: "<<(Xangle*180/PI)<<" "<<(Yangle*180/PI)<<endl;
		cout<<"camera basis: "<<viewdir<<" "<<up<<" "<<right<<endl;
	}

	void initVoxelspace()
	{
		point span = maxp-minp;
		double minspan = fmin(span.x, fmin(span.y, span.z));
		voxelsize = minspan/voxelRes;
		width = span.x/voxelsize+1;
		height = span.y/voxelsize+1;
		depth = span.z/voxelsize+1;
		cout<<"voxelsize: "<<voxelsize<<endl;
		vspace.initialize(width,height,depth);
		cout<<"vspace dimensions: "<<vspace.width<<" "<<vspace.height<<" "<<vspace.depth<<" "<<endl;
		
	}
	void traceRay(point, point);
	void run();
	long getVoxelAdress(point p);
	pair<int,double> check(point & dir, point & base, int , bool );
	void filter();
	void toImage();

	double & getVoxel(point p)
	{
		point crd = (p-minp)/voxelsize;
		crd.map(trunc);
		return vspace(crd.x , crd.y , crd.z);
	}

	Model(): uninit(true){}
	~Model(){}
	
};

long Model::getVoxelAdress(point p)
{
	point crd = (p-minp)/voxelsize;
	crd.map(trunc);
	if( vspace.outofRange(crd.x,crd.y,crd.z))
		return -1;
	return crd.x*vspace.height*vspace.depth+crd.y*vspace.depth+crd.z;
	//arr[x*height*depth + y*depth + z];
}

point barycentric(point & dir, point & base, triangle & trg)
{
	point E1 = trg.p2-trg.p1, E2 = trg.p3 -trg.p1, T = base - trg.p1, P = dir^E2, Q = T^E1;
	return point(Q*E2 , P*T, Q*dir)/(P*E1);
}

double inf = std::numeric_limits<double>::infinity();

pair<int,double> Model::check(point & dir, point & base, int ignore = -1, bool debug = false)
{
	point barymin(inf,-1,-1 ),bary;
	int index=-1;
	double failDist = 0;	
	//cout<<barymin<<endl<<endl;
	for (unsigned int i = 0; i < bounds.size(); ++i)
	{
		bary = barycentric(dir, base, bounds[i]);
		if(debug)
			cout<<i<<": "<<bary<<endl;

		if( bary.x>=0 && bary.x > failDist )
			failDist = bary.x;
		
		if(bary.x>=0 && bary.x<=barymin.x && bary.z >=0 && bary.z<=1 && bary.y >=0 && bary.y<=1 && (1-bary.z-bary.y >=0) && (1-bary.z-bary.y<=1) && i!=ignore)
		{
			index =i;
			barymin=bary;
		}

	}
	if(index<0) barymin.x = failDist;
	return make_pair(index,barymin.x);
}

void Model::traceRay(point dir, point base)
{
	double distance =1, powr = wifi_pow;
	point step = dir*voxelsize;
	int indx=-1;	
	long adr;
	int reflections=-1;
	while(powr>1)
	{	
		pair<int,double> res = check(dir,base, indx);

		if(res.first<0)
		{
			cout<<"0-1; direction: "<<dir<< " with base: "<<base<<" with reflections: "<<reflections<<" with indx: "<<indx<<" was out of range\n";
			break;
		}

		indx = res.first;
		reflections++;
		
		point interpoint  = res.second*dir+base;
		long count =0;
		while(powr>1 && count<res.second/voxelsize)
		{
			adr = getVoxelAdress(base);
			if(adr<0)
			{
				cout<<"0-2; direction: "<<dir<< "with base: "<<base<<" with reflections: "<<reflections<<" with indx: "<<indx<<"was out of range\n";
				indx=1;
				powr=0;
				break;
			}

			if(vspace[adr] < powr)
				vspace[adr] = powr;
			base=base+step;
			distance+=step.length();
			count++;
			
			powr = wifi_pow/(distance*distance);
		}
		
		dir =  dir - 2*bounds[indx].normal*( dir * bounds[indx].normal);
				
		step = dir*voxelsize;
		base = interpoint;
	}
}

bool RaySphereIntersection(point ray_pos, point ray_dir, point spos, double r, double& tResult)
{
//a == 1; // because rdir must be normalized
	point k = ray_pos - spos;
	double b = k*ray_dir; 
	double c = k*k - r*r;
	double d = b*b - c;
	if(d >=0)
	{
	double sqrtfd = sqrt(d);
	double t1 = -b + sqrtfd;
	double t2 = -b - sqrtfd;
	double min_t = fmin(t1,t2);
	double max_t = fmax(t1,t2);
	double t = (min_t >= 0)	?	min_t: max_t;
	tResult = t;
	return (t > 0);
	}
	return false;
}

void Model::toImage()
{
	cout<<"toimage"<<endl;
	Image img(imgH, imgW);
	double maxlog = wifi_pow;
	long adr=0,prevadr=-1;

	Image BSOD = load_image("BSOD.bmp");
	point s_dir = (wifi-c_pos).norm();
	point s_right = (up^s_dir).norm();
	point s_up = (s_dir^s_right).norm(); 

	for (int aa = 0; aa < AA_level; ++aa)
	{
		#pragma omp parallel for firstprivate(adr,prevadr)
		for(long i = 0;i<imgW*imgH;i++)
		{
			int x=i%imgW;
			int y=i/imgW;
			double border=0;
			point dir;
			if(aa == 0)
				dir = viewdir + ((x+0.5)/imgW-0.5)*right - ((y+0.5)/imgH-0.5)*up;
			else
				dir = viewdir + ((x+rnd())/imgW-0.5)*right - ((y+rnd())/imgH-0.5)*up;
			point base;
			dir = dir/dir.length();
			point step = dir*voxelsize;
			pair<int,double> res = check(dir,c_pos);

			if (outside)
			{
				border =res.second-1; 
				res = check(dir, c_pos, res.first);			
			}	
			
			int indx = res.first;
			point clr;
			if(indx<0)
			{
				
				img(y,x)=img(y,x)+ point();
				continue;
			}
			else
				clr = point( 180 + abs(55*dir*bounds[indx].normal) );
			double wifi_t;
			if(RaySphereIntersection(c_pos, dir, wifi, wifi_radius, wifi_t) && wifi_t<res.second)
			{
				point normal =  wifi - (c_pos+dir*wifi_t );
				normal = normal/normal.length();
				int x = (0.5+0.5*(normal*s_right)*(normal*s_dir))*(BSOD.n_cols-1);
				int y = (0.5+0.5*(normal*s_up)*(normal*s_dir))*(BSOD.n_rows-1);

				clr = BSOD(y,x)*(0.2+0.8*(normal*dir));
			}		

			base = c_pos + (res.second)*dir;
			double powr;
			double coeff;
			for (int i = 0; i < (res.second-1-border)/voxelsize; ++i)
			{
				adr=getVoxelAdress(base);
				if((powr=vspace[adr])>2 && adr!=prevadr)
				{
					coeff= pow(powr/maxlog, gamma)*alpha;
					clr=point(254,0,0)*coeff+(1-coeff)*clr;
				}
				base= base-step;
				prevadr=adr;
			}
			img(y,x)=img(y,x)+ clr/AA_level;
			
		}
	}
	cout<<"success: "<<img.n_cols<<" " <<img.n_rows<<endl;
	save_image(img,"res.bmp");
}

void Model::filter()
{
	array3D space;
	int diameter = (2*radius+1);
	space.initialize(vspace.width, vspace.height, vspace.depth);
	//long x,y,z;
	cout<<"filtering voxelspace "<<radius<<"\n";
	//return arr[x*height*depth + y*depth + z];
	#pragma omp parallel for
	for (long x = radius; x < vspace.width-radius; ++x)
	{
		for (long y = radius; y < vspace.height-radius; ++y)
		{
			for (long z = radius; z < vspace.depth-radius; ++z)
			{
				
				for(int ijk = 0; ijk < diameter*diameter*diameter; ijk++)
				{
					int x2 = ijk % diameter-radius;
					int y2 = ijk / diameter % diameter-radius;
					int z2 = ijk / diameter /diameter-radius;
					space(x,y,z) += vspace(x+x2,y+y2,z+z2);
				}
			
				space(x,y,z)/=diameter*diameter*diameter;			
			}
		}
	}
	vspace.destroy();
	vspace=space;
}

void Model::run()
{
	//initSpaces();
	initVoxelspace();	
	initCamera();
	cout<<"wifi distribution with " << iter<<" iterations has started, it may take some time"<<endl;
	for (int i = 0; i < iter; ++i)
	{
		traceRay(rndDir(), wifi);
		//double r = rnd()*2*PI;
		//traceRay( point(cos(r),sin(r),0), wifi);
	}
	cout<<"wifi distribution success"<<endl;
	filter();	
	
	toImage();
	vspace.destroy();
}

Model model{};

void parseOBJ(string file)
{
	string c = split(file,'\r')[0];
	
	ifstream infile(c); 	
   	if (!infile) {
 		cout << "Can't open input file " << c << endl;
 		exit(1);
	}
   	
	string line,inner_line,data;
	
	while(getline(infile,line))   {
    	stringstream isss(line);
    	getline(isss,inner_line, ' ');
    	if(inner_line.compare("v")==0 )
    	{
    		model.addVertex(line);
    		//cout<<line<<"\n";
    	}
    	else
    	if (inner_line.compare("f")==0)
    	{
    		model.addTriangle(line);
    		//cout<<line.erase(0,2)<<"\n";
    	}
    }
}


int main(int argc, char** argv) {
    // Command line options parser
    
    if(argc<2) return cout<<"enter .obj file name",0;
   	ifstream infile(argv[argc-1]);  
   	string line,inner_line;
    while(getline(infile,line))   {
    	stringstream iss(line);
    	getline(iss,inner_line, ' ');
    	//cout<<inner_line<<"\n";
    	if(inner_line.compare("obj-file")==0 )
    	{
    		parseOBJ(split(line,' ')[1]);
    	}
    	else
    	if(inner_line.compare("width")==0 )
    	{
    		model.imgW=stoi(split(line,' ')[1]);
    	}
    	else
    	if(inner_line.compare("height")==0 )
    	{
    		model.imgH=stoi(split(line,' ')[1]);
    	} 
    	if(inner_line.compare("voxel-res")==0 )
    	{
    		model.voxelRes=stoi(split(line,' ')[1]);
    	}
    	if(inner_line.compare("outside")==0 )
    	{
    		model.outside=stoi(split(line,' ')[1]);
    	}
    	if(inner_line.compare("iterations")==0 )
    	{
    		model.iter=stoi(split(line,' ')[1]);
    	}   
    	else
    	if(inner_line.compare("AA-level")==0 )
    	{
    		model.AA_level=stoi(split(line,' ')[1]);
    	}   
    	else
    	if(inner_line.compare("filter-radius")==0 )
    	{
    		model.radius=stoi(split(line,' ')[1]);
    	}   
    	else
    	if(inner_line.compare("wifi")==0 )
    	{
    		model.wifi= fromString(line);
    	} 
    	else
    	if(inner_line.compare("up")==0 )
    	{
    		model.up= fromString(line);
    	}  	
    	else
    	if(inner_line.compare("camera-pos")==0 )
    	{
    		model.c_pos= fromString(line);
    	}  	
    	else
    	if(inner_line.compare("view-angle-x")==0 )
    	{
    		model.Xangle= stod(split(line,' ')[1]);
    	}
    	else
    	if(inner_line.compare("viewdir")==0 )
    	{
    		model.viewdir= fromString(line);
    	}  	
    	else
    	if(inner_line.compare("wifi-pow")==0 )
    	{
    		model.wifi_pow= stod(split(line,' ')[1]);
    	}
    	if(inner_line.compare("wifi-radius")==0 )
    	{
    		model.wifi_radius= stod(split(line,' ')[1]);
    	}
    	if(inner_line.compare("alpha")==0 )
    	{
    		model.alpha= stod(split(line,' ')[1]);
    	} 
    	if(inner_line.compare("gamma")==0 )
    	{
    		model.gamma= stod(split(line,' ')[1]);
    	} 	  	  	 	
    }
    
	for (unsigned int i = 0; i < model.points.size(); ++i)
	{
		cout<< model.points[i]<<"\n";
	}
	cout<<"\n";
	cout<< model.minp<<endl;
	cout<< model.maxp<<endl;
	cout<<"\n";
	for (unsigned int i = 0; i < model.bounds.size(); ++i)
	{
		cout<< model.bounds[i]<<"\n";
	}
	cout<<"\n";

    model.run();
    return 0;
}