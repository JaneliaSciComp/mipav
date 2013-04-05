package gov.nih.mipav.view.renderer.WildMagic.Poisson;


import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry.*;

import gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree.*;
import gov.nih.mipav.view.renderer.WildMagic.Poisson.CmdLine.*;

public class PoissonRun {
	
	public static float SCALE = 1.25f;
	
	public int echoStdout=0;
	
	public String outputFile;
	
	public static int EXIT_SUCCESS = 0;
	public static int EXIT_FAILURE = 1;
	
	public void ShowUsage(String ex)
	{
		System.err.println("Usage: " + ex);
		System.err.println("\t--in  <input points>\n");

		System.err.println("\t--out <ouput triangle mesh>\n");

		System.err.println("\t[--depth <maximum reconstruction depth>]\n");
		System.err.println("\t\t Running at depth d corresponds to solving on a 2^d x 2^d x 2^d\n");
		System.err.println("\t\t voxel grid.\n");

		System.err.println("\t[--scale <scale factor>]\n");
		System.err.println("\t\t Specifies the factor of the bounding cube that the input\n");
		System.err.println("\t\t samples should fit into.\n");

		System.err.println("\t[--binary]\n");
		System.err.println("\t\t If this flag is enabled, the point set is read in in\n");
		System.err.println("\t\t binary format.\n");

		System.err.println("\t[--solverDivide <subdivision depth>]\n");
		System.err.println("\t\t The depth at which a block Gauss-Seidel solver is used\n");
		System.err.println("\t\t to solve the Laplacian.\n");

		System.err.println("\t[--samplesPerNode <minimum number of samples per node>\n");
		System.err.println("\t\t This parameter specifies the minimum number of points that\n");
		System.err.println("\t\t should fall within an octree node.\n");

		System.err.println("\t[--confidence]\n");
		System.err.println("\t\t If this flag is enabled, the size of a sample's normals is\n");
		System.err.println("\t\t used as a confidence value, affecting the sample's\n");
		System.err.println("\t\t constribution to the reconstruction process.\n");

		System.err.println("\t[--verbose]\n");
	}
	
	public int Execute(String inputFileName, String outputFileName, int octreeDepth, 
			int solverDivide, int samplePerNode,int Degree) {
		
		int i;
		cmdLineString In = new cmdLineString();
		In.value = inputFileName;
		cmdLineString Out = new cmdLineString();
		Out.value = outputFileName;
		cmdLineReadable Binary = new cmdLineReadable();
		cmdLineReadable Verbose = new cmdLineReadable();
		cmdLineReadable NoResetSamples = new cmdLineReadable();
		cmdLineReadable NoClipTree = new cmdLineReadable();
		cmdLineReadable Confidence = new cmdLineReadable();
		cmdLineInt Depth = new cmdLineInt(octreeDepth);
		cmdLineInt SolverDivide = new cmdLineInt(solverDivide);
		cmdLineInt IsoDivide = new cmdLineInt(8);
		cmdLineInt Refine = new cmdLineInt(3);
		cmdLineInt KernelDepth = new cmdLineInt();
		cmdLineFloat SamplesPerNode = new cmdLineFloat((float)samplePerNode);
		cmdLineFloat Scale = new cmdLineFloat(1.25f);
		
		/*
		String paramNames[]=
		{
			"in","depth","out","refine","noResetSamples","noClipTree",
			"binary","solverDivide","isoDivide","scale","verbose",
			"kernelDepth","samplesPerNode","confidence"
		};
		
		cmdLineReadable params[]=
		{
			In, Depth, Out, Refine, NoResetSamples, NoClipTree,
			Binary,SolverDivide,IsoDivide, Scale, Verbose,
			KernelDepth, SamplesPerNode, Confidence
		};
		
		int paramNum= paramNames.length;
		int commentNum=0;
		String[] comments;
		
		comments=new String[paramNum+7];
		for(i=0;i<=paramNum+7;i++){
			// comments[i]=new String();
		}
		
		String Rev = "Rev: V2 ";
		String Date = "Date: 2006-11-09 (Thur, 09 Nov 2006) ";
		
		cmdLineParse(argc,argv,paramNames,paramNum,params,0);
		
		System.err.println("Running Multi-Grid Octree Surface Reconstructor (degree " + Degree + "). Version 2");
		if(In.set == 1)				{System.err.println("\t--in " + In.value);}
		if(Out.set == 1)				{System.err.println("\t--out " + Out.value);}
		if(Binary.set == 1)			{System.err.println("\t--binary");}
		if(Depth.set == 1)			{System.err.println("\t--depth " + Depth.value);}
		if(SolverDivide.set == 1)	{System.err.println("\t--solverDivide " + SolverDivide.value);}
		if(IsoDivide.set == 1)		{System.err.println("\t--isoDivide " + IsoDivide.value);}
		if(Refine.set == 1)			{System.err.println("\t--refine " + Refine.value);}
		if(Scale.set == 1)			{System.err.println("\t--scale " + Scale.value);}
		if(KernelDepth.set == 1)		{System.err.println("\t--kernelDepth " + KernelDepth.value);}
		if(SamplesPerNode.set == 1)	{System.err.println("\t--samplesPerNode " + SamplesPerNode.value);}
		if(NoResetSamples.set == 1)	{System.err.println("\t--noResetSamples");}
		if(NoClipTree.set == 1)		{System.err.println("\t--noClipTree");}
		if(Confidence.set == 1)		{System.err.println("\t--confidence");}
		*/
		
		double t;
		double tt=System.currentTimeMillis();
		Point3D center = new Point3D();
		float[] scale= new float[1];
		scale[0] = 1.0f;
		float isoValue=0f;
		Octree tree = new Octree(Degree);  // Degree = 2
		
		PPolynomial ReconstructionFunction = new PPolynomial(Degree); // Degree = 2
		ReconstructionFunction = (ReconstructionFunction.GaussianApproximation(0.5, Degree));
		
		center.coords[0]=center.coords[1]=center.coords[2]=0;
		
	    // OctNode.SetAllocator(Octree.MEMORY_ALLOCATOR_BLOCK_SIZE);   // ?????????????????

		t= System.currentTimeMillis();
		int kernelDepth=Depth.value-2;
		if(KernelDepth.set == 1 ){kernelDepth=KernelDepth.value;}
		
		tree.setFunctionData(ReconstructionFunction,Depth.value,0,(1.0f)/(1<<Depth.value));
		
		System.err.println("Function Data Set In: + " + (double)(System.currentTimeMillis()-t) / 1000.0);
		// DumpOutput("Memory Usage: %.3f MB\n",float(MemoryInfo::Usage())/(1<<20));
		
		if(kernelDepth>Depth.value){
			System.err.println("KernelDepth can't be greater than Depth: " + kernelDepth + " <= " + Depth.value);
			return EXIT_FAILURE;
		}
		
		t= System.currentTimeMillis();	
		tree.setTree(In.value,Depth.value,Binary.set,kernelDepth,(float)(SamplesPerNode.value),Scale.value,center,scale,NoResetSamples.set == 0 ? 1 : 0,Confidence.set);
		
		System.err.println("#             Tree set in: " + ((double)(System.currentTimeMillis()-t) / 1000.0) + " (s), " + tree.maxMemoryUsage + " (MB)");
		System.err.println("Leaves/Nodes: " + tree.tree.leaves() + "/" + tree.tree.nodes());
		// DumpOutput("   Tree Size: %.3f MB\n",float(sizeof(TreeOctNode)*tree.tree.nodes())/(1<<20));
		// DumpOutput("Memory Usage: %.3f MB\n",float(MemoryInfo::Usage())/(1<<20));
		
		
		if( NoClipTree.set == 0){
			t= System.currentTimeMillis();
			tree.ClipTree();
			System.err.println("Tree Clipped In: " + ((double)(System.currentTimeMillis()-t) / 1000.0) + " (s)");
			System.err.println("Leaves/Nodes: " + tree.tree.leaves() + " / " + tree.tree.nodes());
			// DumpOutput("   Tree Size: %.3f MB\n",float(sizeof(TreeOctNode)*tree.tree.nodes())/(1<<20));
		}
		
		t=System.currentTimeMillis();	
		tree.finalize1(Refine.value);
		System.err.println("Finalized 1 In: " + ((double)(System.currentTimeMillis()-t) / 1000.0));
		System.err.println("Leaves/Nodes: " + tree.tree.leaves() + " / " + tree.tree.nodes());
		// DumpOutput("Memory Usage: %.3f MB\n",float(MemoryInfo::Usage())/(1<<20));
		
		t=System.currentTimeMillis();
		tree.maxMemoryUsage=0;
		tree.SetLaplacianWeights();
		System.err.println("#Laplacian Weights Set In: " + ((double)(System.currentTimeMillis()-t) / 1000.0) + " (s) " +  tree.maxMemoryUsage);
		// DumpOutput("Memory Usage: %.3f MB\n",float(MemoryInfo::Usage())/(1<<20));
		// tree.printTree();
		// System.err.println("Refine.value = " + Refine.value);
		
		t=System.currentTimeMillis();
		tree.finalize2(Refine.value);
		System.err.println("Finalized 2 In: " + ((double)(System.currentTimeMillis()-t) / 1000.0));
		System.err.println("Leaves/Nodes: " + tree.tree.leaves() + " / " + tree.tree.nodes());
		// DumpOutput("Memory Usage: %.3f MB\n",float(MemoryInfo::Usage())/(1<<20));
		
		
		System.err.println("end line 1");
		
		tree.maxMemoryUsage=0;
		t=System.currentTimeMillis();
		// tree.printTree();
		tree.LaplacianMatrixIteration(SolverDivide.value);
		System.err.println("# Linear System Solved In: " +  ((double)(System.currentTimeMillis()-t) / 1000.0) + "(s)," + tree.maxMemoryUsage + "(MB)");
		// DumpOutput("Memory Usage: %.3f MB\n",float(MemoryInfo::Usage())/(1<<20));
		
		System.err.println("end line 2");
		
		CoredVectorMeshData mesh = new CoredVectorMeshData();
		tree.maxMemoryUsage=0;
		t=System.currentTimeMillis();
		
		isoValue=tree.GetIsoValue();
		// isoValue = -6.600808e+003f;
		System.err.println("Got average in: " + ((double)(System.currentTimeMillis()-t) / 1000.0));
		System.err.println("Iso-Value: " + isoValue);
		// DumpOutput("Memory Usage: %.3f MB\n",float(tree.MemoryUsage()));
		System.err.println("end line 3");
		
		t=System.currentTimeMillis();
		
	   
		if(IsoDivide.value != 0){
			tree.GetMCIsoTriangles(isoValue,IsoDivide.value,mesh, 0, 1);
		}
		else{
			tree.GetMCIsoTriangles(isoValue,mesh, 0, 1);
		}
		
		System.err.println("#        Got Triangles in: " + ((double)(System.currentTimeMillis()-t) / 1000.0) + "(s), " + tree.maxMemoryUsage + "(MB)");
		System.err.println("#              Total Time: " + ((double)(System.currentTimeMillis()-tt) / 1000.0));
		
		System.err.println("end line 4");
		PlyWriteTriangles(Out.value,mesh,1,center,scale[0]);
		
		return 1;
		
	}
	
	public static int PlyWriteTriangles(String fileName, CoredVectorMeshData mesh, int file_type, final Point3D translate, final float scale ) {
		
		int i;
		int nr_vertices=(int)(mesh.outOfCorePointCount()+mesh.inCorePoints.size());
		int nr_faces=mesh.triangleCount();
		float version;
		
		// File f = new File(System.getProperties().getProperty("user.dir"));
	    // String name = f.getPath() + File.separatorChar + fileName;
		
		mesh.resetIterator();
		

	    try {
	    	
	    	PrintWriter kOut = new PrintWriter(new FileWriter(fileName));
	    	
		    if( kOut == null ){
			     System.err.println("Cannot open off mesh triangle file.");
			     return 0;
		    }
		    
		    kOut.println("ply"); // object is ModelTriangleMesh
	        kOut.println("format ascii 1.0");
	        kOut.println("element vertex " + nr_vertices);
	        kOut.println("property float32 x");
	        kOut.println("property float32 y");
	        kOut.println("property float32 z");
	        kOut.println("element face " + nr_faces);
	        kOut.println("property list uint8 int32 vertex_indices");
	        kOut.println("end_header");
	        
	        
	     // write vertices
	        Point3D p = new Point3D();
	        float x, y, z;
	        for (i = 0; i < mesh.inCorePoints.size(); i++) {
	            
	        	p.coords[0]=mesh.inCorePoints.get(i).coords[0];
	        	p.coords[1]=mesh.inCorePoints.get(i).coords[1];
	        	p.coords[2]=mesh.inCorePoints.get(i).coords[2];
	        	
	        	x = p.coords[0]*scale+translate.coords[0];
	    		y = p.coords[1]*scale+translate.coords[1];
	    		z = p.coords[2]*scale+translate.coords[2];
	        	
	            kOut.print(x);
	            kOut.print(' ');
	            kOut.print(y);
	            kOut.print(' ');
	            kOut.println(z);
	        }
	        
	        for (i=0; i < mesh.outOfCorePointCount(); i++){
	    		mesh.nextOutOfCorePoint(p);
	    		x = p.coords[0]*scale+translate.coords[0];
	    		y = p.coords[1]*scale+translate.coords[1];
	    		z = p.coords[2]*scale+translate.coords[2];
	    	    
	    		 kOut.print(x);
		         kOut.print(' ');
		         kOut.print(y);
		         kOut.print(' ');
		         kOut.println(z);
	    		
	    	}  // for, write vertices
	    	
	        // write faces
	    	TriangleIndex tIndex = new TriangleIndex();
	    	int[] inCoreFlag = new int[1];

	    	for (i=0; i < nr_faces; i++){
	    		mesh.nextTriangle(tIndex,inCoreFlag);
	    		
	    		if((inCoreFlag[0] & CoredMeshData.IN_CORE_FLAG[0]) == 0 ? true : false ){tIndex.idx[0]+=(int)(mesh.inCorePoints.size());}
	    		if((inCoreFlag[0] & CoredMeshData.IN_CORE_FLAG[1]) == 0 ? true : false ){tIndex.idx[1]+=(int)(mesh.inCorePoints.size());}
	    		if((inCoreFlag[0] & CoredMeshData.IN_CORE_FLAG[2]) == 0 ? true : false ){tIndex.idx[2]+=(int)(mesh.inCorePoints.size());}
	    		
	    		kOut.print('3');
	        	kOut.print(' ');
	            kOut.print(tIndex.idx[0]);
	            kOut.print(' ');
	            kOut.print(tIndex.idx[1]);
	            kOut.print(' ');
	            kOut.println(tIndex.idx[2]);
	    	}  // for, write faces
	    	
	        
	        kOut.close();
	        
	    } catch (IOException error) {
	          System.err.println("Error while trying to write mesh file.");
	    }
		
		return 1;
		
	}
	
	public static void cmdLineParse(int argc, String[] argv, String[] names,int num,cmdLineReadable[] readable,
			  int dumpError){
		int i,j;
		int current = 0;
		while (argc > 0) {
			
			if (argv[current].startsWith("--")) {
				for(i=0;i<num;i++){
					String temp = argv[current].substring(2, argv[current].length());
					if (temp.equals(names[i])){
						current++; argc--;
						// System.err.println(argv[current]);
						j=readable[i].read(argv[current],argc);
						current++; argc--;
						break;
					}
				}
				if(i==num){
					if(dumpError == 1){
						System.err.println("invalid option: %s\n" + argv);
						System.err.println("possible options are:");
						for(i=0;i<num;i++){ System.err.println(names[i]);}
					}
					current++; argc--;
				}
			}
			else {
				if(dumpError == 1){
					System.err.println("invalid option: " + argv);
					System.err.println("  options must start with a \'--\'\n");
				}
			    current++; argc--;
			}
			
		}
		
	
	}
	
	/*
	public static void main(String[] args)  {
	// int main(int argc,String[] argv)
	    int argc = args.length;
		int degree=2;
		switch(degree)
		{
			case 1:
				Execute(argc,args, 1);
				break;
			case 2:
				Execute(argc,args, 2);
				break;
			case 3:
				Execute(argc,args, 3);
				break;
			case 4:
				Execute(argc,args, 4);
				break;
			case 5:
				Execute(argc,args, 5);
				break;
			default:
				System.err.println("Degree " + degree + " not supported");
				// return EXIT_FAILURE;
		}
		// return EXIT_SUCCESS;
	}
	*/
	
}
