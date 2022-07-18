package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.LUSOL;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;


/**
 This is a port of C code written by Camille Couprie in 2009.
 Camille Couprie has kindly granted the NIH MIPAV project permission to port her Power watershed code from C to Java
under a BSD license.
Porting performed by William Gandler.

This "powerwatershed" package provides implementation of several segmentation algorithms on 2D or 3D images.
The 3 algorithms are:
1.) Maximum Spanning Forest computed by Kruskal algorithm.
2.) Powerwatersheds (p=infinite, q= 2) : Maximum Spanning Forest computed by Kruskal algorithm and
    Random walker on plateaus.
3.) Maximum Spanning Forest computed by Prim algorithm using red and black trees.

Reference: "Power  watersheds: A new image segmentation framework extending graph cuts, random walker
            and optimal spanning forest" by Camille Couprie, Leo Grady, Laurent Najman, and Hugues Talbot,
            ICCV'09, 2009,
 */

public class AlgorithmPowerWatershed extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
    private static int Kruskal = 1;
    
    private static int PW_qis2 = 2;
    
    private static int Prim = 3;
    
    private static byte RBT_Black = 0;
    private static byte RBT_Red =  1;
    
    private double epsilon = 0.000001;
    private int SIZE_MAX_PLATEAU = 1000000;
    
    private boolean testRbtInteractive= false;

    private boolean testRbtRandom = false;
    
    private boolean testIndic = false;
    
    private boolean testLifo = false;
    

    //~ Instance fields ------------------------------------------------------------------------------------------------
    private int algo;
    
    // If true, multi-labels segmentation, else 2-labels segmentation
    //private boolean multi;
    
    // The index in the image array of the seed
    private Vector<Integer> index_seeds;
    
    // For 2-labels 1 for white foreground and 2 for black background
    // For multi-labels values from 1 to n with n <= 255 for segmentation in n labels
    private Vector<Short> index_labels;
    
    // Geodesic reconstruction
    private boolean geod;
    
    private boolean produceProbaImage = true;
    
    private boolean error = false;
    
    private int MAX;
    
    private short Indics[] = null;       /* en global pour etre efficace */
    
    RandomNumberGen randomGen = new RandomNumberGen();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs Power watershed algorithm.
     *
     * @param  destImg  Image model where result image is to stored
     * @param  srcImg   Source image model
     * @param  algo Kruskal, or Prim
     * @param  index_seeds The index in the image array of the seed
     * @param  index_labels For 2-labels 1 for white foreground and 2 for black background
     *                      For multi-labels values from 1 to n with n <= 255 for segmentation in n labels
     * @param  geod  Geodesic reconstruction
     */
    public AlgorithmPowerWatershed(ModelImage destImg, ModelImage srcImg, int algo,
    		Vector<Integer> index_seeds, Vector<Short> index_labels, boolean geod) {

        super(destImg, srcImg);
        this.algo = algo;
        this.index_seeds = index_seeds;
        this.index_labels = index_labels;
        this.geod = geod;
    }

    

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;

        
        super.finalize();
    }
    
    private void rbtInteractiveTest() {
    	// Works the sames as test in original C code.
    	Rbt T = CreeRbtVide(1);
    	  String r;;
    	  double p;
    	  RbtElt x;
          Scanner input = new Scanner(System.in);
    	  do
    	  {
    		  System.out.println("Commands qUIT, PuSH, PoP, pRINT, TESTeMPTY");
    		  System.out.println("sEARCH MiNIMUM MaXIMUM SUcCESSOR dELETE");
    		  r = input.nextLine();
    		  if (r.equalsIgnoreCase("u")) {
    			  System.out.println("Double value: ");
    			  p = input.nextDouble();
    			  RbtInsert(T, p, 0);
    		  }
    		  else if (r.equalsIgnoreCase("d")) {
    			  System.out.println("Double value: ");
    			  p = input.nextDouble(); 
    			  x = RbtSearch(T, p);
    		      if (x != T.getNil()) RbtDelete(T, x);
    		      else System.out.println("Not found");
    		  }
    		  else if (r.equalsIgnoreCase("s")) {
    			  System.out.println("Double value: ");
    			  p = input.nextDouble(); 
    			  x = RbtSearch(T, p);
    		      System.out.println("Found: x != T.getNil() = " + (x != T.getNil()));
    		  }
    		  else if (r.equalsIgnoreCase("i")) {
    			  x = RbtMinimum(T, T.getRoot());
    		      System.out.println("minimum: x.getKey() = " + x.getKey());
    		  }
    		  else if (r.equalsIgnoreCase("a")) {
    		      x = RbtMaximum(T, T.getRoot());
    	          System.out.println("maximum: x.getKey() = " + x.getKey());
    		  }
    		  else if (r.equalsIgnoreCase("c")) {
    			  System.out.println("Double value: ");
    			  p = input.nextDouble(); 
    			  x = RbtSearch(T, p);
    		      System.out.println("Found: x != T.getNil() = " + (x != T.getNil()));
    		      if (x != T.getNil())
    			{
    		          x = RbtSuccessor(T, x);
    		          if (x != T.getNil()) System.out.println("succ: x.getKey() = " + x.getKey());
    			}

    		  }
    		  else if (r.equalsIgnoreCase("o")) {
    			  if (RbtVide(T)) 
    		          System.out.println("Empty");
    		        else
    		          RbtPopMin(T); 
    		  }
    		  else if (r.equalsIgnoreCase("p")) {
    			  RbtPrint(T);
    		  }
    		  else if (r.equalsIgnoreCase("e")) {
    			  System.out.println("Empty = " + RbtVide(T));  
    		  }
    	  } while (!r.equalsIgnoreCase("q"));
    	  RbtTermine(T);
          input.close();
	
    }
    
    private void rbtRandomTest() {
    	  Rbt T = CreeRbtVide(1);
    	  int n = 0, d;
    	  int rand;
    	  Scanner input = new Scanner(System.in);
    	  String r;   
    	  do
    	  {
    		System.out.println("Press q to quit any other key to continue");
    		 r = input.nextLine();
    		 if (r.equalsIgnoreCase("q")) {
    			 break;
    		 }
    		rand = randomGen.genUniformRandomNum(0, 32767);
    	    if ((rand % 2) == 1)
    	    {
    	       d = randomGen.genUniformRandomNum(0, 32767);
    	       RbtInsert(T, (double)d, d);
    	       n++;
    	       System.out.println("I insert d = " + d + " n = " + n);
    	    }
    	    else
    	    {
    	      if (RbtVide(T)) 
    	        System.out.println("Empty");
    	      else
    	      {
    	        d = RbtPopMin(T); 
    	        n--;
    	        System.out.println("I withdraw d = " + d + " n = " + n);
    	      }
    	    }
    	  } while (true);
    	  RbtTermine(T);
    	  input.close();

    }
    
    private void indicTest() {
    	IndicsInit(3);
    	  Set(0, 0);   if (IsSet(0, 0)) System.out.println("test1 ok");
    	System.out.println("Indics[0] = " + Indics[0]);
    	  Set(0, 1);   if (IsSet(0, 1)) System.out.println("test2 ok");
    	System.out.println("Indics[0] = " + Indics[0]);
    	  UnSet(0, 1); if (!IsSet(0, 1)) System.out.println("test3 ok");
         System.out.println("Indics[0] = " + Indics[0]);
    	               if (IsSet(0, 0)) System.out.println("test4 ok");
    	  UnSetAll(0); if (!IsSet(0, 0)) System.out.println("test5 ok");
    	 System.out.println("Indics[0] = " + Indics[0]);
         Indics = null;

    }
    
    private void lifoTest() {
    	Lifo L = CreeLifoVide(3);
    	  LifoPrint(L);
    	  if (LifoVide(L)) System.out.println("Lifo empty YES\n");
    	  LifoPush(L,1);
    	  LifoPrint(L);
    	  if (!LifoVide(L)) System.out.println("Lifo empty NO\n");
    	  LifoPush(L,2);
    	  LifoPrint(L);
    	  LifoPush(L,3);
    	  LifoPrint(L);
    	  System.out.println("LifoPop(L) = " + LifoPop(L) + " , 3 expected");
    	  LifoPrint(L);
    	  LifoPush(L,4);
    	  LifoPrint(L);
    	  System.out.println("LifoPop(L) = " + LifoPop(L) + " , 4 expected");
    	  LifoPrint(L);
    	  System.out.println("LifoPop(L) = " + LifoPop(L) + " , 2 expected");
    	  LifoPrint(L);
    	  System.out.println("LifoPop(L) = " + LifoPop(L) + " , 1 expected");
    	  LifoPrint(L);
    	  if (LifoVide(L)) System.out.println("Lifo empty YES");
    	  System.out.println("Full release now expected on lifo");
    	  LifoPush(L,3);
    	  LifoPush(L,3);
    	  LifoPush(L,3);
    	  LifoPush(L,3);  

    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	if (testRbtInteractive) {
    		rbtInteractiveTest();
    		setCompleted(false);
    		return;
    	}
    	
    	if (testRbtRandom) {
    		rbtRandomTest();
    		setCompleted(false);
    		return;
    	}
    	if (testIndic) {
    		indicTest();
    		setCompleted(false);
    		return;
    	}
    	if (testLifo) {
    		lifoTest();
    		setCompleted(false);
    		return;
    	}
        int M; // Number of edges
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim;
        int edges[][];
        int weights[];
        int max_weight;
        int max_weight_PW[] = new int[1];
        boolean quicksort = false;
        int size_seeds = index_seeds.size();
        ModelImage colorImage = null;
        ModelImage grayImage = null;
        ModelImage probaImage = null;
        int i;
        int nblabels;
        if (srcImage.getNDims() == 2) {
        	zDim = 1;
        }
        else {
        	zDim = srcImage.getExtents()[2];
        }
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (destImage == null) {
            displayError("Destination Image is null");

            return;
        }



        fireProgressStateChanged("Power Watershed ...");
        
        M = zDim * xDim * (yDim - 1) + zDim * (xDim - 1) * yDim + (zDim - 1) * xDim * yDim;
        edges = new int[2][M];
        nblabels = index_labels.get(0);
        for (i = 1; i < index_labels.size(); i++) {
            if (index_labels.get(i) > nblabels)	{
            	nblabels = index_labels.get(i); 
            }
        }
        
        compute_edges(edges, xDim, yDim, zDim);
        
        if (srcImage.isColorImage()) {
        	if ((srcImage.getFileInfo()[0].getDataType() == ModelStorageBase.ARGB_USHORT) ||
        		(srcImage.getFileInfo()[0].getDataType() == ModelStorageBase.ARGB_FLOAT)) {
        	    colorImage = new ModelImage(ModelStorageBase.ARGB, srcImage.getExtents(), "ColorImage");
        	    boolean image25D = true;
        	    AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(colorImage, srcImage, srcImage.getMin(),
        	    		srcImage.getMax(), 0, 255, image25D);
        	    changeTypeAlgo.run();
          		changeTypeAlgo.finalize();
          		changeTypeAlgo = null;
          		FileInfoBase[] fileInfo = colorImage.getFileInfo();
                fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
                fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
                fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
                fileInfo[0].setExtents(colorImage.getExtents());
                fileInfo[0].setMax(colorImage.getMax());
                fileInfo[0].setMin(colorImage.getMin());
                fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
                fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
                fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        	} // if ((srcImage.getFileInfo()[0].getDataType() == ModelStorageBase.ARGB_USHORT) ||
        	else {
        		colorImage = srcImage;
        	}
        } // if (srcImage.isColorImage())
        else { // gray image
            if ((srcImage.getMin() < 0)	|| (srcImage.getMax() > 255) || 
            (srcImage.getFileInfo()[0].getDataType() == ModelStorageBase.FLOAT) ||
                (srcImage.getFileInfo()[0].getDataType() == ModelStorageBase.DOUBLE)) {
            	grayImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "GrayImage");
        	    boolean image25D = true;
        	    AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(grayImage, srcImage, srcImage.getMin(),
        	    		srcImage.getMax(), 0, 255, image25D);
        	    changeTypeAlgo.run();
          		changeTypeAlgo.finalize();
          		changeTypeAlgo = null;
          		FileInfoBase[] fileInfo = grayImage.getFileInfo();
                fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
                fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
                fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
                fileInfo[0].setExtents(grayImage.getExtents());
                fileInfo[0].setMax(grayImage.getMax());
                fileInfo[0].setMin(grayImage.getMin());
                fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
                fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
                fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());   	
                }
            else {
            	grayImage = srcImage;
            }
        }
        if (algo == Kruskal) {
            weights = new int[M];
            max_weight = 255;
            if (srcImage.isColorImage()) {
            	max_weight = color_standard_weights(colorImage, weights, edges, index_seeds, size_seeds, geod, quicksort);
            	if (error) {
            		setCompleted(false);
            		return;
            	}
            } // if (srcImage.isColorImage())
            else {
            	grey_weights(grayImage, weights, edges, index_seeds, size_seeds, geod, quicksort);
            	if (error) {
            		setCompleted(false);
            		return;
            	}
            }
            
            MSF_Kruskal(edges, weights, max_weight, index_seeds, index_labels, size_seeds, xDim, yDim, zDim, nblabels);
            weights = null;
            if (error) {
            	setCompleted(false);
            	return;
            }
        } // if (algo == Kruskal)
        else if (algo == Prim) {
        	weights = new int[M];
            max_weight = 255;
            if (srcImage.isColorImage()) {
            	max_weight = color_standard_weights(colorImage, weights, edges, index_seeds, size_seeds, geod, quicksort);
            	if (error) {
            		setCompleted(false);
            		return;
            	}
            } // if (srcImage.isColorImage())
            else {
            	grey_weights(grayImage, weights, edges, index_seeds, size_seeds, geod, quicksort);
            	if (error) {
            		setCompleted(false);
            		return;
            	}
            }
            
            MSF_Prim(edges, weights, index_seeds, index_labels, size_seeds, xDim, yDim, zDim, nblabels);
            weights = null;
            if (error) {
            	setCompleted(false);
            	return;
            }	
        } // else if (algo == Prim)
        else if (algo == PW_qis2) {
        	if (produceProbaImage) {
                probaImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), srcImage.getImageName() + "_proba");
        	}
            weights = new int[M];
            int normal_weights[];
            max_weight_PW[0] = 255;
            if (srcImage.isColorImage()) {
            	normal_weights = color_standard_weights_PW(colorImage, weights, edges, index_seeds, size_seeds,
            			max_weight_PW, quicksort);
            	if (error) {
            		setCompleted(false);
            		return;
            	}
            } // if (srcImage.isColorImage())
            else {
            	normal_weights = grey_weights_PW(grayImage, edges, index_seeds, size_seeds, weights, quicksort);
            	if (error) {
            		setCompleted(false);
            		return;
            	}
            }
            if (geod) {
            	PowerWatershed_q2(edges, weights, weights, max_weight_PW[0], index_seeds, index_labels, size_seeds, xDim, yDim, zDim,
            			nblabels, quicksort, probaImage);
            }
            else {
            	PowerWatershed_q2(edges, weights, normal_weights, max_weight_PW[0], index_seeds, index_labels, size_seeds, xDim, yDim, zDim,
            			nblabels, quicksort, probaImage);	
            }
        } // else if (algo == PW_qis2)
        if (srcImage.isColorImage() && (srcImage != colorImage)) {
        	colorImage.disposeLocal();
        	colorImage = null;
        }
        else if ((!srcImage.isColorImage()) && (srcImage != grayImage)) {
        	grayImage.disposeLocal();
        	grayImage = null;
        }
        if (error) {
        	setCompleted(false);
        	return;
        }
        setCompleted(true);
        return;
    } // runAlgorithm
    
    private void PowerWatershed_q2(int edges[][],              /*array of node indexes composing edges */
			   int weights[],        /* reconstructed weights */
			   int normal_weights[], /* original weights */
			   int max_weight,            /* maximum weight value */
			   Vector<Integer> seeds,                /* array of seeded nodes indexes */
			   Vector<Short> labels,          /* label values on the seeded nodes */
			   int size_seeds,            /* nb of seeded nodes */ 
			   int rs,                    /* row size */
			   int cs,                    /* col size */
			   int ds,                    /* depth size */
			   int nb_labels,             /* number of different labels */
			   boolean quicksort,            /* true : bucket sort used; false : stochastic sort o(n log n) */
			   ModelImage img_proba) /* output image of potential/proba map x minimizing Epq*/
/*==================================================================================================================*/
/*returns the result x of the energy minimization : min_x lim_p_inf sum_e_of_E w_{ij}^p |x_i-x_j|^2 */
{  
int i, j, k, x, y, e1, e2, re1,re2, p, xr;
int N = rs * cs * ds;                /* number of vertices */   
int M = ds*rs*(cs-1)+ds*(rs-1)*cs+(ds-1)*cs*rs;    /* number of edges*/ 
double val;
int argmax;
int nb_vertices, e_max, Ne_max, nb_edges, Nnb_edges;
int nb_neighbor_edges = 6;
if(ds>1) nb_neighbor_edges = 12;
boolean success = false;
boolean different_seeds;
int wmax;
Lifo LIFO;
Lifo LCP;
boolean indic_E[]; 
boolean indic_P[]; 
int indic_VP[];
int Rnk[]; 
int Fth[];
int local_seeds[];
int LCVP[];
int Es[];
int NEs[];

LIFO = CreeLifoVide(M);
if (LIFO == null) { MipavUtil.displayError("LIFO CreeLifoVide failed in memory_allocation_PW"); error = true; return;}

LCP = CreeLifoVide(M);
if (LCP == null) { MipavUtil.displayError("LCP CreeLifoVide failed in memory_allocation_PW"); error = true; return; }

indic_E = new boolean[M];
indic_P = new boolean[M];
indic_VP = new int[N];
Rnk = new int[N];
Fth = new int[N];
local_seeds = new int[N];
LCVP = new int[N]; // vertices of a plateau. 
Es = new int[M];
NEs = new int[M]; 


float proba[][] = new float[nb_labels-1][N];
for (i = 0; i < nb_labels-1; i++) {
	for (j = 0; j < N; j++) {
		proba[i][j] = -1;
	}
}

int edgesLCP[][] = new int[2][M];

for (i=0;i<size_seeds;i++)
for (j=0;j<nb_labels-1;j++)
{
if (labels.get(i)==j+1)
proba[j][seeds.get(i)] = 1;
else proba[j][seeds.get(i)] = 0;
}

for(k=0;k<N;k++) Fth[k]=k;

float local_labels[][] = new float[nb_labels-1][N];


int sorted_weights[] = new int[M];

for(k=0;k<M;k++) 
{ 
sorted_weights[k]=weights[k]; 
Es[k]=k;
}
if (quicksort == true)
BucketSort(sorted_weights, Es, M, max_weight+1);
else 
TriRapideStochastique_dec(sorted_weights,Es, 0,M-1); 
int cpt_aretes = 0;
int Ncpt_aretes = 0;

/* beginning of main loop */   
while (cpt_aretes < M)
{
do 
{
e_max=Es[cpt_aretes];
cpt_aretes=cpt_aretes+1;
if(cpt_aretes==M) break;
}while(indic_E[e_max]==true);

if(cpt_aretes==M) break;

//1. Computing the edges of the plateau LCP linked to the edge e_max
LifoPush(LIFO, e_max);
indic_P[e_max]=true;
indic_E[e_max]=true;
LifoPush(LCP, e_max);
nb_vertices=0;
nb_edges = 0;
wmax = weights[e_max];

// 2. putting the edges and vertices of the plateau into arrays 
while (! LifoVide(LIFO))
{
x = LifoPop(LIFO);
e1 = edges[0][x]; e2 = edges[1][x];
re1 = element_find(e1, Fth );
re2 = element_find(e2, Fth );
if (proba[0][re1]<0 || proba[0][re2]<0) 
 {
   if (indic_VP[e1]==0) 
	{
	  LCVP[nb_vertices]=e1;
	  nb_vertices++;
	  indic_VP[e1]=1;
	}
   if (indic_VP[e2]==0) 
	{
	  LCVP[nb_vertices]=e2;
	  nb_vertices++;
	  indic_VP[e2]=1;
	}
   edgesLCP[0][ nb_edges] = e1;
   edgesLCP[1][ nb_edges] = e2;
   NEs[nb_edges]=x;
 
   nb_edges ++;
 }

for (k = 1; k <= nb_neighbor_edges; k++) 
 {
   if (ds>1)
	y = neighbor_edge_3D(e1, e2, x, k, rs, cs, ds);
     else y = neighbor_edge(x, k, rs, cs, ds);
   if (y != -1)
	if ((indic_P[y]==false) && (weights[y] == wmax))
	  {
	    indic_P[y]=true;
	    LifoPush(LIFO, y);
	    LifoPush(LCP, y);
	    indic_E[y]= true;
	  } 
 }
}
for (j=0;j<nb_vertices;j++)
indic_VP[LCVP[j]]=0;
for (j=0;j<LCP.getSp();j++) 
indic_P[LCP.getPts(j)]=false;

// 3. If e_max belongs to a plateau
if (nb_edges > 0)
{
// 4. Evaluate if there are differents seeds on the plateau
p=0;  
different_seeds = false;

for (i=0;i<nb_labels-1;i++)
 { 
   val = -0.5;
   for (j=0;j<nb_vertices;j++)
	{
	  
	  x = LCVP[j];
	  xr = element_find(x, Fth);
	  if(Math.abs(proba[i][xr]-val)>epsilon && proba[i][xr]>=0 ) 
	    {
	      p++; val = proba[i][xr]; 
	    }
	}
   if (p>=2) 
	{
	  different_seeds = true;
	  break;
	}
   else p=0;
 }

if (different_seeds == true)
 {
   // 5. Sort the edges of the plateau according to their normal weight
   for(k=0;k<nb_edges;k++)
	sorted_weights[k]=normal_weights[NEs[k]]; 
		
 
   
   if (quicksort == true)
	BucketSort(sorted_weights, NEs, nb_edges , max_weight+1);
   else 
	TriRapideStochastique_dec(sorted_weights,NEs, 0,nb_edges-1); 


   // Merge nodes for edges of real max weight
   nb_vertices=0;
   Nnb_edges = 0;
   for(Ncpt_aretes = 0; Ncpt_aretes< nb_edges; Ncpt_aretes++)
	{
	  Ne_max=NEs[Ncpt_aretes];
	  e1 = edges[0][ Ne_max];
	  e2 = edges[1][ Ne_max];
	  if (normal_weights[Ne_max] != wmax)
	    merge_node (e1, e2,  Rnk, Fth, proba, nb_labels);
	  else 
	    {
	      re1 = element_find(e1, Fth );
	      re2 = element_find(e2, Fth );
	      if ((re1 !=re2)&&((proba[0][re1]<0 || proba[0][re2]<0)))
		{
		  if (indic_VP[re1]==0) 
		    {
		      LCVP[nb_vertices]=re1;
		      nb_vertices++;
		      indic_VP[re1]=1;
		    }
		  if (indic_VP[re2]==0) 
		    {
		      LCVP[nb_vertices]=re2;
		      nb_vertices++;
		      indic_VP[re2]=1;
		    }
		  edgesLCP[0][ Nnb_edges] = re1;
		  edgesLCP[1][ Nnb_edges] = re2;
		  Nnb_edges ++;
		}
	    }
	}
   for (i=0;i<nb_labels-1;i++)
	{ 
	  k=0;
	  for (j=0;j<nb_vertices;j++)
	    {
	      xr = LCVP[j];
	      if (proba[i][xr]>=0)
		{
		  local_labels[i][k] = proba[i][xr];
		  local_seeds[k] = xr;
		  k++;
		}
	    }
	}
	      
   // 6. Execute Random Walker on plateaus

   if(nb_vertices<SIZE_MAX_PLATEAU)
	success = RandomWalker(edgesLCP, Nnb_edges, LCVP, indic_VP, nb_vertices, local_seeds, local_labels, k, nb_labels, proba);
    if (error) {
    	return;
    }
   if ((nb_vertices>=SIZE_MAX_PLATEAU)||(success==false))
	{ 
	  Preferences.debug("Plateau of a big size ( " + nb_vertices + " vertices, " + Nnb_edges + " edges) the RW is not performed on it\n",
			  Preferences.DEBUG_ALGORITHM);
	  for (j=0;j<Nnb_edges;j++)
	    {
	      e1 = edgesLCP[0][j];
	      e2 = edgesLCP[1][j];
	      merge_node (e1, e2,  Rnk, Fth, proba, nb_labels);
	    }
	}
 
   for (j=0;j<nb_vertices;j++)
	indic_VP[LCVP[j]]=0;
 }
else // if different seeds = false 
 // 7. Merge nodes for edges of max weight
 {
   for (j=0;j<nb_edges;j++)
	{
	  e1 = edgesLCP[0][j];
	  e2 = edgesLCP[1][j];
	  merge_node (e1, e2,  Rnk, Fth, proba, nb_labels);
	}
 }
}
LifoFlush(LCP);
} // end main loop

//building the final proba map (find the root vertex of each tree)
for (i=0; i<N; i++) 
{
j=i;
xr = i;
while(Fth[i] != i)
{ 
i = xr;
xr = Fth[i];
}
for(k=0; k< nb_labels-1;k++) proba[k][j] =proba[k][i];
i=j;
}

//writing results

short Temp[] = new short[N];
short Temp2[] = null;
if (produceProbaImage) {
	Temp2 = new short[N];
}

double maxi;
for (j = 0; j < N; j++)
{
maxi=0; argmax = 0; val =1;
for(k=0; k< nb_labels-1;k++)
{
if(proba[k][j]> maxi) 
  { 
    maxi = proba[k][j] ;
    argmax = k;
  }
val = val - proba[k][j];

}  
if (val>maxi) argmax = k;
Temp[j] = (short)(((argmax)*255)/(nb_labels-1));
}
try {
	destImage.importData(0, Temp, true);
}
catch(IOException e) {
	MipavUtil.displayError("IOException " + e + "on destImage.importData(0, Temp, true)");
	error = true;
	return;
}

if (produceProbaImage) {
for (j = 0; j < N; j++)
Temp2[j] = (short)(255-255*proba[0][j]); 
try {
	img_proba.importData(0, Temp2, true);
}
catch(IOException e) {
	MipavUtil.displayError("IOException " + e + " on img_proba.importData(0, Temp2, true");
	error = true;
	return;
}
}   

// free memory 
LifoTermine(LCP);
LifoTermine(LIFO);

for (i=0;i<2;i++) 
edges[i] = null; 
edges = null;

for (i=0;i<2;i++) 
edgesLCP[i] = null; 
edgesLCP = null;

Rnk = null;
local_seeds = null;
for (i=0; i<nb_labels-1; i++) 
local_labels[i] = null;
local_labels = null;

LCVP =null;
Es = null;
NEs = null;
indic_E = null;
indic_VP = null;
indic_P = null;
Fth = null;
for (i=0; i<nb_labels-1; i++)  
		proba[i] = null;
proba = null;
sorted_weights = null;

return;
}
    
    private boolean RandomWalker(int index_edges[][],          /* list of edges */
  		  int M,                      /* number of edges */
  		  int index[],                /* list of vertices */
  		  int indic_vertex[],         /* boolean array of vertices */
  		  int N,                      /* number of vertices */  
  		  int index_seeds[],           /* list of nodes that are seeded*/
  		  float boundary_values[][], /* associated values for seeds (labels)*/
  		  int numb_boundary,          /* number of seeded nodes */
  		  int nb_labels,              /* number of possible different labels values */
  		  float proba[][])          /* output : solution to the Dirichlet problem */
  /*===========================================================================================*/
  /*
  Function RandomWalker computes the solution to the Dirichlet problem (RW potential function) 
  on a general graph represented by an edge list, given boundary conditions (seeds, etc.) 
  */
  {
    int i, j, v1, v2; 
    int k, l;
    boolean seeded_vertex[] = new boolean[N];
    int indic_sparse[] = new int[N];
    int nb_same_edges[] = new int[M];
    
    // Indexing the edges, and the seeds 
    for (i=0;i<N; i++)
      indic_vertex[index[i]] = i;
    
      for (j=0;j<M; j++)
        {
  	v1 = indic_vertex[index_edges[0][j]];
  	v2 = indic_vertex[index_edges[1][j]];
  	if(v1<v2)
  	  {
  	    for (i=0;i<2; i++)
  	    {
  	      index_edges[i][j] = indic_vertex[index_edges[i][j]]; 
  	      indic_sparse[index_edges[i][j]]++;
  	    }
  	  }
  	else   
  	  {
  	    index_edges[1][j] = v1 ;
  	    index_edges[0][j] = v2 ;
  	    indic_sparse[index_edges[0][j]]++;
  	    indic_sparse[index_edges[1][j]]++;
  	  }
        }
      TriEdges (index_edges, M,   nb_same_edges);

    for (i=0;i<numb_boundary;i++)
      {
        index_seeds[i] = indic_vertex[index_seeds[i]]; 
        seeded_vertex[index_seeds[i]]= true;
      }
    
    // Original code used CSparse by Timothy A. Davis that requires a GNU Lesser General Public License
    // which can only be distributed as libraries in a binary format 
    // Therefore, use LUSOL instead of CSparse
    // a for value, indc for row index, indr for column index, nelem for number of elements, m for number of rows,
    // n for number of columns
    //The system to solve is A x = -B X2 

    // building matrix A : laplacian for unseeded nodes
    int lena = 1000000;
    //double A_a[] = new double[2*M+N];
    //int A_indc[] =  new int[2*M+N];
    //int A_indr[] = new int[2*M+N];
    double A_a[] = new double[lena];
    int A_indc[] = new int[lena];
    int A_indr[] = new int[lena];
    int A_nelem[] = new int[1];
    int A_m[] = new int[1];
    int A_n[] = new int[1];
    
    if (fill_A(A_a, A_indc, A_indr, A_nelem, A_m, A_n, N, M, numb_boundary, index_edges, seeded_vertex,
    		indic_sparse, nb_same_edges) == true) 
    {
    	
      //int lena = Math.max(2*A_nelem[0], Math.max(10*A_m[0], Math.max(10*A_n[0], 10000)));
      int luparm[] = new int[30];
      double parmlu[] = new double[30];
      double factol = 2.0;    // > 1.0
      int TPiv  = 0;     // 0=TPP      1=TRP      2=TCP
      int iprint = 20;
      final int maxm = 100000;
      final int maxn = 100000;
      int lenc[] = new int[maxn];
      int lenr[] = new int[maxm];
      int p[] = new int[maxm];
      int q[] = new int[maxn];
      int iploc[] = new int[maxn];
      int iqloc[] = new int[maxm];
      int ipinv[] = new int[maxm];
      int iqinv[] = new int[maxn];
      int locc[] = new int[maxn];
      int locr[] = new int[maxm];
      double w[] = new double[maxn];
      int inform[] = new int[1];
      LUSOL lu = new LUSOL();
      double x[] = new double[A_m[0]];
      double X[] = new double[numb_boundary];
      double b[] = new double[N-numb_boundary];
      	  
      	// ------------------------------------------------------------------
      		  // Set parameters for LUSOL's lu1fac.
      		  // ------------------------------------------------------------------
      		  luparm[0] = iprint;     // File number for printed messages
      		  luparm[1] = 10;         // Print level. >= 0 to get singularity info.
      		                          //              >=10 to get more LU statistics.
      		                          //              >=50 to get info on each pivot.
      		  luparm[2] = 5;          // maxcol
      		  luparm[5] = TPiv;       // Threshold Pivoting: 0 = TPP, 1 = TRP, 2 = TCP
      		  luparm[7] = 1;          // keepLU
      		  parmlu[0] = factol;     // Ltol1:  max |Lij| during Factor
      		  parmlu[1] = factol;     // Ltol2:  max |Lij| during Update 
      		  parmlu[2] = 3.0e-13;    // small:  drop tolerance
      		  parmlu[3] = 3.7e-11;    // Utol1:  absolute tol for small Uii
      		  parmlu[4] = 3.7e-11;    // Utol2:  relative tol for small Uii
      		  parmlu[5] = 3.0;        // Uspace: 
      		  parmlu[6] = 0.3;        // dens1
      		  parmlu[7] = 0.5;        // dens2
      		  
      		 // ------------------------------------------------------------------
      		  // Factor  A = L U.
      		  // U is stored by rows at the start of A_a, A_indr
      		  // L is stored by cols at the end of A_a, A_indc
      		  // ------------------------------------------------------------------
      	
      		  lu.lu1fac( A_m[0]    , A_n[0]    , A_nelem[0], lena , luparm, parmlu,
      		               A_a    , A_indc , A_indr , p    , q     , 
      		               lenc , lenr , locc , locr ,           
      		               iploc, iqloc, ipinv, iqinv, w     , inform );
      		if (inform[0] > 1) {
      		     MipavUtil.displayError("lu1fac error inform[0] = " + inform[0] + "\n");
      		     error = true;
      		     return false;
      		  }

    	// building boundary matrix B   
    	double B_a[] = new double[2*M+N];
        int B_indc[] =  new int[2*M+N];
        int B_indr[] = new int[2*M+N];
        int B_nelem[] = new int[1];
        int B_m[] = new int[1];
        int B_n[] = new int[1];
        fill_B(B_a, B_indc, B_indr, B_nelem, B_m, B_n, N, M, numb_boundary,   
        		index_edges, seeded_vertex, indic_sparse , nb_same_edges);
        
       // building the right hand side of the system
        for(l=0;l<nb_labels-1;l++)
  	{
  	  // building vector X 
  	  int rnz=0;
  	  for (i=0;i<numb_boundary;i++)
  	    {
  	      X[rnz++] = boundary_values[l][i];
  	    }
  	  
  	  
  	  // B * X
  	  for (i = 0; i < b.length; i++) {
  		  b[i] = 0.0;
  	  }
  	  for (i = 0; i < B_nelem[0]; i++) {
  	      b[B_indc[i]-1] -= B_a[i]*X[B_indr[i]-1];	 
  	  }
  	  
  	
  		 
  		  
  		int mode   = 5;
  	// SOLVE  A x = b.
    // via    L b(new) = b
    // and    U x = b(new).

  		lu.lu6sol( mode, A_m[0], A_n[0], b, x, 
  	               lena, luparm, parmlu,
  	               A_a, A_indc, A_indr, p, q,
  	               lenc, lenr, locc, locr,
  	               inform );
  		
  		int cpt=0;
    	  for(k=0;k<N;k++)
    	    if (seeded_vertex[k]== false)
    	      {
    		proba[l][index[k]]= (float)b[cpt];
    		cpt++;
    	      }

    	  //Enforce boundaries exactly
    	  for(k=0;k<numb_boundary;k++) 
    	    proba[l][index[index_seeds[k]]]= boundary_values[l][k];
  	} // for(l=0;l<nb_labels-1;l++)
        return true;
    } // if (fill_A
    
    return false;
  }
    
   private boolean fill_A(double A_a[],                 /* matrix A values to fill */
		    int A_indc[], /* matrix A row values to fill */
		    int A_indr[], /* matrix A column values to fill */
		    int A_nelem[], /* matrix A number of entries */
		    int A_m[], /* matrix A number of rows */
		    int A_n[], /* matrix A number of columns */
    	    int N,                  /* nb of nodes */
    	    int M,                  /* nb of edges */
    	    int numb_boundary,      /* nb of seeds */
    	    int index_edges[][],      /* array of node index composing edges */
    	    boolean seeded_vertex[],   /* index of seeded nodes */
    	    int indic_sparse[],     /* array of index separating seeded and unseeded nodes */
    	    int nb_same_edges[])    /* indicator of same edges presence */
    /*===============================*/
     // building matrix A (laplacian for unseeded nodes)
    {
      int k;
      int rnz = 0;

      // fill the diagonal
      for (k=0;k<N; k++)
        if (seeded_vertex[k]==false)
          {
    	  A_a[rnz] = indic_sparse[k]; //value
    	  A_indc[rnz] = rnz+1 ; //position 1
    	  A_indr[rnz] = rnz+1 ; //position 2
    	  rnz ++;
          }

       int rnzs = 0;
       int rnzu = 0;

      for (k=0;k<N; k++)
        if (seeded_vertex[k]==true)
          {
    	indic_sparse[k]=rnzs;
    	rnzs++;
          }
        else 
          {
    	indic_sparse[k]=rnzu;
    	rnzu++;
          }

      for (k=0;k<M; k++)
        {
          if ((seeded_vertex[index_edges[0][k]] == false)&&(seeded_vertex[index_edges[1][k]] == false))
    	{
    	  A_a[rnz] =  - nb_same_edges[k]-1;
    	  A_indc[rnz] =  indic_sparse[index_edges[0][k]]+1;
    	  A_indr[rnz] =  indic_sparse[index_edges[1][k]]+1;
    	  rnz ++;
    	  A_a[rnz] =  - nb_same_edges[k]-1;
    	  A_indr[rnz] =  indic_sparse[index_edges[0][k]]+1;
    	  A_indc[rnz] =  indic_sparse[index_edges[1][k]]+1;
    	  rnz ++;
    	  k = k + nb_same_edges[k];
    	}
        }
      A_nelem[0] = rnz ;
      A_m[0] = N-numb_boundary;
      A_n[0] = N-numb_boundary;
      return true;
     
    }
   
   private void fill_B(double B_a[],                 /* matrix B values to fill */
		    int B_indc[], /* matrix B row values to fill */
		    int B_indr[], /* matrix B column values to fill */
		    int B_nelem[], /* matrix B number of entries */
		    int B_m[], /* matrix B number of rows */
		    int B_n[], /* matrix B number of columns */
		    int N,                /* nb of nodes */
		    int M,                /* nb of edges */
		    int numb_boundary,    /* nb of seeds */
		    int  index_edges[][],    /* array of node index composing edges */
		    boolean seeded_vertex[], /* index of seeded nodes */
		    int indic_sparse[],   /* array of index separating seeded and unseeded nodes */
		    int nb_same_edges[])  /* indicator of same edges presence */
	/*=======================================================================================*/
	// building matrix B (laplacian for seeded nodes)
	{
	  int k;
	  int rnz; 

	  rnz = 0;
	  for (k=0;k<M; k++)
	    {
	      if (seeded_vertex[index_edges[0][k]] == true)
		{
		  B_a[rnz] = - nb_same_edges[k]-1;
		  B_indr[rnz] = indic_sparse[index_edges[0][k]]+1;
		  B_indc[rnz] = indic_sparse[index_edges[1][k]]+1;
		  rnz++;
		  k=k+ nb_same_edges[k];
		}
	      else if(seeded_vertex[index_edges[1][k]] == true)
		{
		  B_a[rnz] =  - nb_same_edges[k]-1;;
		  B_indr[rnz] = indic_sparse[index_edges[1][k]]+1;
		  B_indc[rnz] = indic_sparse[index_edges[0][k]]+1;
		  rnz++;
		  k=k+ nb_same_edges[k];
		}
	    }
	 
	  B_nelem[0] = rnz ;
	  B_m[0] = N- numb_boundary;
	  B_n[0] = numb_boundary ;
	  
	}


    
   private void TriEdges(int index_edges[][],   /* array of vertices composing edges */
  	      int M,               /* nb of edges */
  	      int nb_same_edges[]) /* indicator of same edges presence  */
  /*======================================================================*/
  // sort the array of vertices composing edges by ascending node index
  // and fill an indicator of same edges presence
  {
    int i, j, k;
    TriRapideStochastique(  index_edges[0],  index_edges[1], 0, M-1);
    i=0;
    while(i<M)
      {
        j = i;
        while((i<M-1)&&(index_edges[0][i]==index_edges[0][i+1])) i++;
        int Alength = index_edges[1].length - j;
        int A[] = new int[Alength];
        for (k = 0; k < Alength; k++) {
        	A[k] = index_edges[1][j+k];
        }
        int Ilength = index_edges[0].length - j;
        int I[] = new int[Ilength];
        for (k = 0; k < Ilength; k++) {
        	I[k] = index_edges[0][j+k];
        }
        if (i!=j) TriRapideStochastique(A, I, 0, i-j);
        i++;
      }
    for(i=0;i<M;i++)
      {
        j = 0;
        while((i+j<M-1)&&( index_edges[0][i+j]== index_edges[0][i+j+1] && index_edges[1][i+j]== index_edges[1][i+j+1])) j++;
        nb_same_edges[i] =j;
      }
    
  }

   private void TriRapideStochastique (int A[], int I[], int p, int r)
   /* =============================================================== */
   /* 
     trie les valeurs du tableau A de l'indice p (compris) a l'indice r (compris) 
     par ordre croissant 
   */
   {
     int q; 
     if (p < r)
     {
       q = PartitionStochastique(A, I, p, r);
       TriRapideStochastique (A, I, p, q) ;
       TriRapideStochastique (A, I, q+1, r) ;
     }
   } /* TriRapideStochastique() */
   
   private int PartitionStochastique (int A[], int I[], int p, int r)
   /* =============================================================== */
   /*
     partitionne les elements de A entre l'indice p (compris) et l'indice r (compris)
     en deux groupes : ceux <= A[q] et les autres, avec q tire au hasard dans [p,r].
   */
   {
     int t;
     int t1;
     int q;

     int rand = randomGen.genUniformRandomNum(0, 32767);
     q = p + (rand % (r - p + 1));
     t = A[p];         /* echange A[p] et A[q] */
     A[p] = A[q]; 
     A[q] = t;
     
     t1 = I[p];         /* echange I[p] et I[q] */
     I[p] = I[q]; 
     I[q] = t1;

     return Partitionner(A, I, p, r);
   } /* PartitionStochastique() */

   private int Partitionner(int A[], int I[], int p, int r)
   /* =============================================================== */
   /*
     partitionne les elements de A entre l'indice p (compris) et l'indice r (compris)
     en deux groupes : ceux <= A[p] et les autres.
   */
   {
     int  t;
     int t1;
     int x = A[p];
     int i = p - 1;
     int j = r + 1;
     while (true)
     {
       do j--; while (A[j] > x);
       do i++; while (A[i] < x);
       if (i < j) 
         { 
   	t = A[i];
   	A[i] = A[j];
   	A[j] = t; 
   	t1 = I[i];
   	I[i] = I[j];
   	I[j] = t1; 
         }
       else return j;
     } /* while (true) */   
   } /* Partitionner() */

    
    private void merge_node (int e1,            /* index of node 1 */
   		 int e2,            /* index of node 2 */
   		 int Rnk[],         /* array needed for union-find efficiency */
   		 int Fth[],          /* array for storing roots of merged nodes trees */
   		 float proba[][], /* array for storing the result x */
   		 int nb_labels)     /* nb of labels */
   /*===================================================================================*/
   /* update the result, Rnk and Fth arrays when 2 nodes are merged */
   {
     int k,re1, re2;
     re1 = element_find(e1, Fth );
     re2 = element_find(e2, Fth );
    
     if ((re1 != re2) && (!(proba[0][re1]>=0 && proba[0][re2]>=0))) 
       {
         element_link(re1,re2, Rnk, Fth);
         if (proba[0][re2]>=0 && proba[0][re1]<0) 
   	for(k=0;k<nb_labels-1;k++)
   	  proba[k][re1]= proba[k][re2];
         else if (proba[0][re1]>=0 && proba[0][re2]<0)
   	for(k=0;k<nb_labels-1;k++)
   	  proba[k][re2]= proba[k][re1];
       }
   }

    
    private void TriRapideStochastique_dec (int A[], int I[], int p, int r)
    /* =============================================================== */
    /* 
      trie les valeurs du tableau A de l'indice p (compris) a l'indice r (compris) 
      par ordre decroissant 
    */
    {
      int q; 
      if (p < r)
      {
        q = PartitionStochastique_dec(A, I, p, r);
        TriRapideStochastique_dec (A, I, p, q) ;
        TriRapideStochastique_dec (A, I, q+1, r) ;
      }
    } /* TriRapideStochastique() */
    
    private int PartitionStochastique_dec (int A[], int I[], int p, int r)
    /* =============================================================== */
    /*
      partitionne les elements de A entre l'indice p (compris) et l'indice r (compris)
      en deux groupes : ceux <= A[q] et les autres, avec q tire au hasard dans [p,r].
    */
    {
      int t;
      int t1;
      int q;


      int rand = randomGen.genUniformRandomNum(0, 32767);
      q = p + (rand % (r - p + 1));
      t = A[p];         /* echange A[p] et A[q] */
      A[p] = A[q]; 
      A[q] = t;
      
      t1 = I[p];         /* echange I[p] et I[q] */
      I[p] = I[q]; 
      I[q] = t1;

      return Partitionner_dec(A, I, p, r);
    } /* PartitionStochastique_dec() */
    
    private int Partitionner_dec(int A[], int I[], int p, int r)
    /* =============================================================== */
    /*
      partitionne les elements de A entre l'indice p (compris) et l'indice r (compris)
      en deux groupes : ceux <= A[p] et les autres.
    */
    {
      int  t;
      int t1;
      int x = A[p];
      int i = p - 1;
      int j = r + 1;
      while (true)
      {
        do j--; while (A[j] < x);
        do i++; while (A[i] > x);
        if (i < j) 
          { 
    	t = A[i];
    	A[i] = A[j];
    	A[j] = t; 
    	t1 = I[i];
    	I[i] = I[j];
    	I[j] = t1; 
          }
        else return j;
      } /* while (true) */   
    } /* Partitionner_dec() */



    
    


    
    private int[] grey_weights_PW(ModelImage image, /*IN : image */  
			   int edges[][],      /*IN: array of node indexes composing edges */
			   Vector<Integer> seeds,       /*IN: array of seeded nodes indexes */
			   int size_seeds,    /*IN : nb of seeded nodes */
			   int weights[], /*OUT : array to store the reconstructed weights on the edges */
			   boolean quicksort)    /*IN : true : bucket sort used; false : stochastic sort o(n log n) */
/* ======================================================================================================== */
/* Computes weights inversely proportionnal to the image gradient for grey level (pgm) images 
Returns the normal weights and computes the reconstructed weights in the array weights */

{
    	
    	int i,M, xDim, yDim, zDim;
        xDim = image.getExtents()[0];
        yDim = image.getExtents()[1];
        zDim = 1;
        if (image.getNDims() > 2) {
     	   zDim = image.getExtents()[2];
        }
        int imgLength = xDim * yDim * zDim;
        short img[] = new short[imgLength];
        
        try {
     	   image.exportData(0, imgLength, img);
        }
        catch (IOException e) {
     	   MipavUtil.displayError("IOException " + e + " on image.exportData(0, imgLength, img)");
     	   error = true;
     	   return null;
        }
       
        M = zDim*xDim*(yDim-1)+zDim*(xDim-1)*yDim+(zDim-1)*yDim*xDim;  // number of edges
        
int normal_weights[] = new int[M];
int j,k,n;
int seeds_function[] = new int[M];
int numvoisins = 4;
if (zDim>1) numvoisins = 6;
for (i=0;i<M;i++)
 normal_weights[i]=  255-Math.abs(img[edges[0][i]]-img[edges[1][i]]);

for (j=0;j<size_seeds;j++)
 for (k=1;k<=numvoisins; k++)
   {
	n = neighbor_node_edge(seeds.get(j), k, xDim, yDim, zDim);
	if (n != -1)
	  seeds_function[n]= normal_weights[n];
   } 
gageodilate_union_find(seeds_function, normal_weights, weights, edges, xDim, yDim, zDim, 255, quicksort);

seeds_function = null;
img = null;
return normal_weights;
}

    
    private int[] color_standard_weights_PW(ModelImage image , /* IN : image */
		      int weights[], /* OUT : array to store the values of weights on the edges */
		      int edges[][],       /* IN : array of node indexes composing edges */ 
		      Vector<Integer> seeds,        /* IN : array of seeded nodes indexes */
		      int size_seeds,     /* IN : nb of seeded nodes */
		      int maxi[],         /* OUT : the maximum weight value */
		      boolean quicksort)     /* IN : true : bucket sort used; false : stochastic sort */
/* ================================================================================================================= */
/* Computes weights inversely proportional to the image gradient for 2D color (ppm) images 
Returns the normal weights and computes the reconstructed weights in the array weights */
{
maxi[0] = 0;
int i,M, xDim, yDim, zDim;

xDim = image.getExtents()[0];
yDim = image.getExtents()[1];
if (image.getNDims() == 2) {
	zDim = 1;
}
else {
	zDim = image.getExtents()[2];
}

int wr, wg, wb;
int imgLength = xDim * yDim;
if (image.getNDims() > 2) {
	imgLength *= zDim;
}

short img_r[] = new short[imgLength];
short img_g[] = new short[imgLength];
short img_b[] = new short[imgLength];

try {
	image.exportRGBData(1, 0, imgLength, img_r);
}
catch (IOException e) {
    MipavUtil.displayError("IOException " + e + " on image.exportRGBData(1, 0, imgLength, img_r)");
    error = true;
    return null;
}
try {
	image.exportRGBData(2, 0, imgLength, img_g);
}
catch (IOException e) {
    MipavUtil.displayError("IOException " + e + " on image.exportRGBData(2, 0, imgLength, img_g)");
    error = true;
    return null;
}
try {
	image.exportRGBData(3, 0, imgLength, img_b);
}
catch (IOException e) {
    MipavUtil.displayError("IOException " + e + " on image.exportRGBData(3, 0, imgLength, img_b)");
    error = true;
    return null;
}


M = zDim*xDim*(yDim-1)+zDim*(xDim-1)*yDim+(zDim-1)*yDim*xDim;
int normal_weights[] = new int[M];
for (i=0;i<M;i++)
{
  wr = Math.abs(img_r[edges[0][i]]-img_r[edges[1][i]]) ;
  wg = Math.abs(img_g[edges[0][i]]-img_g[edges[1][i]]) ;
  wb = Math.abs(img_b[edges[0][i]]-img_b[edges[1][i]]) ;
  //weights[i] = wr*wr+wg*wg+wb*wb;
  //if (weights[i]> maxi) (maxi) = weights[i];
  
  weights[i] = 255-wr;
  if (255-wg < weights[i]) weights[i] = 255-wg; 
  if (255-wb < weights[i]) weights[i] = 255-wb;
  maxi[0] = 255;
}

//for (i=0;i<M;i++)
//  normal_weights[i]=*maxi-weights[i];

for (i=0;i<M;i++)
normal_weights[i]=weights[i]; 

int j,k,n;

int seeds_function[] = new int[M];
int numvoisins = 4;
if (zDim>1) numvoisins = 6;

for (j=0;j<size_seeds;j++)
for (k=1;k<=numvoisins; k++)
{
n = neighbor_node_edge(seeds.get(j), k, xDim, yDim, zDim);
if (n != -1)
seeds_function[n]=normal_weights[n];
} 
gageodilate_union_find(seeds_function, normal_weights, weights, edges, xDim, yDim, zDim, maxi[0], quicksort);
seeds_function = null;

img_r = null;
img_g = null;
img_b = null;

return normal_weights;
}

    
    private void MSF_Prim(int edges[][],       /* array of node indexes composing edges */ 
			  int weights[], /* weights values on the edges */
			  Vector<Integer> seeds,         /* array of seeded nodes indexes */
			  Vector<Short> labels,   /* label values on the seeded nodes */
			  int size_seeds,     /* nb of seeded nodes */ 
			  int rs,             /* row size of the image */
			  int cs,             /* col size of the image */
			  int ds,             /* depth size of the image */
			  int nblabels)       /*nb of different labels */		  
/*=====================================================================================*/
/* returns a segmentation performed by Prim's algorithm for Maximum Spanning Forest computation */
{  
int i, u,v, x,y,z, x_1,y_1;                        
int N = rs * cs * ds;                         /* nb of nodes */
int   M = ds*rs*(cs-1)+ds*(rs-1)*cs+(ds-1)*cs*rs; /* number of edges */
int numvoisins = 6;

short G[] = new short[N];
for (i=0;i<size_seeds;i++)
  G[seeds.get(i)] = labels.get(i);
  
         
Rbt L;                           

IndicsInit(M);
L = CreeRbtVide(M);
i=0;
int sizeL = 0;
for(u = 0; u < M; u ++)
  Set(u, 0);
  
for (u=0;u<M;u++)
  {
    if (u==seeds.get(i))
	{
	  for (x=1;x<=numvoisins;x++)
	    {
	      y = neighbor_node_edge(seeds.get(i), x, rs, cs, ds);
	      if ((y != -1) && (!IsSet(y, 1)))
		{ 
		  RbtInsert(L, (double)(255-weights[y]), y);
		  sizeL++;
		  Set(y,1);
		}
	    }
	  i++;
	  if (i == size_seeds) {
		  break;
	  }
	}
  }

while(sizeL != 0)
  {
    u = RbtPopMin(L);
    sizeL--;
    x = edges[0][u];
    y = edges[1][u];
    if(G[x] > G[y])
	{z=x; x=y; y=z;}
    if((Math.min(G[x],G[y]) == 0) && (Math.max(G[x],G[y]) > 0))
	{
	  G[x] = G[y];
	  for(i = 1; i <= numvoisins; i++)
	    {
	      v = neighbor_node_edge(x,i,rs,cs, ds);
	      if ((v != -1) && (!IsSet(v, 1)))
		{
		  x_1 = edges[0][v];
		  y_1 = edges[1][v];
		  if((Math.min(G[x_1],G[y_1]) == 0) && (Math.max(G[x_1],G[y_1]) > 0))
		    {
		      RbtInsert(L, (double)(255-weights[v]), v);
		      sizeL++;
		      Set(v, 1);
		    }	  
		}
	    }
	}
    UnSet(u, 1);
  }

short F[] = new short[N];

for(i = 0; i < N; i++)
  F[i]= (short)(255*(G[i]-1)/(nblabels-1));

try {
	destImage.importData(0, F, true);
}
catch(IOException e) {
	MipavUtil.displayError("IOException " + e + " in destImage.importData(0, F, true)");
	error = true;
	return;
}

Indics = null;
G = null;
RbtTermine(L);
for (i=0;i<2;i++) 
	edges[i] = null; 
edges = null;     
return;
}
    
   private int RbtPopMin(
    		  Rbt T)
    		/* ==================================== */
    		/* 
    		  Retire de l'arbre l'element de cle min.
    		  ATTENTION: pas de test arbre vide.
    		*/
    		{
    		  RbtElt z = T.getRoot();
    		  while (z.getLeft() != T.getNil()) z = z.getLeft(); /* recherche le min */
    		  z = RbtDeleteAux(T, z);                /* efface de l'arbre */
    		  z.setRight(T.getLibre());
    		  T.setLibre(z);
    		  T.setUtil(T.getUtil() - 1);
    		  return z.getAuxdata();
    		} /* RbtPopMin() */
   
   /* ==================================== */
   private RbtElt RbtDeleteAux(         /* return deleted node */
     Rbt T, RbtElt z)
   /* ==================================== */
   {
     RbtElt c;
     RbtElt d;

     Preferences.debug("RbtDeleteAux \n", Preferences.DEBUG_ALGORITHM);

     if ((z.getLeft() == T.getNil()) || (z.getRight() == T.getNil()))
       {   d = z;
         Preferences.debug("d=z \n", Preferences.DEBUG_ALGORITHM);
   }
     else 
       {
       d = RbtSuccessor(T, z);
       Preferences.debug("d=succ \n", Preferences.DEBUG_ALGORITHM);
       }
     if (d.getLeft() != T.getNil())
       {
         c = d.getLeft();
         Preferences.debug("1 : c = " + c.getAuxdata() + "\n");
       }
     else 
       {
       c = d.getRight();
       Preferences.debug("2 : d = " + d.getAuxdata()+ " c = " + c.getAuxdata() + "\n", Preferences.DEBUG_ALGORITHM);
       }
     c.setParent(d.getParent());      /* no test for NIL with sentinel */

     if (d.getParent() == T.getNil())
       T.setRoot(c);
     else 
     {
       if (d == d.getParent().getLeft())
         d.getParent().setLeft(c);
       else 
         d.getParent().setRight(c);
     }

     if (d != z)
     {
       z.setKey(d.getKey());
       z.setAuxdata(d.getAuxdata());
     }
     if (d.getColor() == RBT_Black)
       RbtDeleteFixup(T, c);     /* c is now "Double-Black" */

   Preferences.debug("Finish RbtDeleteAux\n", Preferences.DEBUG_ALGORITHM);

     return d;
   } /* RbtDeleteAux() */
   
   private void RbtDeleteFixup(
		   Rbt T, RbtElt x)
		 /* ==================================== */
		 {
		   RbtElt s;

		 Preferences.debug("RbtDeleteFixup \n", Preferences.DEBUG_ALGORITHM);
		 Preferences.debug("RbtDeleteFixup " + x.getAuxdata() + " " + x.getKey() + "\n", Preferences.DEBUG_ALGORITHM);
		   while ((x != T.getRoot()) && (x.getColor() == RBT_Black))
		   {
		     if (x == x.getParent().getLeft())
		     {
		       s = x.getParent().getRight();               /* Get x's sibling */
		       if (s.getColor() == RBT_Red)
		       {
		         s.setColor(RBT_Black);              /* Case I */
		         x.getParent().setColor(RBT_Red);
		         LeftRotate(T, x.getParent());
		         s = x.getParent().getRight();
		       }
		       if ((s.getLeft().getColor() == RBT_Black) && (s.getRight().getColor() == RBT_Black))
		       {
		         s.setColor(RBT_Red);                /* Case II */
		         x = x.getParent();
		       }              
		       else 
		       {
		         if (s.getRight().getColor() == RBT_Black)
		 	{
		           s.getLeft().setColor(RBT_Black);      /* Case III */
		           s.setColor(RBT_Red);                        
		           RightRotate(T,s);
		           s = x.getParent().getRight();
		         }
		         s.setColor(x.getParent().getColor());   /* Case IV */
		         x.getParent().setColor(RBT_Black);
		         s.getRight().setColor(RBT_Black);
		         LeftRotate(T, x.getParent());                   
		         x = T.getRoot();
		       }
		     }
		     else
		     {            /* Same as "then" with right and left swapped */
		       s = x.getParent().getLeft();               /* Get x's sibling */
		       if (s.getColor() == RBT_Red)
		       {
		         s.setColor(RBT_Black);              /* Case I */
		         x.getParent().setColor(RBT_Red);
		         RightRotate(T, x.getParent());
		         s = x.getParent().getLeft();
		       }
		       if ((s.getRight().getColor() == RBT_Black) && (s.getLeft().getColor() == RBT_Black))
		       {
		         s.setColor(RBT_Red);                /* Case II */
		         x = x.getParent();
		       }              
		       else 
		       {
		         if (s.getLeft().getColor() == RBT_Black)
		 	{
		           s.getRight().setColor(RBT_Black);     /* Case III */
		           s.setColor(RBT_Red);                        
		           LeftRotate(T,s);
		           s = x.getParent().getLeft();
		         }
		         s.setColor(x.getParent().getColor());   /* Case IV */
		         x.getParent().setColor(RBT_Black);
		         s.getLeft().setColor(RBT_Black);
		         RightRotate(T, x.getParent());                   
		         x = T.getRoot();
		       }
		     }
		   } /* while */
		   x.setColor(RBT_Black);

		 Preferences.debug("Finished RbtDeleteFixup\n", Preferences.DEBUG_ALGORITHM);

		 } /* RbtDeleteFixup() */
   
   static void LeftRotate(
		   Rbt T, RbtElt x)
		 /* ==================================== */
		 {
		   RbtElt y;

		   y = x.getRight();                    /* assume right(x) != NIL */
		   x.setRight(y.getLeft());              /* move y's child over */
		   if (y.getLeft() != T.getNil())
		     y.getLeft().setParent(x);
		   y.setParent(x.getParent());           /* move y up to x's position */
		   if (x.getParent() == T.getNil())
		     T.setRoot(y);
		   else 
		   {
		     if (x == x.getParent().getLeft())
		       x.getParent().setLeft(y);
		     else x.getParent().setRight(y);
		   }
		   y.setLeft(x);                     /* move x down */
		   x.setParent(y);
		 } /* LeftRotate() */

		 /* ==================================== */
		 static void RightRotate(
		   Rbt T, RbtElt x)
		 /* ==================================== */
		 {
		   RbtElt y;

		   y = x.getLeft();              /* assume left(x) != NIL */
		   x.setLeft(y.getRight());
		   if (y.getRight() != T.getNil())
		     y.getRight().setParent(x);
		   y.setParent(x.getParent());
		   if (x.getParent() == T.getNil())
		     T.setRoot(y);
		   else 
		   {
		     if (x == x.getParent().getRight())
		        x.getParent().setRight(y);
		     else x.getParent().setLeft(y);
		   }
		   y.setRight(x);
		   x.setParent(y);
		 } /* RightRotate() */


   
   /* ==================================== */
   private RbtElt RbtSuccessor(
     Rbt T, RbtElt x)
   /* ==================================== */
   {
     RbtElt y;
     if (x.getRight() != T.getNil()) return RbtMinimum(T, x.getRight());
     y = x.getParent();
     while ((y != T.getNil()) && (x == y.getRight()))
     {
       x = y;
       y = y.getParent();
     }
     return y;
   } /* RbtSuccessor() */
   
   private RbtElt RbtMinimum(
		   Rbt T, RbtElt x)
		 /* ==================================== */
		 {
		   while (x.getLeft() != T.getNil()) {
			   x = x.getLeft();
		   }
		   return x;
		 } /* RbtMinimum() */




    
    private RbtElt RbtInsert(
    		  Rbt T, double k, int d)
    		/* ==================================== */
    		{
    		  RbtElt x;
    		  RbtElt xc; /* pour retourner le pointeur sur l'element alloue */
    		  RbtElt uncle;

    		Preferences.debug("RbtInsert: data = " + d + " key = " + k + "\n");

    		  xc = x = RbtInsertAux(T, k, d);          /* allocation et insertion simple */
    		  x.setColor(RBT_Red);

    		  /* re-equilibrage de l'arbre */
    		 while ((x != T.getRoot()) && (x.getParent().getColor() == RBT_Red))
    		  {
    		    if (x.getParent() == x.getParent().getParent().getLeft())
    		    {
    		      uncle = x.getParent().getParent().getRight();
    		      if (uncle.getColor() == RBT_Red)
    		      {
    		        x.getParent().setColor(RBT_Black);                    /* Case I */
    		        uncle.setColor(RBT_Black);
    		        x.getParent().getParent().setColor(RBT_Red);
    		       x = x.getParent().getParent();
    		      }
    		      else 
    		      {
    		        if (x == x.getParent().getRight())
    		        {
    		          x = x.getParent();                             /* Case II */
    		          LeftRotate(T,x);
    		        }
    		        x.getParent().setColor(RBT_Black);                    /* Case III */
    		        x.getParent().getParent().setColor(RBT_Red);
    		        RightRotate(T, x.getParent().getParent());
    		      }
    		    }
    		    else /* same as "then" with "right" and "left" swapped */
    		    {
    		      uncle = x.getParent().getParent().getLeft();
    		      if (uncle.getColor() == RBT_Red)
    		      {
    		        x.getParent().setColor(RBT_Black);                     /* Case I */
    		        uncle.setColor(RBT_Black);
    		        x.getParent().getParent().setColor(RBT_Red);
    		        x = x.getParent().getParent();
    		      }
    		      else 
    		      {
    		       if (x == x.getParent().getLeft())
    		        {
    		          x = x.getParent();                             /* Case II */
    		          RightRotate(T,x);
    		        }
    		        x.getParent().setColor(RBT_Black);                    /* Case III */
    		        x.getParent().getParent().setColor(RBT_Red);
    		        LeftRotate(T, x.getParent().getParent());
    		      }
    		    }
    		  } /* while */
    		 T.getRoot().setColor(RBT_Black);

    		Preferences.debug("Finished RbtInsert xc.getAuxdata() = " + xc.getAuxdata() + " xc.getKey() = " + xc.getKey() + "\n",
    				Preferences.DEBUG_ALGORITHM);

    		  if (xc.getAuxdata() != d) 
    			  Preferences.debug("BUG RbtInsert xc.getAuxdata() = " + xc.getAuxdata() + " d = " + d + "\n",
    					  Preferences.DEBUG_ALGORITHM);

    		  return xc;                      /* modif mc: retourne xc plutot que x (sinon: BUG) */
    		} /* RbtInsert() */
    
    private RbtElt RbtInsertAux(  /* allocation et insertion simple */
    		  Rbt T, double k, int d)
    		/* ==================================== */
    		{
    		  RbtElt z;

    		Preferences.debug("RbtInsertAux\n", Preferences.DEBUG_ALGORITHM);

    		  if (T.getLibre() == null) RbtReAlloc(T);
    		  T.setUtil(T.getUtil()+1);
    		  if (T.getUtil() > T.getMaxutil()) T.setMaxutil(T.getUtil());
    		  z = T.getLibre();
    		  T.setLibre(T.getLibre().getRight());
    		  z.setKey(k);
    		  z.setAuxdata(d);
    		  z.setLeft(T.getNil());
    		  z.setRight(T.getNil());
    		  RbtInsertSimple(T, z);

    		Preferences.debug("Finished RbtInsertAux\n", Preferences.DEBUG_ALGORITHM);

    		  return z;
    		} /* RbtInsertAux() */
    
    
    void RbtInsertSimple(
    		  Rbt T, RbtElt z)
    		/* ==================================== */
    		{
    		  RbtElt x;
    		  RbtElt y;

    		Preferences.debug("RbtInsertSimple  \n", Preferences.DEBUG_ALGORITHM);
    		Preferences.debug("z = " + z + " z.getKey()  = " + z.getKey() + "\n", Preferences.DEBUG_ALGORITHM);

    		  y = T.getNil();
    		  x = T.getRoot();
    		  while (x != T.getNil())
    		  {
    		    y = x;
    		    if (z.getKey() < x.getKey()) x = x.getLeft(); else x = x.getRight();
    		  }
    		  z.setParent(y);
    		  if (y == T.getNil())
    		    T.setRoot(z);
    		  else
    		    if (z.getKey() < y.getKey()) y.setLeft(z); else y.setRight(z);

    		Preferences.debug("Finished RbtInsertSimple\n", Preferences.DEBUG_ALGORITHM);

    		} /* RbtInsertSimple() */

    
    private void RbtReAlloc(Rbt A)
    /* ==================================== */
    {
      int taillemax;
      Rbt T;

    //#ifdef VERBOSE
      //printf("RbtReAlloc: ancienne taille %ld nouvelle taille %ld\n", (*A)->max, 2 * (*A)->max);
    //#endif
      Preferences.debug("RbtReAlloc A.getMax() = " + A.getMax() + "\n", Preferences.DEBUG_ALGORITHM);
      taillemax = 2 * A.getMax();  /* alloue le double de l'ancienne taille */ 
      T = CreeRbtVide(taillemax);
      Preferences.debug("RbtTransRec in RbtReAlloc\n", Preferences.DEBUG_ALGORITHM);
      RbtTransRec(T, A, A.getRoot());
      Preferences.debug("Finished RbtTransRec in RbtReAlloc\n", Preferences.DEBUG_ALGORITHM);
      //A = T;
      RbtCopy(A,T);
    } /* RbtReAlloc() */

    private void RbtTransRec(
    		  Rbt T, Rbt A, RbtElt x)
    		/* ==================================== */
    		{
    		  if (x == A.getNil()) return;
    		  RbtInsert(T, x.getKey(), x.getAuxdata());
    		  RbtTransRec(T, A, x.getLeft());
    		  RbtTransRec(T, A, x.getRight());
    		} /* RbtTransRec() */

    private RbtElt RbtSearch(
    		  Rbt T, double k)
    		/* ==================================== */
    		{
    		  RbtElt x = T.getRoot();
    		  while ((x != T.getNil()) && (k != x.getKey()))
    		    if (k < x.getKey()) x = x.getLeft(); else x = x.getRight();
    		  return x;
    		} /* RbtSearch() */

    private void RbtDelete(
    		  Rbt T, RbtElt z)
    		/* ==================================== */
    		{

    		  Preferences.debug("RbtDelete z.getAuxdata() = " + z.getAuxdata() +  "\n", Preferences.DEBUG_ALGORITHM);

    		  z = RbtDeleteAux(T, z);
    		 
    		  z.setRight(T.getLibre());
    		  T.setLibre(z);
    		  T.setUtil(T.getUtil() - 1);

    		  Preferences.debug("Finished RbtDelete\n", Preferences.DEBUG_ALGORITHM);

    		} /* RbtDelete() */
    
    private RbtElt RbtMaximum(
    		  Rbt T, RbtElt x)
    		/* ==================================== */
    		{
    		  while (x.getRight() != T.getNil()) x = x.getRight();
    		  return x;
    		} /* RbtMaximum() */

    // vide means empty
    private boolean RbtVide(
    		  Rbt T)
    		/* ==================================== */
    		{
    		  return (T.getUtil() == 0);
    		} /* RbtVide() */
    
    private void RbtPrint(
    		  Rbt T)
    		/* ==================================== */
    		{
    		  RbtPrintRec(T, T.getRoot(), 0);
    		} /* RbtPrint() */

    private void RbtPrintRec(
    		  Rbt T, RbtElt x, int niv)
    		/* ==================================== */
    		{
    		  int i;
    		  if (x == T.getNil()) return;
    		  RbtPrintRec(T, x.getLeft(), niv+1);
    		  for (i = 0; i < niv; i++) System.out.print("    ");
    		  System.out.print("x.getKey() = " + x.getKey() + " x.getAuxdata() = " + x.getAuxdata());
    		  if (x.getColor() == RBT_Red) System.out.print(" color = r"); else System.out.print(" color = b");
    		  System.out.print(")\n");
    		  RbtPrintRec(T, x.getRight(), niv+1);
    		} /* RbtPrintRec() */
    
    private void MSF_Kruskal(int edges[][],       /* array of node indexes composing edges */ 
		     int weights[], /* weights values on the edges */
		     int max_weight,     /* maximum weight value */
		     Vector<Integer> seeds,         /* array of seeded nodes indexes */
		     Vector<Short> labels,   /* label values on the seeded nodes */
		     int size_seeds,     /* nb of seeded nodes */ 
		     int rs,             /* row size of the image */
		     int cs,             /* col size of the image */
		     int ds,             /* depth size of the image */
		     int nblabels)       /* nb of different labels */
/*=====================================================================*/
/*returns a segmentation performed by Kruskal's algorithm for Maximum Spanning Forest computation*/
{  
int i, j, k, x, y, e1, e2;
int N, M;

N = rs * cs * ds;                /* number of vertices */   
M = ds*rs*(cs-1)+ds*(rs-1)*cs+(ds-1)*cs*rs; /*number of edges*/
int numvoisins = 6;

int extents[];
if (ds == 1) {
	extents = new int[2];
	extents[0] = rs;
	extents[1] = cs;
}
else {
	extents = new int[3];
	extents[0] = rs;
	extents[1] = cs;
	extents[2] = ds;
}

Lifo LIFO; 
LIFO = CreeLifoVide(M);


int Mrk[] = new int[N];

for (i=0;i<size_seeds;i++){

Mrk[seeds.get(i)] = labels.get(i);
}
int Rnk[] = new int[N];
int Fth[] = new int[N];

for(k=0;k<N;k++) { Fth[k]=k; }

// Es : E sorted by decreasing weights
int Es[] = new int[M];

for(k=0;k<M;k++) Es[k]=k;

int sorted_weights[] = new int[M];
for(k=0;k<M;k++) 
sorted_weights[k]=weights[k]; 
BucketSort (sorted_weights, Es, M, max_weight+1);
sorted_weights = null;

int nb_arete = 0;
int e_max, root;

/* beginning of main loop */  

int cpt_aretes = 0;

while (nb_arete < N-size_seeds)
{
 e_max=Es[cpt_aretes];
 // printf("%d \n", e_max);
 cpt_aretes=cpt_aretes+1;
 e1 = edges[0][e_max];
 e2 = edges[1][e_max];
 x = element_find(e1, Fth );
 y = element_find(e2, Fth );

 if ((x != y) && (!(Mrk[x]>=1 && Mrk[y]>=1)))
{
 root = element_link( x,y, Rnk, Fth);
 nb_arete=nb_arete+1;
 if ( Mrk[x]>=1) Mrk[root]= Mrk[x];
 else if ( Mrk[y]>=1) Mrk[root]= Mrk[y]; 
}
}

//building the map (find the root vertex of each tree)
int Map2[] = new int[N];
int Map[] = new int[N];
for (i=0; i<N; i++) 
Map2[i] = element_find(i, Fth);

boolean Fullseeds[] = new boolean[N];
for (i=0;i<size_seeds;i++)
{
  Fullseeds[seeds.get(i)]= true;
  Map[seeds.get(i)] = (int)labels.get(i);
}

for (i=0;i<N;i++)Mrk[i] = 0;
for (i=0;i<size_seeds; i++) 
{
  LifoPush(LIFO, seeds.get(i));
  while (!LifoVide(LIFO))
{
  x = LifoPop(LIFO);
  Mrk[x]=1;
  for (k=1;k<=numvoisins;k++)
    {
      y = neighbor(x, k, rs, cs, ds);
      if (y != -1)
	 {
	   if (Map2[y]==Map2[seeds.get(i)] && Fullseeds[y]!=true && Mrk[y]==0)
	     {   
	       LifoPush(LIFO, y);
	       Map[y]= labels.get(i);
	       Mrk[y]=1;
	     }
	 }
    }
}  
  LifoFlush(LIFO);
}

short Temp[] = new short[N]; 
for (j = 0; j < N; j++)
Temp[j] = (short)(255*(Map[j]-1)/(nblabels-1));

    try {
    	destImage.importData(0, Temp, true);
    }
    catch(IOException e) {
    	MipavUtil.displayError("IOException " + e + " in destImage.importData(0, Temp, true)");
    	error = true;
    	return;
    }

LifoTermine(LIFO);
for (i=0;i<2;i++) 
    edges[i] = null;
  edges = null;
  Mrk = null;
  Rnk = null;
  Fullseeds = null;
  Es = null;
  Map = null;
  Map2 = null;
  Fth= null;
  return;
}
    
    private int neighbor( int i,  /* node index */
  	      int k,  /* number of the desired neighbor node of node i */
  	      int rs, /* rowsize of the image */
  	      int cs, /* colsize of the image */
  	      int ds) /* depth of the image */
  /* =================================================================== */
  /* 
    return the index of the k_th neighbor 
         5         From the top :
       3 0 1       4: slice below,  2 : slice above
         6 
    return -1 if the neighbor is outside the image */
   
  {
    int rs_cs=rs*cs;
    int z = i / (rs_cs);  
    int x = i / (rs*ds);
    int y = i % (rs);
    
    switch(k)
      {
      case 1:
        if (y >= rs-1) return -1;
        else return i+1;	
      case 3:
        if (y == 0) return -1;
        else return i-1;
      case 2:
        if (x >= cs-1) return -1;
        else return i+rs;
      case 4:
        if (x == 0) return -1;
        else return i-rs;
      case 5:
        if (z >= ds-1) return -1;
        else return i+rs_cs;
      case 6:
        if (z == 0) return -1;
        else return i-rs_cs;
      case 0:
         return rs_cs;
      case -1:
        return rs_cs+1;
      }
     return -1; //never happens 
  }

    
    private void BucketSort (int A[], /* array to sort */
   		 int I[],       /* array such as I[i]=i*/
   		 int r,    /* number of element to sort */
   		 int N)        /* N : nb of buckets */
   /* =============================================================== */
   /*  Sort the r values of array A by descending order */
   {
     int i;
     int H[] = new int[N];
     for (i=0;i<r;i++) 
       H[A[i]]++;
       
     Lifo Bucket[] = new Lifo[N];
     
     for (i=0;i<N;i++)
       Bucket[i] = CreeLifoVide(H[i]);
     
     for (i=0;i<r;i++) 
       LifoPush(Bucket[A[i]], i);
     
     int j=0; 
     for (i=N-1;i>=0;i--) 
       {
         while(!LifoVide(Bucket[i]))
   	{
   	  I[j] = LifoPop(Bucket[i]);
   	  A[j]= i; 
   	  j++; 
   	}
       }
     H = null;
     for (i=0;i<N;i++) LifoTermine(Bucket[i]);
     Bucket = null;
   } 
    
    private int element_link( int x,int y, int Rnk[], int Fth[])
    /*================================================*/  
    {
      if( Rnk[x] > Rnk[y])
        { 
          int t;
          t=x;
          x=y;
          y=t;
        }
      if( Rnk[x] == Rnk[y])
        {
          Rnk[y]=Rnk[y]+1;
        }
      Fth[x] = y;
      return y;
    }




        
        private void grey_weights(ModelImage image, /* image */
     		   int weights[], /* array to store the values of weights on the edges */
     		   int edges[][],       /* array of node indexes composing edges */ 
     		   Vector<Integer> seeds,        /* array of seeded nodes indexes */
     		   int size_seeds,     /* nb of seeded nodes */
     		   boolean geod,          /* if true, geodesic reconstruction is performed */
     		   boolean quicksort)     /* true : bucket sort used; false : stochastic sort o(n log n) */
     /* =============================================================== */
     /* Computes weights inversely proportionnal to the image gradient for grey level images */
     {
       int i,M, xDim, yDim, zDim;
       xDim = image.getExtents()[0];
       yDim = image.getExtents()[1];
       zDim = 1;
       if (image.getNDims() > 2) {
    	   zDim = image.getExtents()[2];
       }
       int imgLength = xDim * yDim * zDim;
       short img[] = new short[imgLength];
       
       try {
    	   image.exportData(0, imgLength, img);
       }
       catch (IOException e) {
    	   MipavUtil.displayError("IOException " + e + " on image.exportData(0, imgLength, img)");
    	   error = true;
    	   return;
       }
      
       M = zDim*xDim*(yDim-1)+zDim*(xDim-1)*yDim+(zDim-1)*yDim*xDim;  // number of edges
       if (geod==false)
         {
           for (i=0;i<M;i++)
     	weights[i]= 255-Math.abs(img[edges[0][i]]-img[edges[1][i]]);
         }
       else 
         {
           int j,k,n;
           int weights_tmp[] = new int[M];
           int seeds_function[] = new int[M];
           int numvoisins = 4;
           if (zDim>1) numvoisins = 6;
           for (i=0;i<M;i++)
     	weights_tmp[i]=255-Math.abs(img[edges[0][i]]-img[edges[1][i]]) ;
     	
           for (j=0;j<size_seeds;j++)
     	for (k=1;k<=numvoisins; k++)
     	  {
     	    n = neighbor_node_edge(seeds.get(j), k, xDim, yDim, zDim);
     	    if (n != -1)
     	      seeds_function[n]= weights_tmp[n];
     	  } 
          
           gageodilate_union_find(seeds_function, weights_tmp, weights, edges, xDim, yDim, zDim, 255, quicksort);
          
           weights_tmp = null;
           seeds_function = null;
         }
       img = null;
     }
    
     private void compute_edges(  int edges[][], /*array of node indexes composing edges */
    				     int rs, /* rowsize of the image */
    				     int cs, /* colsize of the image */
    				     int ds) /* depth of the image */
    		{
    		  int i,j,k,l,M;
    		  M=0; 
    		  int rs_cs = rs*cs;
    		  for(k=0;k<ds;k++) 
    		    {
    		      for(i=0;i<cs;i++) 
    			{
    			  for(j=0;j<rs;j++)
    			    {
    			      if(i<(cs-1))
    				{
    				  edges[0][M]=j+i*rs+k*rs_cs;
    				  edges[1][M]=j+(i+1)*rs+k*rs_cs;
    				  M++;
    				}
    			    }
    			}
    		      for(i=0;i<cs;i++) 
    			{
    			  for(j=0;j<rs;j++)
    			    {
    			      if(j<(rs-1))
    				{
    				  edges[0][M]=j+i*rs+k*rs_cs;
    				  edges[1][M]=j+1+i*rs+k*rs_cs;
    				  M++;
    				}
    			    }
    			}
    		      if (k != ds-1)
    			for(l=k*rs*cs;l<(k+1)*rs_cs;l++) 
    			  {
    			    edges[0][M]=l;
    			    edges[1][M]=l+rs_cs;
    			    M++;
    			  }
    		    }
    		} // compute_edges;

    private int color_standard_weights(ModelImage image , /* image */
			    int weights[], /* array to store the values of weights on the edges */
			    int edges[][],       /* array of node indexes composing edges */ 
			    Vector<Integer> seeds,        /* vector of seeded nodes indexes */
			    int size_seeds,     /* nb of seeded nodes */
			    boolean geod,          /* if true, geodesic reconstruction is performed */
			    boolean quicksort)     /* true : bucket sort used; false : stochastic sort o(n log n) */
/* ================================================================================================== */
/* Computes weights inversely proportionnal to the image gradient for 2D color (ppm) images */
{
int maxi = 0;
int i,M, xDim, yDim, zDim;
xDim = image.getExtents()[0];
yDim = image.getExtents()[1];
if (image.getNDims() == 2) {
	zDim = 1;
}
else {
	zDim = image.getExtents()[2];
}

int wr, wg, wb;
int imgLength = xDim * yDim;
if (image.getNDims() > 2) {
	imgLength *= zDim;
}

short img_r[] = new short[imgLength];
short img_g[] = new short[imgLength];
short img_b[] = new short[imgLength];

try {
	image.exportRGBData(1, 0, imgLength, img_r);
}
catch (IOException e) {
    MipavUtil.displayError("IOException " + e + " on image.exportRGBData(1, 0, imgLength, img_r)");
    error = true;
    return -1;
}
try {
	image.exportRGBData(2, 0, imgLength, img_g);
}
catch (IOException e) {
    MipavUtil.displayError("IOException " + e + " on image.exportRGBData(2, 0, imgLength, img_g)");
    error = true;
    return -1;
}
try {
	image.exportRGBData(3, 0, imgLength, img_b);
}
catch (IOException e) {
    MipavUtil.displayError("IOException " + e + " on image.exportRGBData(3, 0, imgLength, img_b)");
    error = true;
    return -1;
}


M = zDim*xDim*(yDim-1)+zDim*(xDim-1)*yDim+(zDim-1)*yDim*xDim; 
	 
for (i=0;i<M;i++)
 {
   wr = Math.abs(img_r[edges[0][i]]-img_r[edges[1][i]]) ;
   wg = Math.abs(img_g[edges[0][i]]-img_g[edges[1][i]]) ;
   wb = Math.abs(img_b[edges[0][i]]-img_b[edges[1][i]]) ;
   weights[i] = wr*wr+wg*wg+wb*wb;
   if (weights[i]> maxi) (maxi) = weights[i];
 }
if (geod==false)
 { 
   for (i=0;i<M;i++)
	weights[i]= maxi-weights[i];
 }
else
 { 
   int j,k,n;
   int weights_tmp[] = new int[M];
   int seeds_function[] = new int[M];
   int numvoisins = 4;
   if (zDim>1) numvoisins = 6;
   for (i=0;i<M;i++)
	weights_tmp[i]=maxi-weights[i];
	
   for (j=0;j<size_seeds;j++)
	for (k=1;k<=numvoisins; k++)
	  {
	    n = neighbor_node_edge(seeds.get(j), k, xDim, yDim, zDim);
	    if (n != -1)
	      seeds_function[n]=weights_tmp[n];
	  } 
   gageodilate_union_find(seeds_function, weights_tmp, weights, edges, xDim, yDim, zDim, maxi, quicksort);
   weights_tmp = null;
   seeds_function = null;
 }
img_r = null;
img_g = null;
img_b = null;
return maxi;
}
    
    private int neighbor_node_edge( int i,  /* node index */
			int k,  /* number of the desired edge neighbor of node i */
			int rs, /* rowsize of the image */
			int cs, /* colsize of the image */
			int ds) /* depth of the image */
/* ============================================================================== */
/* return the index of the k_th edge neighbor of the node "i" 
       4   
     3 0 1   5 front slice,  6 back slice
       2 
   return -1 if the neighbor is outside the image */
{
  int rs_cs = rs*cs;
  int zp = i % (rs_cs); 
  int z = i / (rs_cs);   
  int V = (cs-1)*rs;
  int H = (rs-1)*cs;
  switch(k)
    {
    case 1:
      if (zp % rs >= rs-1) return -1;
      else return (zp+V)-(zp/rs)+z*(V+H+rs_cs);
    case 3:
      if (zp % rs == 0) return -1;
      else return (zp+V)-(zp/rs)-1+z*(V+H+rs_cs);
    case 2:
      if (zp / rs >= cs-1) return -1;
      else return zp+z*(V+H+rs_cs);
    case 4:
      if (zp / rs == 0) return -1;
      else return zp-rs+z*(V+H+rs_cs);
    case 5:
      if (z == 0) return -1;
      else return z*(V+H)+zp+(z-1)*rs_cs;
    case 6:
      if (z >= ds-1) return -1;
      else return (z+1)*(V+H)+zp+z*rs_cs;
    case -1:
      return i+rs_cs;
    case 0:
     return i + rs_cs*2;
    }
  return -1; //never happens 
}

    private void gageodilate_union_find(int F[],  /* f : image seeds */ 
		     int G[],  /* g : image weights */
		     int O[],  /* O : result of the reconstruction by dilation of g under f */
		     int edges[][],  /* list of couple of vertices forming edges*/
		     int rs,    /* row size */
		     int cs,    /* col size */
		     int ds,    /* depth size */
		     int max_weight, 
		     boolean quicksort)  
/* ===================================================================================================== */
/* reconstruction by dilation of g under f.  Union-find method described by Luc Vicent. */
{

int k, p,i,n;
int M = ds*rs*(cs-1)+ds*(rs-1)*cs+(ds-1)*cs*rs;      /*number of edges*/
boolean Mrk[] = new boolean[M];
int Fth[] = new int[M];

// Es : E sorted by decreasing weights
int Es[] = new int[M]; 

for(k=0;k<M;k++) 
{
 Fth[k]=k; 
 O[k]=F[k];
 F[k]=G[k]; 
 Es[k] = k;
}

MAX = max_weight; 
if(quicksort) BucketSortCroiss (F, Es, M, MAX+1);
else TriRapideStochastique_inc(F, Es, 0, M-1);
/* first pass */
if(ds==1)//2D
{
 for(k=M-1;k>=0;k--)
{
 p = Es[k];
 for (i = 1; i <= 6; i += 1) // parcourt les 6 voisins  
   {
     n = neighbor_edge(p, i, rs, cs, ds);
     if (n != -1)
	if(Mrk[n]) 
	    element_link_geod_dilate(n,p, Fth, G, O);
	  
     Mrk[p]=true;
   }
}
}
else // 3D 
{
 for(k=M-1;k>=0;k--)
{
 p = Es[k];
 for (i = 1; i <= 12; i += 1) // parcourt les 12 voisins  
   {
     n = neighbor_edge_3D(edges[0][p], edges[1][p], p, i, rs, cs, ds);
     if (n != -1)
	if(Mrk[n]) 
	  element_link_geod_dilate(n,p, Fth, G, O);
     Mrk[p]=true;
   }
}
}

/* second pass */

for(k=0;k<M;k++)
{
 p = Es[k];
 if (Fth[p]==p) // p is root
{
 if (O[p]==MAX) O[p]=G[p];
}
 else O[p]= O[Fth[p]];
}

Es = null;
Mrk = null;
Fth = null;
} 
    
    private void element_link_geod_dilate( int n,
		       int p,
		       int Fth[], 
		       int G[],
		       int  O[])
/*================================================*/  
{ 
int r = element_find(n, Fth);

if (r != p)
{
if((G[r] == G[p])||(G[p]>=O[r]))
{
Fth[r] = p;
O[p] = Math.max(O[r],O[p]);
}
else O[p] = MAX;
} 
}
    
    int element_find(int x, int Fth[] )
    /*===============================*/
    { 
      if (Fth[x] != x) 
        Fth[x] = element_find(Fth[x], Fth);  
      return Fth[x];
    }


    
    private int neighbor_edge_3D(int node1, /* index of node 1 */
		     int node2, /* index of node 2 */
		     int i,     /* edge index */
		     int k,     /* number of the desired neighbor of edge i */
		     int rs,    /* rowsize of the image */
		     int cs,    /* colsize of the image */
		     int ds)    /* depth of the image */
/* ======================================================================== */
/* return the index of the k_th neighbor
 
%      3       _|_       
%  1 2 0 4 5   _|_       
%  6 7 0 9 10   |        
%      8                 
%                        
% return -1 if the neighbor is outside the image


% example of indexing of the edges in 3D

%        27      28
%       _____ ________
%     /      /       /|
%  12/    13/     14/ |
%   /__6___/___7__ /  | 23
%   |      |      |   |
%  0|     1|     2|17/|
%   |      |      | / |
%   |__8___|___9__|/  | 26
%   |      |      |   |
%  3|     4|     5|20/
%   |      |      | /
%   |__10__|__11__|/

*/
{
 if (ds==1)  return neighbor_edge( i, k, rs, cs, ds); 
 int index=-1;
 int rs_cs = rs*cs;
 int V = (cs-1)*rs;
 int H = (rs-1)*cs;
 if(k<= 6)
   {
     int zp = node1 % (rs_cs); 
     int z = node1 / (rs_cs);   
     switch(k)
	{
	case 1:
	  if (zp % rs >= rs-1) return -1;
	  else index = (zp+V)-(zp/rs)+z*(V+H+rs_cs);break;
	case 3:
	  if (zp % rs == 0) return -1;
	  else index = (zp+V)-(zp/rs)-1+z*(V+H+rs_cs);break;
	case 2:
	  if (zp / rs >= cs-1) return -1;
	  else index = zp+z*(V+H+rs_cs);break;
	case 4:
	  if (zp / rs == 0) return -1;
	  else index = zp-rs+z*(V+H+rs_cs);break;
	case 5:
	  if (z == 0) return -1;
	  else index = z*(V+H)+zp+(z-1)*rs_cs;break;
	case 6:
	  if (z >= ds-1) return -1;
	  else index = (z+1)*(V+H)+zp+z*rs_cs;break;
	}
   }
 else 
   {
     int zp = node2 % (rs_cs); 
     int z = node2 / (rs_cs);   
     switch(k-6)
	{
	case 1:
	  if (zp % rs >= rs-1) return -1;
	  else index = (zp+V)-(zp/rs)+z*(V+H+rs_cs);break;
	case 3:
	  if (zp % rs == 0) return -1;
	  else index = (zp+V)-(zp/rs)-1+z*(V+H+rs_cs);break;
	case 2:
	  if (zp / rs >= cs-1) return -1;
	  else index = zp+z*(V+H+rs_cs);break;
	case 4:
	  if (zp / rs == 0) return -1;
	  else index = zp-rs+z*(V+H+rs_cs);break;
	case 5:
	  if (z == 0) return -1;
	  else index = z*(V+H)+zp+(z-1)*rs_cs;break;
	case 6:
	  if (z >= ds-1) return -1;
	  else index = (z+1)*(V+H)+zp+z*rs_cs;break;
	}
   }
 if (index == i) return -1;
 return index;
}

    
    private int neighbor_edge(int i,  /* edge index */
  		  int k,  /* number of the desired neighbor of edge i */
  		  int rs, /* rowsize of the image */
  		  int cs, /* colsize of the image */
  		  int ds) /* depth of the image */
  /* =================================================================== */
  /* return the index of the k_th neighbor 
  (only works in 2D, a little faster than the neighbor_edge3D)
    
  %      1       _|_          
  %    2 0 6     _|_          2 1      _|_|_
  %    3 0 5      |         3 0 0 6     | | 
  %      4                    4 5 
  %                           
  % return -1 if the neighbor is outside the image


  % indexing edges 2D
  % 
  %    _4_ _5_
  %  0|  1|
  %   |_6_|_7_
  %  2|  3|
  %   |   |

  */
  {
    int V = (cs-1)*rs; // nb vertical edges 
    if (i >= V)
      {
        //horizontal
        switch(k)
  	{
  	case 2:
  	  if ((i-V) < rs-1) return -1;
  	  else return ((i-V)/(rs-1)-1)*rs + ((i - V)%(rs-1));
  	case 3:
  	  if ((i-V)%(rs-1)==0) return -1;
  	  else return i-1 ;
  	case 4:
  	  if (i>(rs-1)*cs+ V -rs) return -1;
  	  else return ((i-V)/(rs-1)-1)*rs + ((i - V)%(rs-1)) + rs;
  	case 5:
  	  if (i>(rs-1)*cs+ V - rs) return -1;
  	  else return ((i-V)/(rs-1)-1)*rs + ((i - V)%(rs-1))  + rs +1;
  	case 6:
  	  if ((i-V)%(rs-1)==rs-2) return -1;
  	  else return i+1;
  	case 1:
  	  if (i-V<rs-1) return -1;
  	  else return ((i-V)/(rs-1)-1)*rs + ((i - V)%(rs-1))+1;
  	}
      }
    else
      { //vertical
        switch(k)
  	{
  	case 6:
  	  if (i %rs == rs-1) return -1;
  	  else return (i+V)-(i/rs);
  	case 1:
            if (i < rs) return -1;
            else return i-rs;
          case 2:
            if (i%rs==0) return -1;
            else return (i+V)-(i/rs)-1;
          case 3:
            if (i%rs==0) return -1;
            else  return (i+V)-(i/rs)-1+rs-1;
          case 4:
            if (i>=V-rs) return -1;
            else return i+rs;
          case 5:
  	  if (i %rs == rs-1) return -1;
  	  else return (i+V)-(i/rs)+rs-1;
  	}
      }
    return -1; //never happens 
  }

    
   private void TriRapideStochastique_inc (int A[], int I[], int p, int r)
    /* =============================================================== */
    /* 
      trie les valeurs du tableau A de l'indice p (compris) a l'indice r (compris) 
      par ordre croissant 
    */
    {
      int q; 
      if (p < r)
      {
        q = PartitionStochastique_inc(A, I, p, r);
        TriRapideStochastique_inc (A, I, p, q) ;
        TriRapideStochastique_inc (A, I, q+1, r) ;
      }
    } /* TriRapideStochastique_inc() */
   
   private int PartitionStochastique_inc (int A[], int I[], int p, int r)
   /* =============================================================== */
   /*
     partitionne les elements de A entre l'indice p (compris) et l'indice r (compris)
     en deux groupes : ceux <= A[q] et les autres, avec q tire au hasard dans [p,r].
   */
   {
     int t;
     int t1;
     int q;

     
     int rand = randomGen.genUniformRandomNum(0, 32767);
     q = p + (rand % (r - p + 1));
     t = A[p];         /* echange A[p] et A[q] */
     A[p] = A[q]; 
     A[q] = t;
     
     t1 = I[p];         /* echange I[p] et I[q] */
     I[p] = I[q]; 
     I[q] = t1;

     return Partitionner_inc(A, I, p, r);
   } 
   
   int Partitionner_inc(int A[], int I[], int p, int r)
   /* =============================================================== */
   /*
     partitionne les elements de A entre l'indice p (compris) et l'indice r (compris)
     en deux groupes : ceux <= A[p] et les autres.
   */
   {
     int  t;
     int t1;
     int x = A[p];
     int i = p - 1;
     int j = r + 1;
     while (true)
     {
       do j--; while (A[j] > x);
       do i++; while (A[i] < x);
       if (i < j) 
         { 
   	t = A[i];
   	A[i] = A[j];
   	A[j] = t; 
   	t1 = I[i];
   	I[i] = I[j];
   	I[j] = t1; 
         }
       else return j;
     } /* while (1) */   
   } 



    
    
    private void BucketSortCroiss (int A[], /* array to sort */
		       int I[],       /* array such as I[i]=i*/
		       int r,    /* number of element to sort */
		       int N)        /* N : nb of buckets */
/* =============================================================== */
/* Sort the r values of array A by ascending order */
{
int i;
int H[] = new int[N];
for (i=0;i<r;i++) 
 H[A[i]]++;
 
Lifo Bucket[] = new Lifo[N];
for (i=0;i<N;i++)
 Bucket[i] = CreeLifoVide(H[i]);
 
for (i=0;i<r;i++) 
 LifoPush(Bucket[A[i]], i);
 
int j=0; 
for (i=0;i<N;i++) 
 {
   while(!LifoVide(Bucket[i]))
	{
	  I[j] = LifoPop(Bucket[i]);
	  A[j]= i;  
	  j++; 
	}
 }
H = null;
for (i=0;i<N;i++) LifoTermine(Bucket[i]);
Bucket = null;
}
    
    private Lifo CreeLifoVide(
    		  int taillemax)
    		/* ==================================== */
    		{
    		  Lifo L = new Lifo();
    		  
    		  L.setMax(taillemax);
    		  L.setSp(0);
    		  return L;
    		}

    private void LifoPush(Lifo L, int V)
    /* ==================================== */
    {
      if (L.getSp() > L.getMax() - 1)
      {
        System.err.println("error Lifo full\n");
        System.exit(1);
      }
      L.setPts(L.getSp(),V);
      L.setSp(L.getSp() + 1);
    }
    
    private boolean LifoVide(
    		  Lifo L)
    		/* ==================================== */
    		{
    		  return (L.getSp() == 0);
    		}
    
    private int LifoPop(
    		  Lifo L)
    		/* ==================================== */
    		{
    		  if (L.getSp() == 0)
    		  {
    		    System.err.println("error Lifo empty");
    		    System.exit(1);
    		  }
    		  L.setSp(L.getSp()-1);
    		  return L.getPts(L.getSp());
    		}

    private void LifoTermine(
    		  Lifo L)
    		/* ==================================== */
    		{
    	      L.deletePts();
    		  L = null;
    		}
    
    private void LifoFlush(
    		  Lifo L)
    		/* ==================================== */
    		{
    		  L.setSp(0);
    		}
    
    private void LifoPrint(Lifo L)
    /* ==================================== */
    {
      int i;
      if (LifoVide(L)) {System.out.println("[]"); return;}
      System.out.print("[ ");
      for (i = 0; i < L.getSp(); i++)
        System.out.print(" L.getPts(i) = " + L.getPts(i));
      
      System.out.println("]");
    }




  class Lifo {
    	  int Max;          /* taille max de la Lifo */
    	  int Sp;           /* index de pile (pointe la 1ere case libre) */
    	  int Pts[] = new int[1];
    	  
    	  public void setMax(int Max) {
    		  this.Max = Max;
    	  }
    	  
    	  public void setSp(int Sp) {
    		  this.Sp = Sp;
    	  }
    	  
    	  public int getMax() {
    		  return Max;
    	  }
    	  
    	  public int getSp() {
    		  return Sp;
    	  }
    	  
    	  public void setPts(int index, int value) {
    		  int i;
    		  if (index > Pts.length-1) {
    			  int tmpPts[] = new int[Pts.length];
    			  for (i = 0; i < Pts.length; i++) {
    				  tmpPts[i] = Pts[i];
    			  }
    			  Pts = null;
    			  Pts = new int[index+1];
    			  for (i = 0; i < tmpPts.length; i++) {
    				  Pts[i] = tmpPts[i];
    			  }
    			  
    			  tmpPts = null;
    		  } // if (index > Pts.length-1)
    		  Pts[index] = value;
    	  }
    	  
    	  public int getPts(int index) {
    		  return Pts[index];
    	  }
    	  
    	  public void deletePts() {
    		  Pts = null;
    	  }
    	}
  
  private Rbt CreeRbtVide(
		  int taillemax)
		/* ==================================== */
		{
		  int i;
		  Rbt T = new Rbt();
		  /* le tableau Elts du Rbt peut stocker taillemax+1 elements, dont 1 pour nil */
		  // Table of Elts Rbt can store taillemax+1 elements, including 1 for nil
		  /* l'element 0 du tableau est reserve pour representer nil */
		  // The element of the array 0 is reserved to represent nil
		  T.setElts(taillemax,  new RbtElt());
		 
		  T.setMax(taillemax);
		  T.setUtil(0);
		  T.setMaxutil(0);
		  T.setNil(T.getElts(0));
		  RbtElt nil = T.getNil();
		  nil.setLeft(null);
		  nil.setRight(null);
		  nil.setParent(null);
		  
		  T.setRoot(nil);

		  /* chaine les elements libres a l'aide du pointeur right */
		  // free chain elements to using the right pointer
		  for (i = 1; i < taillemax; i++) {
			  
			  T.getElts(i).setRight(T.getElts(i+1));
		  }
		  T.getElts(taillemax).setRight(null);
		  T.setLibre(T.getElts(1));

		  return T;
		} /* CreeRbtVide() */

  
  class RbtElt {
	  int auxdata;
	  double key;
	  byte color;
	  RbtElt left;
	  RbtElt right;
	  RbtElt parent;
	  
	  public void setAuxdata(int auxdata) {
		  this.auxdata = auxdata;
	  }
	  
	  public int getAuxdata() {
		  return auxdata;
	  }
	  
	  public void setKey(double key) {
		  this.key = key;
	  }
	  
	  public double getKey() {
		  return key;
	  }
	  
	  public void setColor(byte color) {
		  this.color = color;
	  }
	  
	  public byte getColor() {
		  return color;
	  }
	  
	  public void setLeft(RbtElt left) {
		  this.left = left;
	  }
	  
	  public RbtElt getLeft() {
		  return left;
	  }
	  
	  public void setRight(RbtElt right) {
		  this.right = right;
	  }
	  
	  public RbtElt getRight() {
		  return right;
	  }
	  
	  public void setParent(RbtElt parent) {
		  this.parent = parent;
	  }
	  
	  public RbtElt getParent() {
		  return parent;
	  }
	} 
  
    private void RbtCopy(Rbt dest, Rbt src) {
    	dest.setMax(src.getMax());
    	dest.setUtil(src.getUtil());
    	dest.setMaxutil(src.getMaxutil());
    	dest.setRoot(src.getRoot());
    	dest.setNil(src.getNil());
    	dest.setLibre(src.getLibre());
    	dest.setElts(src.getElts());
    }

	class Rbt {
	  int max;             /* taille max du rbt (en nombre de points) */
	  int util;            /* nombre de points courant dans le rbt */
	  int maxutil;         /* nombre de points utilises max (au cours du temps) */
	  RbtElt root;        /* racine de l'arbre */
	  RbtElt nil;         /* sentinelle et element dont l'adresse joue le role de NIL */
	  RbtElt libre;       /* pile des cellules libres */
	  RbtElt elts[] = new RbtElt[]{new RbtElt()};      /* tableau des elements physiques */
	  
	  public void setMax(int max) {
		  this.max = max;
	  }
	  
	  public int getMax() {
		  return max;
	  }
	  
	  public void setUtil(int util) {
		  this.util = util;
	  }
	  
	  public int getUtil() {
		  return util;
	  }
	  
	  public void setMaxutil(int maxutil) {
		  this.maxutil = maxutil;
	  }
	  
	  public int getMaxutil() {
		  return maxutil;
	  }
	  
	  public void setRoot(RbtElt root) {
		  this.root = root;
	  }
	  
	  public RbtElt getRoot() {
		  return root;
	  }
	  
	  public void setNil(RbtElt nil) {
		  this.nil = nil;
	  }
	  
	  public RbtElt getNil() {
		  return nil;
	  }
	  
	  public void setLibre(RbtElt libre) {
		  this.libre = libre;
	  }
	  
	  public RbtElt getLibre() {
		  return libre;
	  }
	  
	  public void setElts(int index, RbtElt value) {
		  int i;
		  if (index > elts.length-1) {
			  RbtElt tmpelts[] = new RbtElt[elts.length];
			  for (i = 0; i < elts.length; i++) {
				  tmpelts[i] = elts[i];
			  }
			  elts = null;
			  elts = new RbtElt[index+1];
			  for (i = 0; i < tmpelts.length; i++) {
				  elts[i] = tmpelts[i];
			  }
			  for (i = 0; i < index; i++) {
				  elts[i] = new RbtElt();
			  }
			  tmpelts = null;
		  } // if (index > elts.length-1)
		  elts[index] = value;	  
	  }
	  
	  public RbtElt getElts(int index) {
          return elts[index];  
	  }
	  
	  public void setElts(RbtElt elts[]) {
		  this.elts = elts;
	  }
	  
	  public RbtElt[] getElts() {
		  return elts;
	  }
	  
	  public int getEltsLength() {
		  return elts.length;  
	  }
	  
	}
	
	public void RbtTermine(Rbt T) {
		  int i;
		  RbtElt elts[] = T.getElts();
		  if (elts != null) {
		      for (i = 0; i < elts.length; i++) {
		    	  elts[i] = null;
		      }
		      elts = null;
		  }
	      T.setRoot(null);
	      T.setNil(null);
	      T.setLibre(null);
	      T = null;
	  }
	
	private void IndicsInit(int Size)
	/* ==================================== */
	{
	  Indics = new short[Size];
	}

	private void Set(int x, int INDIC) {
		Indics[x]|=(1<<INDIC);
	}
	
	private void UnSet(int x,int INDIC) {
		Indics[x]&=~(1<<INDIC);
	}
	
	private boolean IsSet(int x, int INDIC) {
		boolean set;
		set = ((Indics[x]&(1<<INDIC))	!= 0);
		return set;
	}
	
	private void UnSetAll(int x)    {
		Indics[x]=0;
	}
    
}
