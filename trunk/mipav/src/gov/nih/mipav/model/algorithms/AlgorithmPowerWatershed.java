package gov.nih.mipav.model.algorithms;
import WildMagic.LibFoundation.Mathematics.Vector3f;


import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;
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

    

    //~ Instance fields ------------------------------------------------------------------------------------------------
    private int algo;
    
    // If true, multi-labels segmentation, else 2-labels segmentation
    private boolean multi;
    
    // The index in the image array of the seed
    private Vector<Integer> index_seeds;
    
    // For 2-labels 1 for white foreground and 2 for black background
    // For multi-labels values from 1 to n with n <= 255 for segmentation in n labels
    private Vector<Short> index_labels;
    
    // Geodesic reconstruction
    private boolean geod;
    
    private boolean error = false;
    
    private int MAX;
    
    private short Indics[] = null;       /* en global pour etre efficace 

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs Power watershed algorithm.
     *
     * @param  destImg  Image model where result image is to stored
     * @param  srcImg   Source image model
     * @param  algo Kruskal, PW_qis2, or Prim
     * @param  multi If true multi-labels, else two-labels
     * @param  index_seeds The index in the image array of the seed
     * @param  index_labels For 2-labels 1 for white foreground and 2 for black background
     *                      For multi-labels values from 1 to n with n <= 255 for segmentation in n labels
     * @param  geod  Geodesic reconstruction
     */
    public AlgorithmPowerWatershed(ModelImage destImg, ModelImage srcImg, int algo, boolean multi,
    		Vector<Integer> index_seeds, Vector<Short> index_labels, boolean geod) {

        super(destImg, srcImg);
        this.algo = algo;
        this.multi = multi;
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

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int M; // Number of edges
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        String imageName = srcImage.getImageName();
        int zDim;
        int edges[][];
        int weights[];
        int max_weight;
        boolean quicksort = false;
        int size_seeds = index_seeds.size();
        ModelImage colorImage = null;
        ModelImage grayImage = null;
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
        nblabels = index_labels.get(index_labels.size()-1);
        
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
        setCompleted(true);
        return;
    } // runAlgorithm
    
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
  Indics[u] |= (1 << 0);
  
for (u=0;u<M;u++)
  {
    if (u==seeds.get(i))
	{
	  for (x=1;x<=numvoisins;x++)
	    {
	      y = neighbor_node_edge(seeds.get(i), x, rs, cs, ds);
	      if ((y != -1) && ((Indics[y] & (1 << 1)) == 0))
		{ 
		  RbtInsert(L, (double)(255-weights[y]), y);
		  sizeL++;
		  Indics[y] |= (1 << 1);
		}
	    }
	  i++;
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
	      if ((v != -1) && ((Indics[v] & (1 << 1)) == 0))
		{
		  x_1 = edges[0][v];
		  y_1 = edges[1][v];
		  if((Math.min(G[x_1],G[y_1]) == 0) && (Math.max(G[x_1],G[y_1]) > 0))
		    {
		      RbtInsert(L, (double)(255-weights[v]), v);
		      sizeL++;
		      Indics[v] |= (1 << 1);
		    }	  
		}
	    }
	}
    Indics[u] &= ~(1 << 1);
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
L.RbtTermine();
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
     d = null;

   //#ifdef DEBUGDELETE
     //fprintf(stderr,"RbtDeleteAux \n");
   //#endif

     if ((z.getLeft() == T.getNil()) || (z.getRight() == T.getNil()))
       {   d = z;
         //  fprintf(stderr,"d=z \n");
   }
     else 
       {
       //d = RbtSuccessor(T, z);
       // fprintf(stderr,"d=succ \n");
       }
     if (d.getLeft() != T.getNil())
       {
         c = d.getLeft();
         //     printf("1 : c = %ld \n", c->auxdata);
       }
     else 
       {
       c = d.getRight();
       //   printf("2 : d = %ld, c = %ld \n",d->auxdata, c->auxdata);
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
     //if (d.getColor() == RBT_Black)
       //RbtDeleteFixup(T, c);     /* c is now "Double-Black" */

   //#ifdef DEBUGDELETE
   //fprintf(stderr,"Fin RbtDeleteAux\n");
   //#endif

     return d;
   } /* RbtDeleteAux() */


    
    RbtElt RbtInsert(
    		  Rbt T, double k, int d)
    		/* ==================================== */
    		{
    		  RbtElt x;
    		  RbtElt xc; /* pour retourner le pointeur sur l'element alloue */
    		  xc = null;
    		  RbtElt uncle;

    		//#ifdef DEBUGINSERT
    		//printf("RbtInsert: data = %ld ; key = %lg\n", d, k);
    		//#endif

    		  //xc = x = RbtInsertAux(T, k, d);          /* allocation et insertion simple */
    		  //x->color = RBT_Red;

    		  /* re-equilibrage de l'arbre */
    		 // while ((x != (*T)->root) && (x->parent->color == RBT_Red))
    		  //{
    		  //  if (x->parent == x->parent->parent->left)
    		   // {
    		    //  uncle = x->parent->parent->right;
    		     // if (uncle->color == RBT_Red)
    		     // {
    		      //  x->parent->color = RBT_Black;                    /* Case I */
    		      //  uncle->color = RBT_Black;
    		      //  x->parent->parent->color = RBT_Red;
    		      // x = x->parent->parent;
    		     // }
    		     // else 
    		     // {
    		      //  if (x == x->parent->right)
    		      //  {
    		       //   x = x->parent;                             /* Case II */
    		       //   LeftRotate((*T),x);
    		       // }
    		       // x->parent->color = RBT_Black;                    /* Case III */
    		       // x->parent->parent->color = RBT_Red;
    		       // RightRotate((*T), x->parent->parent);
    		      //}
    		   // }
    		   // else /* same as "then" with "right" and "left" swapped */
    		   // {
    		     // uncle = x->parent->parent->left;
    		    //  if (uncle->color == RBT_Red)
    		     // {
    		     //   x->parent->color = RBT_Black;                     /* Case I */
    		      //  uncle->color = RBT_Black;
    		      //  x->parent->parent->color = RBT_Red;
    		      //  x = x->parent->parent;
    		     // }
    		     // else 
    		     // {
    		     //  if (x == x->parent->left)
    		      //  {
    		      //    x = x->parent;                             /* Case II */
    		      //    RightRotate((*T),x);
    		      //  }
    		      //  x->parent->color = RBT_Black;                    /* Case III */
    		      //  x->parent->parent->color = RBT_Red;
    		      //  LeftRotate((*T), x->parent->parent);
    		    //  }
    		   // }
    		 // } /* while */
    		//  (*T)->root->color = RBT_Black;

    		//#ifdef DEBUGINSERT
    		//printf("FIN RbtInsert xc->data = %ld ; xc->key = %lg\n", xc->auxdata, xc->key);
    		//#endif

    		//#ifdef PARANO
    		//  if (xc->auxdata != d) printf("BUG RbtInsert xc->auxdata = %ld ; d = %ld\n", xc->auxdata, d);
    		//#endif

    		  return xc;                      /* modif mc: retourne xc plutot que x (sinon: BUG) */
    		} /* RbtInsert() */


    
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

     RandomNumberGen randomGen = new RandomNumberGen();
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
        System.err.println("error Lifo pleine\n");
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
    		    System.err.println("erreur Lifo vide");
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
		  /* l'element 0 du tableau est reserve pour representer nil */
		 
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
		  for (i = 1; i < taillemax; i++) 
			  T.getElts(i).setRight(T.getElts(i+1));
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

	class Rbt {
	  int max;             /* taille max du rbt (en nombre de points) */
	  int util;            /* nombre de points courant dans le rbt */
	  int maxutil;         /* nombre de points utilises max (au cours du temps) */
	  RbtElt root;        /* racine de l'arbre */
	  RbtElt nil;         /* sentinelle et element dont l'adresse joue le role de NIL */
	  RbtElt libre;       /* pile des cellules libres */
	  RbtElt elts[] = new RbtElt[1];      /* tableau des elements physiques */
	  
	  public void setMax(int max) {
		  this.max = max;
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
			  tmpelts = null;
		  } // if (index > elts.length-1)
		  elts[index] = value;	  
	  }
	  
	  public RbtElt getElts(int index) {
          return elts[index];  
	  }
	  
	  public void RbtTermine() {
		  int i;
	      for (i = 0; i < elts.length; i++) {
	    	  elts[i] = null;
	      }
	      elts = null;
	      root = null;
	      nil = null;
	      libre = null;
	  }
	} 
	
	private void IndicsInit(int Size)
	/* ==================================== */
	{
	  Indics = new short[Size];
	}


    
}
