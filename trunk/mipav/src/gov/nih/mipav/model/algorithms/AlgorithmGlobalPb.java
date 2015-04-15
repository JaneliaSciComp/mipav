package gov.nih.mipav.model.algorithms;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;

/**
 * Compute globalPb, the globalized probability of boundary of an image
 * Reference: Contour Detection and Hierarchical Image Segmentation by Pablo Arbelaez, Michael Maire,
 * Charless Fowlkes, and Jitendra Malik, IEEE Transactions on Pattern Analysis and Machine Intelligence,
 * Volume 33, Issue 5, August, 2010, pp. 898-916.
 * 
 * BSR package code written in MATLAB and C++ is ported to Java with the permission of 
 * Pablo Andres Arbelaez Escalante.
 */

public class AlgorithmGlobalPb extends AlgorithmBase {
	
	// Resizing factor in (0, 1], to speed up eigenvector
	private double rsz = 1.0;
	
	private int xDim;
	
	private int yDim;
	
	private int sliceSize;
	
	// Maximum fraction of items in k-means clusterer
	private double maxFraction = 1.0;
	
	// Minimum number of items for frac sample
	private int minSample = 1;
	
	// Maximum # items (0 for unlimited)
	private int maxItems = 0;
	
	// Desired number of clusters
	private int kmeansK = 64;
	
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmGlobalPb - default constructor.
     */
    public AlgorithmGlobalPb() { }
    
    /**
     * 
     * @param destImg
     * @param srcImg
     */
    public AlgorithmGlobalPb(ModelImage destImg, ModelImage srcImg) {
    	super(destImg, srcImg);
    }
    
    /**
     * Original globalPb.m by Pablo Arbelaez on December, 2010
     */
    public void runAlgorithm() {
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        ModelImage inputImage;
      	AlgorithmChangeType changeTypeAlgo;
      	FileInfoBase[] fileInfo;
      	double weights[] = new double[13];
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        if ((rsz <= 0.0) || (rsz > 1.0)) {
        	MipavUtil.displayError("Resizing factor rsz out of range (0, 1]");
        	setCompleted(false);
        	return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running Global Pb ...");
        
        if (srcImage.isColorImage()) {
        	inputImage = new ModelImage(ModelStorageBase.ARGB_FLOAT,
      				srcImage.getExtents(), "changeTypeImage");
        }
        else {
        	inputImage = new ModelImage(ModelStorageBase.DOUBLE,
      				srcImage.getExtents(), "changeTypeImage");
        }
  		inputImage.getFileInfo(0).setEndianess(FileBase.LITTLE_ENDIAN);
  		changeTypeAlgo = new AlgorithmChangeType(inputImage,
				srcImage, srcImage.getMin(),
				srcImage.getMax(), 0.0, 1.0, image25D);
  		changeTypeAlgo.run();
  		changeTypeAlgo.finalize();
  		changeTypeAlgo = null;
  		fileInfo = inputImage.getFileInfo();
        fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
        fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
        fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo[0].setExtents(inputImage.getExtents());
        fileInfo[0].setMax(inputImage.getMax());
        fileInfo[0].setMin(inputImage.getMin());
        fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
        fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
        fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        
        // Default feature weights
        if (srcImage.isColorImage()) {
        	weights[0] = 0.0;
        	weights[1] = 0.0;
        	weights[2] = 0.0039;
        	weights[3] = 0.0050;
        	weights[4] = 0.0058;
        	weights[5] = 0.0069;
        	weights[6] = 0.0040;
        	weights[7] = 0.0044;
        	weights[8] = 0.0049;
        	weights[9] = 0.0024;
        	weights[10] = 0.0027;
        	weights[11] = 0.0170;
        	weights[12] = 0.0074;
        }
        else {
        	weights[0] = 0.0;
        	weights[1] = 0.0;
        	weights[2] = 0.0054;
        	weights[3] = 0.0;
        	weights[4] = 0.0;
        	weights[5] = 0.0;
        	weights[6] = 0.0;
        	weights[7] = 0.0;
        	weights[8] = 0.0;
        	weights[9] = 0.0048;
        	weights[10] = 0.0049;
        	weights[11] = 0.0264;
        	weights[12] = 0.0090;
        }
        
        // mPb
        multiscalePb(inputImage);
        
        setCompleted(true);
        return;
    }
    
    /**
     * Compute local contour cues of an image
     * gradinets by Michael Maire
     * Original MATLAB code by Pablo Arbelaez December, 2010
     * @param im
     */
    private void multiscalePb(ModelImage im) {
        double weights[] = new double[12];
        double buffer[];
        double red[] = new double[sliceSize];
        double green[] = new double[sliceSize];
        double blue[] = new double[sliceSize];
        int ini;
        int outi;
        int x;
        int y;
        if (im.isColorImage()) {
        	buffer = new double[4*sliceSize];
        	try {
        	    im.exportData(0, 4* sliceSize, buffer);
        	}
        	catch (IOException e) {
        	    e.printStackTrace();	
        	}
        	im.disposeLocal();
        	im = null;
        	for (x = 0; x < xDim; x++) {
        		for (y = 0; y < yDim; y++) {
        			ini = x + y * xDim;
        			outi = y + x * yDim;
            	    red[outi] = buffer[4*ini+1];
            	    green[outi] = buffer[4*ini+2];
            	    blue[outi] = buffer[4*ini+3];
        		}
            }
            weights[0] = 0.0146;
            weights[1] = 0.0145;
            weights[2] = 0.0163;
            weights[3] = 0.0210;
            weights[4] = 0.0243;
            weights[5] = 0.0287;
            weights[6] = 0.0166;
            weights[7] = 0.0185;
            weights[8] = 0.0204;
            weights[9] = 0.0101;
            weights[10] = 0.0111;
            weights[11] = 0.0141;
        }
        else {
        	buffer = new double[sliceSize];
        	try {
        	    im.exportData(0, sliceSize, buffer);
        	}
        	catch (IOException e) {
        	    e.printStackTrace();	
        	}
        	im.disposeLocal();
        	im = null;
        	for (x = 0; x < xDim; x++) {
        		for (y = 0; y < yDim; y++) {
        			ini = x + y * xDim;
        			outi = y + x * yDim;
        			red[outi] = buffer[ini];
        			green[outi] = buffer[ini];
        			blue[outi] = buffer[ini];
        		}
        	}
        	weights[0] = 0.0245;
        	weights[1] = 0.0220;
        	weights[2] = 0.0233;
        	weights[3] = 0.0;
        	weights[4] = 0.0;
        	weights[5] = 0.0;
        	weights[6] = 0.0;
        	weights[7] = 0.0;
        	weights[8] = 0.0;
        	weights[9] = 0.0208;
        	weights[10] = 0.0210;
        	weights[11] = 0.0229;
        }
        
        // Get gradients
        det_mPb(red, green, blue);
    }
    
    /**
     * Compute image gradients.  Implementation by Michael Maire.
     * @param im
     */
    private void det_mPb(double red[], double green[], double blue[]) {
        // Compute pb
    	mex_pb_parts_final_selected(red, green, blue);
    }
    
    private void mex_pb_parts_final_selected(double L[], double a[], double b[]) {
    	int i;
    	int x;
    	int y;
    	// Border pixels
    	int border = 30;
    	// Mirror border
    	int dstXDim = xDim + 2 * border;
    	int dstYDim = yDim + 2 * border;
    	double L_border[] = new double[dstXDim * dstYDim];
    	compute_border_mirror_2D(L, L_border, border, border, xDim, yDim);
    	double a_border[] = new double[dstXDim * dstYDim];
    	compute_border_mirror_2D(a, a_border, border, border, xDim, yDim);
    	double b_border[] = new double[dstXDim * dstYDim];
    	compute_border_mirror_2D(b, b_border, border, border, xDim, yDim);
    	// Converting RGB to grayscale
    	double gray[] = new double[L_border.length];
    	grayscale(gray, L_border, a_border, b_border);
    	// Gamma correct
    	rgb_gamma_correct(L_border, a_border, b_border, 2.5);
    	// Converting RGB to Lab
    	rgb_to_lab(L_border, a_border, b_border);
    	lab_normalize(L_border, a_border, b_border);
    	// Quantizing color channels
    	// # bins for bg
    	int numLBins = 25;
    	// # bins for cg_a
    	int numaBins = 25;
    	// # bins for cg_b
    	int numbBins = 25;
    	int Lq[] = new int[L_border.length];
    	quantize_values(Lq, L_border, numLBins);
    	int aq[] = new int[a_border.length];
    	quantize_values(aq, a_border, numaBins);
    	int bq[] = new int[b_border.length];
    	quantize_values(bq, b_border, numbBins);
    	// Computing filter set for textons
    	// Number of orientations
    	int n_ori = 8;
    	// sigma for small tg filters
    	double sigma_tg_filt_sm = 2.0;
    	ArrayList <double[][]> filters_small = new ArrayList<double[][]>();
    	texton_filters(filters_small, n_ori, sigma_tg_filt_sm);
    	// Sigma for large tg filters
    	double sigma_tg_filt_lg = 2.0 * Math.sqrt(2.0);
    	ArrayList <double[][]> filters_large = new ArrayList<double[][]>();
    	texton_filters(filters_large, n_ori, sigma_tg_filt_lg);
    	ArrayList <double[][]> filters = new ArrayList<double[][]>();
    	for (i = 0; i < filters_small.size(); i++) {
    		filters.add(filters_small.get(i));
    	}
    	filters_small.clear();
    	for (i = 0; i < filters_large.size(); i++) {
    		filters.add(filters_large.get(i));
    	}
    	filters_large.clear();
    	// Compute textons
    	ArrayList<double[][]>textons = new ArrayList<double[][]>();
    	int iterations = 10;
    	double subsampling = 0.10;
    	double gray2D[][] = new double[dstXDim][dstYDim];
    	for (x = 0; x < dstXDim; x++) {
    		for (y = 0; y < dstYDim; y++) {
    			gray2D[x][y] = gray[y + x * dstYDim];
    		}
    	}
    	int t_assign[] = textons_routine(gray2D, filters, textons, 64, iterations, subsampling);
    }
    
    private int[] textons_routine(double m[][], ArrayList<double[][]> filters, ArrayList<double[][]> textons,
    		int K, int max_iter, double subsampling) {
    	int i;
    	// Convolve image with filters
    	// Return the convolution of the image with each of the filters so that
    	// the result is the same size as the original image
        ArrayList<double[][]> responses = new ArrayList<double[][]>();
        double Cout[][];
        boolean is_cropped = true;
        boolean is_strict = false;
        for (i = 0; i < filters.size(); i++) {
        	Cout = compute_conv_2D(m, filters.get(i), is_cropped, is_strict);
        	responses.add(Cout);
        }
        maxFraction = subsampling;
        minSample = K;
        kmeansK = K;
        return null;
        /*
         * Sample clusterer.
         *
         * This clusterer produces centroids by clustering a randomly selected subset
         * of the items.  It then uses these centroids to compute cluster assignments
         * for all of the items.
         *
         * Items are selected for inclusion in the subset to cluster with probablility
         * proportional to their weight.
         */
        /*
         * Constructor.
         * Create a clusterer that operates on a fraction of the total items.
         * However, if this fraction is less than the specified minimum sample 
         * size, then the clusterer operates on the minimum sample size.
         */
        /*explicit sample_clusterer(
           const centroid_clusterer<T>&, /* underlying clusterer to employ */
           //double,                       /* fraction of items */
           //unsigned long = 1             /* minimum sample size */
        //);*/


        // matrix_clusterer(K (desired number of clusters), max_iter (maximum number of iterations, 0 for 
        //                  unlimited, clustering metric, default is L2 distance), 
        // Cluster filter responses
        //sample_clusterer< matrix<> >(
                //kmeans::matrix_clusterer<>(K, max_iter, matrix_metrics<>::L2_metric()),
                //subsampling,
                //K
             //)
        //return cluster_filter_responses(responses, clusterer, textons);

    }
    
    /*int[] cluster_filter_responses(
    		   final ArrayList<double[][]>                            responses,
    		   final centroid_clusterer< matrix<> >&                clusterer,
    		   ArrayList<double[][]> centroids)
    		{
    		   // create collection of vectors
    		   list< matrix<> > vec_list;
    		   for (int n = 0; n < responses._size; n++)
    		      vec_list.add(responses._data[n]);
    		   // cluster
    		   array<unsigned long> assign = clusterer.cluster(vec_list, centroids);
    		   // convert assignment array to assignment matrix
    		   matrix<unsigned long> m_assign = matrix<unsigned long>::to_matrix(assign);
    		   m_assign.reshape(responses._dims);
    		   return m_assign;
    		}*/
    
    /*
     * Clustering.
     * Return the cluster assignments and cluster centroids.
     */
    private int []cluster(
       final ArrayList<double[][]> items,
       ArrayList<metricCentroid> centroids) 
    {
    	int i;
    	int elremoved;
    	int j;
       // compute number of items to cluster
       int n_items = items.size();
       int samp_size = sample_size(n_items); 
       // check if clustering the full set
       if (samp_size == n_items) {
          return kmeans_cluster(items, centroids);
       } else {
          // randomize item order
    	   RandomNumberGen randomGen = new RandomNumberGen();
    	   ArrayList<doubleIntegerItem> randomList = new ArrayList<doubleIntegerItem>();
    	   for (i = 0; i < n_items; i++) {
    		   randomList.add(new doubleIntegerItem(randomGen.genUniformRandomNum(0.0, 1.0), i));
    	   }
    	   Collections.sort(randomList, new doubleIntegerComparator());
    	   int idx_map[] = new int[n_items];
    	   int idx_map2[] = new int[n_items];
    	   for (i = 0; i < n_items; i++) {
    	       idx_map[i] = randomList.get(i).getiN();
    	       idx_map2[i] = idx_map[i];
    	   }
    	   ArrayList<double[][]>items_subset = new ArrayList<double[][]>();
    	   for (i = 0; i < samp_size; i++) {
    		   elremoved = idx_map2[i];
    		   items_subset.add(items.remove(elremoved));
    		   for (j = i+1; j < idx_map2.length; j++) {
    			   if (idx_map2[j] > elremoved) {
    				   idx_map2[j]--;
    			   }
    		   }
    	   } // for (i = 0; i < samp_size; i++)
          // cluster subset
          int[] assign_subset = kmeans_cluster(
             items_subset, centroids
          );
          // compute assignments for remaining items
          /*int[] assign_array = kmeans_assign_cluster(
             items, centroids
          );*/
          // combine assignment arrays
          int assign[] = new int[n_items];
          for (int n = 0; n < samp_size; n++)
             assign[idx_map[n]] = assign_subset[n];
          /*for (int n = samp_size; n < n_items; n++)
             assign[idx_map[n]] = assign_array[n - samp_size];*/
          return assign;
       }
    }
    
    private int[] kmeans_cluster(ArrayList<double[][]>items, ArrayList<metricCentroid>centroids) {
    	int i;
    	int n_items = items.size();
    	double weights[] = new double[n_items];
    	for (i = 0; i < n_items; i++) {
    	    weights[i] = 1.0;	
    	}
    	RandomNumberGen randomGen = new RandomNumberGen();
 	    ArrayList<doubleIntegerItem> randomList = new ArrayList<doubleIntegerItem>();
 	    for (i = 0; i < n_items; i++) {
 		    randomList.add(new doubleIntegerItem(randomGen.genUniformRandomNum(0.0, 1.0), i));
 	    }
 	    Collections.sort(randomList, new doubleIntegerComparator());
 	    int idx_map[] = new int[n_items];
 	    for (i = 0; i < n_items; i++) {
	        idx_map[i] = randomList.get(i).getiN();
	    }
 	    return kmeans_cluster(items, weights, idx_map, centroids);
    }
    
    private int[] kmeans_cluster(ArrayList<double[][]>items, double[] weights, int[] idx_map, ArrayList<metricCentroid>centroids) {
    	int i;
    	int n_items = items.size();
    	// Randomize item order using the specified random permutation
    	ArrayList<double[][]>items_array = new ArrayList<double[][]>();
    	double weights_array[] = new double[n_items];
    	for (i = 0; i < n_items; i++) {
    		items_array.add(items.get(idx_map[i]));
    		weights_array[i] = weights[idx_map[i]];
    	}
    	// Compute maximum number of clusters to return 
    	int K = (kmeansK < n_items) ? kmeansK : n_items;
    	if ((n_items > 0) && (K == 0)) {
    	      MipavUtil.displayError(
    	         "K must be nonzero during K-means clustering of nonempty collection"
    	      );
    	      return null;
    	   }
    	   // Allocate collection of centroids
    	   centroids.clear();
    	   /* return now if there are no items to cluster */
    	   if (n_items == 0)
    	      return null;
    	   // Initialize assignments (to undefined cluster # K)
    	   // Initialize cluster membership for centroids
    	   int assign[] = new int[n_items];
    	   for (i = 0; i < n_items; i++) {
    		   assign[i] = K;
    	   }
    	   ArrayList<double[][]> cluster_items = new ArrayList<double[][]>(K);
    	   ArrayList <Double> cluster_weights = new ArrayList<Double>(K);
    	   for (int n = 0; n < K; n++) {
    	      cluster_items.set(n, items_array.get(n));
    	      cluster_weights.set(n, weights_array[n]);
    	   }
    	   // Initialize centroids 
    	   for (int n = 0; n < K; n++) {
    		   centroids.add(new metricCentroid(cluster_items.get(n), cluster_weights.get(n)));
    	   }
    	   // Allocate arrays for distances
    	   
    	   ArrayList<ArrayList<Double> > distances = new ArrayList<ArrayList<Double>>(n_items);
    	   dist_resizer(
    	      0, n_items - 1, K, distances
    	   );
    	   // Initially mark the undefined cluster # K as changed
    	   boolean[] has_changed = new boolean[K+1];
    	   for (i = 0; i < K+1; i++) {
    		   has_changed[i] = false;
    	   }
    	   has_changed[K] = true;
    	   int[] changed_ids;
    	   // Compute sizes of coarse problems
    	   /*array<unsigned long> coarse_sizes =
    	      metric_clusterer<T,V>::coarse_problem_sizes(n_items, K);
    	   unsigned long n_problems = coarse_sizes.size();
    	   // Solve coarse to fine clustering problems
    	   unsigned long n_items_prev = 0;
    	   for (unsigned long prob = 0; prob < n_problems; prob++) {
    	      // Get problem size
    	      unsigned long n_items_curr = coarse_sizes[prob];
    	      // Compute distances between new items and unchanged clusters
    	      {
    	         array<bool> has_not_changed(K);
    	         for (unsigned long n = 0; n < K; n++)
    	            has_not_changed[n] = !(has_changed[n]);
    	         array<unsigned long> unchanged_ids = 
    	            basic_clusterer<T,V>::compute_changed_ids(has_not_changed);
    	         typename metric_clusterer<T,V>::distance_updater dist_updater(
    	            this->_metric,
    	            n_items_prev,
    	            n_items_curr - 1,
    	            items_array,
    	            *centroids,
    	            unchanged_ids,
    	            distances
    	         );
    	         dist_updater.run();
    	      }
    	      // Initialize any empty clusters with new items
    	      for (unsigned long n = 0, next_item = n_items_prev;
    	           ((n < K) && (next_item < n_items_curr)); n++)
    	      {
    	         if (cluster_items[n].is_empty()) {
    	            // Add item to cluster
    	               cluster_items[n].add(items_array[next_item]);
    	               cluster_weights[n].add(weights_array[next_item]);
    	            // Compute centroid
    	            auto_ptr<T> cntrd = this->_metric.centroid(
    	               cluster_items[n],
    	               cluster_weights[n]
    	            );
    	            T& cntrd_old = centroids->replace(n, *cntrd);
    	            cntrd.release();
    	            delete &cntrd_old;
    	            // Indicate that cluster has changed
    	            has_changed[n] = true;
    	            next_item++;
    	         }
    	      }
    	      // Recompute changed ids to include any filled empty clusters
    	      changed_ids = basic_clusterer<T,V>::compute_changed_ids(has_changed, K);
    	      // Iteratively update assignments and centroids
    	      for (unsigned long n_iter = 0; 
    	           ((n_iter < _max_iterations) || (_max_iterations == 0));
    	           n_iter++)
    	      {
    	         // Store old assignments
    	         array<unsigned long> assign_old = assign.subarray(0, n_items_curr-1);
    	         // Update distances
    	         typename metric_clusterer<T,V>::distance_updater dist_updater(
    	            this->_metric,
    	            0,
    	            n_items_curr - 1,
    	            items_array,
    	            *centroids,
    	            changed_ids,
    	            distances
    	         );
    	         dist_updater.run();
    	         // Update assignments
    	         typename metric_clusterer<T,V>::assignment_updater assign_updater(
    	            0,
    	            n_items_curr - 1,
    	            K,
    	            has_changed,
    	            changed_ids,
    	            distances,
    	            assign
    	         );
    	         assign_updater.run();
    	         // Compute which clusters have changed
    	         for (unsigned long n = 0; n < K; n++)
    	            has_changed[n] = false;
    	         for (unsigned long n = 0; n < n_items_curr; n++) {
    	            unsigned long assign_id     = assign[n];
    	            unsigned long assign_id_old = assign_old[n];
    	            if (assign_id != assign_id_old) {
    	               has_changed[assign_id]     = true;
    	               has_changed[assign_id_old] = true;
    	            }
    	         }
    	         // Compute ids of changed clusters
    	         changed_ids = basic_clusterer<T,V>::compute_changed_ids(
    	            has_changed, K
    	         );
    	         // Finish if no clusters changed
    	         unsigned long n_changed = changed_ids.size();
    	         if (n_changed == 0)
    	            break;
    	         // Update cluster membership
    	         for (unsigned long n = 0; n < K; n++) {
    	            cluster_items[n].clear();
    	            cluster_weights[n].clear();
    	         }
    	         for (unsigned long n = 0; n < n_items_curr; n++) {
    	            unsigned long assign_id = assign[n];
    	            cluster_items[assign_id].add(items_array[n]);
    	            cluster_weights[assign_id].add(weights_array[n]);
    	         }
    	         // Update centroids
    	         typename metric_clusterer<T,V>::centroid_updater cntrd_updater(
    	            this->_metric,
    	            0,
    	            n_changed - 1,
    	            changed_ids,
    	            cluster_items,
    	            cluster_weights,
    	            centroids
    	         );   
    	         cntrd_updater.run();
    	      }
    	      // Update previous problem size
    	      n_items_prev = n_items_curr;
    	   }
    	   // Drop empty clusters
    	   array<unsigned long> remap(K, K);   // Init remap to invalid cluster # K
    	   unsigned long n_clusters = 0;
    	   for (unsigned long n = 0; n < K; n++) {
    	      auto_ptr<T> cntrd(&(centroids->remove_head()));
    	      if (!(cluster_items[n].is_empty())) {
    	         remap[n] = n_clusters;
    	         centroids->add(*cntrd);
    	         cntrd.release();
    	         n_clusters++;
    	      }
    	   }
    	   // Retrieve original assignment order
    	   array<unsigned long> assignments(n_items);
    	   for (unsigned long n = 0; n < n_items; n++)
    	      assignments[idx_map[n]] = remap[assign[n]];
    	   return assignments;*/
    	   return null;

    }
    
    private void dist_resizer(int start, int end, int n_centroids, ArrayList<ArrayList<Double>>distances) {
    	// Resize distances sequentially
        for (int n = start; n <= end; n++)
           distances.get(n).ensureCapacity(n_centroids);

    }
    
    private class metricCentroid {
    	private double[][] cluster_items;
    	
    	private double cluster_weights;
    	
    	public metricCentroid(double[][] cluster_items,double cluster_weights) {
    		this.cluster_items = cluster_items;
    		this.cluster_weights = cluster_weights;
    	}
    }
    
    private class doubleIntegerComparator implements Comparator<doubleIntegerItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final doubleIntegerItem o1, final doubleIntegerItem o2) {
            final double a = o1.getrN();
            final double b = o2.getrN();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else {
                return 0;
            }
        }

    }
	
	private class doubleIntegerItem {

        /** DOCUMENT ME! */
        private final double rN;

        /** DOCUMENT ME! */
        private final int iN;

        /**
         * Creates a new doubleIntegerItem object.
         * 
         * @param rN
         * @param iN
         */
        public doubleIntegerItem(final double rN, final int iN) {
            this.rN = rN;
            this.iN = iN;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getrN() {
            return rN;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int getiN() {
            return iN;
        }

    }
       
       /*
        * Compute size of the sample to cluster given the total number of items.
        */
        private int sample_size(int n_items) {
          /* initialize sample size */
          int samp_size = n_items;
          /* enforce maximum items constraint */
          if (maxItems > 0) {
             if(maxItems < samp_size) { samp_size = maxItems; }
          }
          /* enforce maximum fractional constraint */
          int n_frac = (int)Math.ceil(maxFraction * n_items);
          if (n_frac < minSample) { n_frac = minSample; }
          if (n_frac < samp_size) { samp_size = n_frac; }
          return samp_size;
       }



    
    /*
     * Compute convolution (for 2D matrices).
     */
    private double[][] compute_conv_2D(
       final double m02D[][],
       final double m12D[][],
       boolean is_cropped,
       boolean is_strict)
    {
       /* get size of each matrix */
       int size0_x = m02D.length;
       int size0_y = m02D[0].length;
       int size1_x = m12D.length;
       int size1_y = m12D[0].length;
       double m0[] = new double[size0_x * size0_y];
       double m1[] = new double[size1_x * size1_y];
       int x;
       int y;
       for (x = 0; x < size0_x; x++) {
    	   for (y = 0; y < size0_y; y++) {
    		   m0[y + x * size0_y] = m02D[x][y];
    	   }
       }
       for (x = 0; x < size1_x; x++) {
    	   for (y = 0; y < size1_y; y++) {
    		   m1[y + x * size1_y] = m12D[x][y];
    	   }
       }
       /* compute dimensions of resulting matrix and also     */ 
       /* compute range of position within full result matrix */
       int size_x = 0;
       int size_y = 0;
       int pos_start_x = 0;
       int pos_start_y = 0;
       if (!is_cropped) {
          /* set dimensions for full result matrix (start position is zero) */
          size_x = ((size0_x > 0) && (size1_x > 0)) ? (size0_x + size1_x - 1) : 0;
          size_y = ((size0_y > 0) && (size1_y > 0)) ? (size0_y + size1_y - 1) : 0;
       } else if (!is_strict) {
          /* set dimensions for result matrix no larger than left input */
          size_x = ((size0_x > 0) && (size1_x > 0)) ? (size0_x) : 0;
          size_y = ((size0_y > 0) && (size1_y > 0)) ? (size0_y) : 0;
          /* set start position for result matrix no larger than left input */
          pos_start_x = size1_x/2;
          pos_start_y = size1_y/2;
       } else {
          /* set dimensions for central portion of result matrix */
          size_x =
             ((size0_x >= size1_x) && (size1_x > 0)) ? (size0_x - size1_x + 1) : 0;
          size_y =
             ((size0_y >= size1_y) && (size1_y > 0)) ? (size0_y - size1_y + 1) : 0;
          /* set start position for central portion of result matrix */
          pos_start_x = (size1_x > 0) ? (size1_x - 1) : 0;
          pos_start_y = (size1_y > 0) ? (size1_y - 1) : 0;
       }
       int pos_bound_y = pos_start_y + size_y;
       /* initialize result */
       double m2D[][] = new double[size_x][size_y];
       double m[] = new double[size_x * size_y];
       if (m.length == 0)
          return null;
       /* initialize position in result */
       int pos_x = pos_start_x;
       int pos_y = pos_start_y;
       /* compute initial range of offset_x */
       int offset_min_x =
          ((pos_x + 1) > size0_x) ? (pos_x + 1 - size0_x) : 0;
       int offset_max_x =
          (pos_x < size1_x) ? pos_x : (size1_x - 1);
       int ind0_start_x = (pos_x - offset_min_x) * size0_y;
       int ind1_start_x = (offset_min_x) * size1_y;
       for (int n = 0; n < m.length; n++) {
          /* compute range of offset_y */
          int offset_min_y =
             ((pos_y + 1) > size0_y) ? (pos_y + 1 - size0_y) : 0;
          int offset_max_y =
             (pos_y < size1_y) ? pos_y : (size1_y - 1);
          int offset_range_y = offset_max_y - offset_min_y;
          /* initialize indices */
          int ind0 = ind0_start_x + (pos_y - offset_min_y);
          int ind1 = ind1_start_x + offset_min_y;
          for (int o_x = offset_min_x; o_x <= offset_max_x; o_x++) {
             for (int o_y = offset_min_y; o_y < offset_max_y; o_y++) {
                /* update result value */
                m[n] += m0[ind0] * m1[ind1];
                /* update linear positions */
                ind0--;
                ind1++;
             }
             /* update last result value */
             m[n] += m0[ind0] * m1[ind1];
             /* update linear positions */
             ind0 = ind0 + offset_range_y - size0_y;
             ind1 = ind1 - offset_range_y + size1_y;
          }
          /* update position */
          pos_y++;
          if (pos_y == pos_bound_y) {
             /* reset y position, increment x position */
             pos_y = pos_start_y;
             pos_x++;
             /* update range of offset_x */
             offset_min_x = ((pos_x + 1) > size0_x) ? (pos_x + 1 - size0_x) : 0;
             offset_max_x = (pos_x < size1_x) ? pos_x : (size1_x - 1);
             ind0_start_x = (pos_x - offset_min_x) * size0_y;
             ind1_start_x = (offset_min_x) * size1_y;
          }
       }
       for (x = 0; x < size_x; x++) {
    	   for (y = 0; y < size_y; y++) {
    		   m2D[x][y] = m[y + x * size_y];
    	   }
       }
       return m2D;
    }
    
    private void compute_border_mirror_2D(double src[], double dst[], int borderX, int borderY, int xSrcSize, int ySrcSize) {
    	// Compute destination size
    	int xDstSize = xSrcSize + 2 * borderX;
    	int yDstSize = ySrcSize + 2 * borderY;
    	// Compute step sizes in destination matrix (for copying interior)
    	int indInit = borderX * yDstSize + borderY;
    	int indStep = 2 * borderY;
    	int n;
    	int ind;
    	int x;
    	int y;
    	int indIntr;
    	int indBdr;
    	
    	// Copy interior
    	for (n = 0, ind = indInit, x = 0; x < xSrcSize; x++) {
    		for (y = 0; y < ySrcSize; y++, n++, ind++) {
    			dst[ind] = src[n];
    		}
    		ind += indStep;
    	}
    	
    	// Mirror top
    	indIntr = borderX * yDstSize;
    	indBdr = indIntr + yDstSize;
    	for (x = 0; x < borderX; x++) {
    	    indBdr -= 2 * yDstSize;
    	    for (y = 0; y < yDstSize; y++) {
    	    	dst[indBdr++] = dst[indIntr++];
    	    }
    	}
    	
    	// Mirror bottom
    	indBdr = (xSrcSize + borderX) * yDstSize;
    	indIntr = indBdr + yDstSize;
    	for (x = 0; x < borderX; x++) {
    		indIntr -= 2*yDstSize;
    		for (y = 0; y < yDstSize; y++) {
    			dst[indBdr++] = dst[indIntr++];
    		}
    	}
    	
    	// Mirror left
    	indBdr = 0;
    	indIntr = 2 * borderY;
    	for (x = 0; x < xDstSize; x++) {
    		for (y = 0; y < borderY; y++) {
    		    dst[indBdr++] = dst[--indIntr];	
    		}
    		indBdr += (yDstSize - borderY);
    		indIntr += (yDstSize + borderY);
    	}
    	
    	// Mirror right
    	indBdr = yDstSize - borderY;
    	indIntr = indBdr;
    	for (x = 0; x < xDstSize; x++) {
    		for (y = 0; y < borderY; y++) {
    			dst[indBdr++] = dst[--indIntr];
    		}
    		indBdr += (yDstSize - borderY);
    		indIntr += (yDstSize + borderY);
    	}
    	return;
    }
    
    private void grayscale(double m[], double r[], double g[], double b[]) {
    	int n;
    	for (n = 0; n < m.length; n++) {
    		m[n] = (0.29894 * r[n]) + (0.58704 * g[n]) + (0.11402 * b[n]);
    	}
    	return;
    }
    
    private void rgb_gamma_correct(double r[], double g[], double b[], double gamma) {
    	int n;
    	for (n = 0; n < r.length; n++) {
    	    r[n] = Math.pow(r[n], gamma);
    	    g[n] = Math.pow(g[n], gamma);
    	    b[n] = Math.pow(b[n], gamma);
    	}
    	return;
    }
    
    /**
     * Convert from RGB color space to Lab color space
     * @param r_l
     * @param g_a
     * @param b_b
     */
    private void rgb_to_lab(double r_l[], double g_a[], double b_b[]) {
    	rgb_to_xyz(r_l, g_a, b_b);
    	xyz_to_lab(r_l, g_a, b_b);
    }
    
    /**
     * Convert from RGB color space to XYZ color space
     * @param r_x
     * @param g_y
     * @param b_z
     */
    private void rgb_to_xyz(double r_x[], double g_y[], double b_z[]) {
    	int n;
    	double r;
    	double g;
    	double b;
    	for (n = 0; n < r_x.length; n++) {
    		r = r_x[n];
    		g = g_y[n];
    		b = b_z[n];
    		r_x[n] = (0.412453 * r) + (0.357580 * g) + (0.180423 * b);
    		g_y[n] = (0.212671 * r) + (0.715160 * g) + (0.072169 * b);
    		b_z[n] = (0.019334 * r) + (0.119193 * g) + (0.950227 * b);
    	}
    	return;
    }
    
    /**
     * Convert from XYZ color space to Lab color space
     * @param x_l
     * @param y_a
     * @param z_b
     */
    private void xyz_to_lab(double x_l[], double y_a[], double z_b[]) {
        // D65 white point reference
    	final double xRef = 0.950456;
    	final double yRef = 1.000000;
    	final double zRef = 1.088754;
    	// Threshold value
    	final double threshold = 0.008856;
    	// Convert XYZ to Lab
    	int n;
    	double x;
    	double y;
    	double z;
    	double fx;
    	double fy;
    	double fz;
    	for (n = 0; n < x_l.length; n++) {
    		// Extract xyz color value and normalize by reference point
    		x = x_l[n]/xRef;
    		y = y_a[n]/yRef;
    		z = z_b[n]/zRef;
    		// Compute fx, fy, fz
    		fx = (x > threshold) ? Math.pow(x, (1.0/3.0)) : (7.787 * x + (16.0/116.0));
    		fy = (y > threshold) ? Math.pow(y, (1.0/3.0)) : (7.787 * y + (16.0/116.0));
    		fz = (z > threshold) ? Math.pow(z, (1.0/3.0)) : (7.787 * z + (16.0/116.0));
    		// Compute Lab color value
    		x_l[n] = (y > threshold) ? (116.0 * Math.pow(y, (1.0/3.0)) - 16.0) : (903.3 * y);
    		y_a[n] = 500.0 * (fx - fy);
    		z_b[n] = 200.0 * (fy - fz);
    	}
    	return;
    }
    
    /**
     * Normalize an Lab image so that values for each channel lie in [0,1].
     * @param l
     * @param a
     * @param b
     * @return
     */
    private void lab_normalize(double l[], double a[], double b[]) {
        // Range for a, b channels
    	final double abMin = -73.0;
    	final double abMax = 95.0;
    	final double abRange = abMax - abMin;
    	// Normalize Lab image
    	int n;
    	double lVal;
    	double aVal;
    	double bVal;
    	for (n = 0; n < l.length; n++) {
    		lVal = l[n]/100.0;
    		aVal = (a[n] - abMin)/abRange;
    		bVal = (b[n] - abMin)/abRange;
    		if (lVal < 0.0) {
    			lVal = 0.0;
    		}
    		else if (lVal > 1.0) {
    			lVal = 1.0;
    		}
    		if (aVal < 0.0) {
    			aVal = 0.0;
    		}
    		else if (aVal > 1.0) {
    			aVal = 1.0;
    		}
    		if (bVal < 0.0) {
    			bVal = 0.0;
    		}
    		else if (bVal > 1.0) {
    			bVal = 1.0;
    		}
    		l[n] = lVal;
    		a[n] = aVal;
    		b[n] = bVal;
    	}
    }
    
    private void quantize_values(int assign[], double m[], int n_bins) {
    	int n;
    	int bin;
    	for (n = 0; n < m.length; n++) {
    	    bin = (int)Math.floor(m[n]*n_bins);
	    	if (bin == n_bins) {
	    		bin = n_bins-1;
	    	}
	    	assign[n] = bin;
    	}
    }
    
    /**
     * Filters for computing textons.
     * 
     * The set of texton filters is the union of the even and odd-symmetric
     * filter sets in aditon to a center-surround difference of Gaussians filter.
     * 
     * Each returned filter is an (s + 1) x (s + 1) matrix where s = 3 * sigma and
     * sigma is the specified standard deviation.
     * 
     * Filters are created in parallel when possible
     * @param filters
     * @param n_ori
     * @param sigma
     */
    private void texton_filters(ArrayList <double[][]> filters, int n_ori, double sigma) {
    	// Allocate collection to hold filters
    	int i;
    	ArrayList <double[][]> filters_even = new ArrayList <double[][]>();
    	ArrayList <double[][]> filters_odd = new ArrayList <double[][]>();
    	oe_filters_even(n_ori, sigma, filters_even);
    	oe_filters_odd(n_ori, sigma, filters_odd);
    	for (i = 0; i < filters_even.size(); i++) {
    		filters.add(filters_even.get(i));
    	}
    	filters_even.clear();
    	for (i = 0; i < filters_odd.size(); i++) {
    		filters.add(filters_odd.get(i));
    	}
    	filters_odd.clear();
    	/* compute center surround filter */
    	int support = (int)Math.ceil(3*sigma);
    	double [][]f_cs = gaussian_cs_2D(sigma, sigma, 0, Math.sqrt(2.0), support, support);
    	/* add center surround filter to collection */
    	filters.add(f_cs);
    	return;
    }
    
    private void oe_filters_even(int n_ori, double sigma, ArrayList <double[][]> filters_even) {
    	gaussian_filters(n_ori, sigma, 2, false, 3.0, filters_even);
    	return;
    }
    
    private void oe_filters_odd(int n_ori, double sigma, ArrayList <double[][]> filters_odd) {
    	gaussian_filters(n_ori, sigma, 2, true, 3.0, filters_odd);
    	return;
    }
    
    private void gaussian_filters(int n_ori, double sigma, int deriv, boolean hlbrt, double elongation,
    		ArrayList <double[][]> filters) {
    	double oris[] = new double[n_ori];
    	standard_filter_orientations(n_ori, oris);
    	gaussian_filters(oris, sigma, deriv, hlbrt, elongation, filters);
    	return;
    }
    
    private void standard_filter_orientations(int n_ori, double[] oris) {
    	int n;
    	double ori = 0.0;
    	double ori_step = (n_ori > 0) ? (Math.PI/(double)n_ori) : 0;
    	for (n = 0; n < n_ori; n++, ori += ori_step) {
    		oris[n] = ori;
    	}
    	return;
    }
    
    /**
     * Oriented Gaussian derivative filters
     * 
     * Create a filter set consisting of rotated versions of the Gaussian
     * derivative filter with the specified parameters.
     * 
     * Specify the standard deviation (sigma) along the longer principle axis.
     * The standard deviation along the other principle axisw is sigma/r where
     * r is the elongation ratio.
     * 
     * Each returned filter is an (s+1) x (s+1) matrix where s = 3 sigma.                       
     * @param oris
     * @param sigma
     * @param deriv
     * @param hlbrt
     * @param elongation
     * @param filters
     */
    private void gaussian_filters(double[] oris, double sigma, int deriv, boolean hlbrt, double elongation,
        ArrayList <double[][]> filters) {
    	int n;
    	// Compute support from sigma
    	int support = (int)Math.ceil(3 * sigma);
    	double sigmaX = sigma;
    	double sigmaY = sigmaX/elongation;
    	// Setup filter creators
    	int n_ori = oris.length;
    	for (n = 0; n < n_ori; n++) {
    	    double f[][] = gaussian_2D(sigmaX, sigmaY, oris[n], deriv, hlbrt, support, support);	
    	    filters.add(f);
    	}
    	return;
    }
    
    /**
     * Gaussian kernel (2D)
     * The kernel is normalized to have unit L1 norm.  If returning a 1st or 2nd derivative,
     * the kernel has zero mean.
     * @param sigmaX standard deviation along x axis
     * @param sigmaY standard deviation along y axis
     * @param ori orientation in radians
     * @param deriv The 1st or 2nd derivative can be taken along the y-axis prior to rotation
     * @param hlbrt The hilbert transform can be taken along the y-axis prior to rotation
     * @param supportX
     * @param supportY
     * @return
     */
    private double[][] gaussian_2D(double sigmaX, double sigmaY, double ori, int deriv, boolean hlbrt, int supportX,
    		int supportY) {
    	double m[][] = null;
    	// Compute size of larger grid for axis-aligned gaussian
    	// Reverse rotate corners of bounding box by orientation
    	int support_x_rot = support_x_rotated(supportX, supportY, -ori);
    	int support_y_rot = support_y_rotated(supportX, supportY, -ori);
    	// Compute 1D kernels
    	double mx[] = gaussian(sigmaX, 0, false, support_x_rot);
    	double my[] = gaussian(sigmaY, deriv, hlbrt, support_y_rot);
    	// Compute 2D kernel from product of 1D kernels
    	m = new double[mx.length][my.length];
    	for (int nx = 0; nx < mx.length; nx++) {
    		for (int ny = 0; ny < my.length; ny++) {
    			m[nx][ny] = mx[nx] * my[ny];
    		}
    	}
    	// Rotate 2D kernel by orientation
    	m = rotate_2D_crop(m, ori, 2*supportX + 1, 2*supportY + 1);
    	// Make zero mean (if returning derivative)
    	if (deriv > 0) {
    		double sum = 0.0;
    		for (int nx = 0; nx < m.length; nx++) {
    			for (int ny = 0; ny < m[0].length; ny++) {
    				sum += m[nx][ny];
    			}
    		}
    		double mean = sum/(m.length * m[0].length);
    		for (int nx = 0; nx < m.length; nx++) {
    			for (int ny = 0; ny < m[0].length; ny++) {
    				m[nx][ny] -= mean;
    			}
    		}
    	} // if (deriv > 0)
    	// Make unit L1 norm
    	double sum = 0.0;
    	for (int nx = 0; nx < m.length; nx++) {
			for (int ny = 0; ny < m[0].length; ny++) {
				sum += Math.abs(m[nx][ny]);
			}
		}
    	for (int nx = 0; nx < m.length; nx++) {
			for (int ny = 0; ny < m[0].length; ny++) {
			    m[nx][ny] = m[nx][ny]/sum;	
			}
    	}
    	return m;
    }
    
    /**
     * Gaussian center-surround kernel (2D).
     * 
     * Specify the standard deviation along each axis, the
     * orientation in radians, and the support.
     * 
     * The center-surround kernel is the difference of a Gaussian with the
     * specified standard deivation and one with a standard deviation scaled
     * by the specified factor.
     * 
     * The kernel is normalized to have unit L1 norm and zero mean.
     * @param sigmaX
     * @param sigmaY
     * @param ori
     * @param scaleFactor
     * @param supportX
     * @param supportY
     * @return
     */
    private double[][] gaussian_cs_2D(double sigmaX, double sigmaY, double ori, double scaleFactor,
    		int supportX, int supportY) {
    	// Compute standard deviation for center kernel
    	double sigmaXC = sigmaX/scaleFactor;
    	double sigmaYC = sigmaY/scaleFactor;
    	// Compute center and surround kernels
    	double m_center[][] = gaussian_2D(sigmaXC, sigmaYC, ori, 0, false, supportX, supportY);
    	double m_surround[][] = gaussian_2D(sigmaX, sigmaY, ori, 0, false, supportX, supportY);
    	// Compute center-surround kernel
    	double m[][] = new double[m_center.length][m_center[0].length];
    	int x;
    	int y;
    	for (x = 0; x < m.length; x++) {
    		for (y = 0; y < m[0].length; y++) {
    			m[x][y] = m_surround[x][y] - m_center[x][y];
    		}
    	}
    	// Make zero mean and unit L1 norm
    	double sum = 0.0;
    	for (x = 0; x < m.length; x++) {
    		for (y = 0; y < m[0].length; y++) {
    			sum += m[x][y];
    		}
    	}
    	double mean = sum/(m.length * m[0].length);
    	for (x = 0; x < m.length; x++) {
    		for (y = 0; y < m[0].length; y++) {
    		    m[x][y] -= mean;	
    		}
        }
    	sum = 0.0;
    	for (x = 0; x < m.length; x++) {
    		for (y = 0; y < m[0].length; y++) {
    			sum += Math.abs(m[x][y]);
    		}
    	}
    	for (x = 0; x < m.length; x++) {
    		for (y = 0; y < m[0].length; y++) {
    		    m[x][y] /= sum;	
    		}
    	}
    	return m;
    }
    
    /*
     * Rotate and pad with zeros, but crop the result to the specified size.
     */
    private double[][] rotate_2D_crop(
       double m[][], double ori, int size_x, int size_y)
    {
       /* compute rotation */
       double m_rot[][] = new double[size_x][size_y];
       compute_rotate_2D(m, m_rot, ori);
       return m_rot;
    }
    
    /*
     * Compute rotated 2D matrix using bilinear interpolation.
     */
    private void compute_rotate_2D(
       double[][]      m_src,       /* source matrix */
       double[][]      m_dst,       /* destination matrix */
       double ori)                /* orientation */
    {
       int size_x_src = m_src.length;
       int size_y_src = m_src[0].length;
       int size_x_dst = m_dst.length;
       int size_y_dst = m_dst[0].length;
       /* check that matrices are nonempty */
       if ((size_x_src > 0) && (size_y_src > 0) &&
           (size_x_dst > 0) && (size_y_dst > 0))
       {
          /* compute sin and cos of rotation angle */
          final double cos_ori = Math.cos(ori);
          final double sin_ori = Math.sin(ori);
          /* compute location of origin in src */
          final double origin_x_src = (size_x_src - 1.0)/ 2.0;
          final double origin_y_src = (size_y_src - 1.0) / 2.0;
          /* rotate */
          double u = -((size_x_dst - 1.0) / 2.0);
          int n = 0;
          for (int dst_x = 0; dst_x < size_x_dst; dst_x++) {
             double v = -((size_y_dst - 1.0) / 2.0);
             for (int dst_y = 0; dst_y < size_y_dst; dst_y++) {
                /* reverse rotate by orientation and shift by origin offset */
                double x = u * cos_ori + v * sin_ori + origin_x_src;
                double y = v * cos_ori - u * sin_ori + origin_y_src;
                /* check that location is in first quadrant */
                if ((x >= 0) && (y >= 0)) {
                   /* compute integer bounds on location */
                   int x0 = (int)Math.floor(x);
                   int x1 = (int)Math.ceil(x);
                   int y0 = (int)Math.floor(y);
                   int y1 = (int)Math.ceil(y);
                   /* check that location is within src matrix */
                   if ((0 <= x0) && (x1 < size_x_src) &&
                       (0 <= y0) && (y1 < size_y_src))
                   {
                      /* compute distances to bounds */
                      double dist_x0 = x - x0;
                      double dist_x1 = x1 - x;
                      double dist_y0 = y - y0;
                      double dist_y1 = y1 - y;
                      /* grab matrix elements */
                      final double m00 = m_src[x0][y0];
                      final double m01 = m_src[x0][y1];
                      final double m10 = m_src[x1][y0];
                      final double m11 = m_src[x1][y1];
                      /* interpolate in x-direction */
                      final double t0 =
                         (x0 != x1) ? (dist_x1 * m00 + dist_x0 * m10) : m00;
                      final double t1 =
                         (x0 != x1) ? (dist_x1 * m01 + dist_x0 * m11) : m01;
                      /* interpolate in y-direction */
                      m_dst[n/size_y_dst][n % size_y_dst] = (y0 != y1) ? (dist_y1 * t0 + dist_y0 * t1) : t0;
                   }
                }
                /* increment coordinate */
                n++;
                v++;
             }
             u++;
          }
       }
    }


    
    /*
     * Compute integer-valued supports for rotated 2D matrix.
     */
    private int support_x_rotated(
       int support_x, int support_y, double ori)
    {
       return (int)(
          Math.ceil(support_x_rotated(
             (double)(support_x), (double)(support_y), ori
          ))
       );
    }

    private int support_y_rotated(
       int support_x, int support_y, double ori)
    {
       return (int)(
          Math.ceil(support_y_rotated(
             (double)(support_x), (double)(support_y), ori
          ))
       );
    }

    
    private double support_x_rotated(double support_x, double support_y, double ori) {
    	   final double sx_cos_ori = support_x * Math.cos(ori);
    	   final double sy_sin_ori = support_y * Math.sin(ori);
    	   double x0_mag = Math.abs(sx_cos_ori - sy_sin_ori);
    	   double x1_mag = Math.abs(sx_cos_ori + sy_sin_ori);
    	   return (x0_mag > x1_mag) ? x0_mag : x1_mag;
    	}

    private double support_y_rotated(double support_x, double support_y, double ori) {
    	   final double sx_sin_ori = support_x * Math.sin(ori);
    	   final double sy_cos_ori = support_y * Math.cos(ori);
    	   double y0_mag = Math.abs(sx_sin_ori - sy_cos_ori);
    	   double y1_mag = Math.abs(sx_sin_ori + sy_cos_ori);
    	   return (y0_mag > y1_mag) ? y0_mag : y1_mag;
    	}
    
    /**
     * Gaussian kernel (1D).
     * The length of the returned vector is 2*support + 1.
     * The kernel is normalized to have unit L1 norm.
     * If returning a 1st or 2nd derivative, the kernel has zero mean
     * @param sigma
     * @param deriv
     * @param hlbrt
     * @param support
     * @return
     */
    private double[] gaussian(double sigma, int deriv, boolean hlbrt, int support) {
    	double m[] = null;
    	int n;
    	int i;
    	// Enlarge support so that hilbert transform can be done efficiently.
    	int support_big = support;
    	if (hlbrt) {
    		support_big = 1;
    		int temp = support;
    		while (temp > 0) {
    			support_big *= 2;
    			temp /= 2;
    		}
    	}
    	// Compute constants
    	final double sigma2_inv = 1.0/(sigma * sigma);
    	final double neg_two_sigma2_inv = -0.5 * sigma2_inv;
    	// Compute gaussian (or gaussian derivative).
    	int size = 2*support_big + 1;
    	m = new double[size];
    	double x = -(double)support_big;
    	if (deriv == 0) {
    		// Compute gaussian
    		for (n = 0; n < size; n++, x++) {
    		    m[n] = Math.exp(x*x*neg_two_sigma2_inv);	
    		}
    	}
    	else if (deriv == 1) {
    		// Compute gaussian first derivative
    		for (n = 0; n < size; n++, x++) {
    			m[n] = Math.exp(x*x*neg_two_sigma2_inv) * (-x);
    		}
    	}
    	else if (deriv == 2) {
    		// Compute gaussian second derivative
    		for (n = 0; n < size; n++, x++) {
    			double x2 = x * x;
    			m[n] = Math.exp(x2 * neg_two_sigma2_inv) * (x2*sigma2_inv - 1);
    		}
    	}
    	else {
    		MipavUtil.displayError("Only derivatives 0, 1, and 2 supported");
    		return null;
    	}
    	// Take hilbert transform (if requested)
    	if (hlbrt) {
    		// Grab power of two sized submatrix (ignore last element)
    		double mtemp[] = new double[m.length-1];
    		for (i = 0; i < m.length-1; i++) {
    			mtemp[i] = m[i];
    		}
    		m = null;
    		m = new double[mtemp.length];
    		for (i = 0; i < mtemp.length; i++) {
    			m[i] = mtemp[i];
    		}
    		mtemp = null;
    		// Grab desired submatrix after hilbert transform
    		int start = support_big - support;
    		int end = start + support + support;
    		m = hilbert(m);
       	    mtemp = new double[end - start + 1];
       	    for (i = start; i <= end; i++) {
       	    	mtemp[i-start] = m[i];
       	    }
       	    m = null;
       	    m = new double[mtemp.length];
       	    for (i = 0; i < mtemp.length; i++) {
       	    	m[i] = mtemp[i];
       	    }
       	    mtemp = null;
    	}
    	double sum;
    	// Make zero mean (if returning derivative)
    	if (deriv > 0) {
    		 sum = 0.0;
    		for (i = 0; i < m.length; i++) {
    			sum += m[i];
    		}
    		double mean = sum/m.length; 
    		for (i = 0; i < m.length; i++) {
    			m[i] = m[i] - mean;
    		}
    	} // if (deriv > 0)
    	// Make unit L1 norm
    	sum = 0.0;
    	for (i = 0; i < m.length; i++) {
    		sum += Math.abs(m[i]);
    	}
    	for (i = 0; i < m.length; i++) {
    		m[i] = m[i]/sum;
    	}
    	return m;
    }
    
    private double[] hilbert(double[] m) {
    	   /* get # steps along dimension */
    	   double mImag[] = new double[m.length];
    	   int n_steps = m.length;
    	   int half_n_steps = n_steps/2;
    	   /* compute fourier transform */
    	   FFTUtility fft = new FFTUtility(m, mImag, 1, m.length, 1,
    		-1, FFTUtility.FFT);
    	   fft.setShowProgress(false);
    	   fft.run();
    	   fft.finalize();
    	   fft = null;
    	   /* compute step size along dimension */
    	   int step_size = 1;
    	   /* double positive frequencies   */
    	   /* zero out negative frequencies */
    	   /* leave dc component            */
    	   boolean is_mult_two = (n_steps == (half_n_steps*2));
    	   int pos_freq_bound_ind = (n_steps+1)/2;
    	    int ind = 0;
    	    /* double positive frequencies */
    	    ind++;
    	    for (int n_step = 1; n_step < pos_freq_bound_ind; n_step++)
    	    {
    	       m[ind] *= 2;
    	       mImag[ind] *= 2;
    	       ind++;
    	    }
    	    /* zero out negative frequencies */
    	    ind += (is_mult_two ? step_size : 0);
    	    for (int n_step = (half_n_steps+1); n_step < n_steps; n_step++)
    	    {
    	       m[ind] = 0;
    	       mImag[ind] = 0;
    	       ind++;
    	    }
    	   /* compute inverse fourier transform */
    	    fft = new FFTUtility(m, mImag, 1, m.length, 1,
					+1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
    	   /* grab and return imaginary component */
    	   return mImag;
    	}
    
    
    


    
}
