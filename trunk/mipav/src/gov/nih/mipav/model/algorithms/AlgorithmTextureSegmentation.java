package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;
import Jama.Matrix;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;

/**  This software implements the factorization-based segmentation algorithm. 
 *   The original code was written in MATLAB and C by:
 *   Jiangye Yuan
 *   Computational Sciences and and Engineering Division
 *   Oak Ridge National Laboratory, Oak Ridge, Tennessee 37831
 *   yuanj@ornl.gov
 *   
 *   Reference:

     [1] J. Yuan and D. L. Wang. Factorization-based texture segmentation. Technical Report OSU-CISRC-1/13 -TR01, 2013.
     The website for this code is:
     https://sites.google.com/site/factorizationsegmentation/
     
     This code was ported to Java by William Gandler
 */

public class AlgorithmTextureSegmentation extends AlgorithmBase implements AlgorithmInterface {
	
	private static final int filterOp = 1;
	
	private int operationType = filterOp;
	
	private double filter[];
	
	// An integration scale.  A windowSize by windowSize square window
	private int windowSize = 25;
	
	// Number of segments.  Determined automatically if set to 0.
	private int segmentNumber = 0;
	
	// True when nonnegativity constraint imposed
	private boolean nonNegativity = true;
	
	private boolean removeSmallRegions = false;
	
	// For segment number estimation based on singular values.  May need to be
	// tuned if the choice of filters are changed.
	private double omega = 0.05;
	
	private int filterNumber = 11;
	
	private int binNumber = 11;
	
	private int xDim;
	
	private int yDim;
	
	
	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------
	public AlgorithmTextureSegmentation(ModelImage destImage, ModelImage srcImage,
			                            int windowSize, int segmentNumber, boolean nonNegativity,
			                            boolean removeSmallRegions) {
		super(destImage, srcImage);
		this.windowSize = windowSize;
		this.segmentNumber = segmentNumber;
		this.nonNegativity = nonNegativity;
		this.removeSmallRegions = removeSmallRegions;  
	}
	
	public void runAlgorithm() {
		xDim = srcImage.getExtents()[0];
		yDim = srcImage.getExtents()[0];
		int length = xDim * yDim;
		double srcBuffer[];
		double Ig[][][];
		int i;
		int kExtents[] = new int[2];
		int halfMask;
		double GData[];
		double denom;
		double sigma = 1.0;
		double kd;
		int x;
		int y;
		double distSquared;
		AlgorithmDConvolver convolver;
		boolean entireImage = true;
		int expandedSize;
		double expandedBuffer[];
		int extents[] = new int[2];
		ModelImage expandedImage;
		double S = 5;
		double f = 0.2;
		double scale;
		double xPrime;
		double yPrime;
		double theta = 0.0;
		int ws; 
		double sh_mx[][][];
		int bb;
		double Y[][];
		Matrix matY;
		double SE[][];
		double eigenvalue[];
	    double eigenvector[][];
	    int m;
	    int n;
	    int index;
	    double temp;
	    double tempCol[];
	    double sqk[];
	    double lse[];
	    int dimn;
	    double U1[][];
	    Matrix matU1;
	    double Y1[][];
	    double Y2[][][];
	    double tmp[][];
	    byte intreg[][];
	    double maxtmp;
	    double threshold;
	    int len;
	    double Mx[][];
	    double tmplt[][];
	    double L[];
	    double maxL;
	    int rn = 0;
	    double seedmap[][];
	    int idx[];
	    int tn;
	    double CY[][];
	    double ccos[];
	    double diff;
	    double maxccos;
	    int id = 0;
	    double cenInt[][];
	    double ccosArray[][];
	    int flag;
	    double minccos;
	    int clab[];
	    double NcenInt[][] = null;
	    int lengthtmind;
	    boolean equalArray;
	    Matrix Ncenmat;
	    double B[][];
	    double maxB;
	    int slab[];
	    double w0[][];
	    double dnorm0;
	    Matrix matw0;
	    double ww[][];
	    Matrix matwy;
	    Matrix matww;
	    double h[][] = null;
	    Matrix matH;
	    double hh[][];
	    Matrix mathY;
	    double w[][];
	    Matrix mathh;
	    Matrix matw;
	    double d[][];
	    double dnorm;
	    double maxh;
	    int segLabel[][];
	    int segLabelLarge[][];
		if (srcImage.isColorImage()) {
			
		}
		else {
			// Segment images with heavy texture
			// This code segments gray level images
			srcBuffer = new double[length];
			try {
				srcImage.exportData(0, length, srcBuffer);
			}
			catch(IOException e) {}
			
			Ig = new double[yDim][xDim][filterNumber];
			for (y = 0; y < yDim; y++)  {
				for (x = 0; x < xDim; x++) {
			        Ig[y][x][0] = srcBuffer[x + y * xDim];
				}
			}
			
			for (i = 1; i <= 10; i++) {
				if (i == 1) {
				    halfMask = 1;
				    sigma = 0.5;
				}
				else if (i == 2) {
					halfMask = 2;
					sigma = 1.0;
				}
				else if ((i >= 3) && (i <= 6)) {
					S = 1.5;
					halfMask = 3;
					f = 1.0/3.0;
				}
				else {
					S = 2.5;
					halfMask = 5;
					f = 0.2;
				}
				if ((i == 3) || (i == 7)) {
					theta = Math.PI/2.0;
				}
				else if ((i == 4) || (i == 8)) {
					theta = 0.0;
				}
				else if ((i == 5) || (i == 9)) {
					theta = Math.PI/4.0;
				}
				else if ((i == 6) || (i == 10)) {
					theta = -Math.PI/4.0;
				}
				expandedSize = (xDim + 2 * halfMask) * (yDim + 2 * halfMask);
	        	expandedBuffer = new double[expandedSize];
	        	for (y = 0; y < yDim; y++) {
	        		for (x = 0; x < xDim; x++) {
	        			expandedBuffer[x + halfMask + (y + halfMask) * (xDim + 2 * halfMask)] = srcBuffer[x + y * xDim];
	        		}
	        	}
	        	for (x = 0; x < halfMask; x++) {
	        		for (y = 0; y < halfMask; y++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[0];	
	        		}
	        		
	        		for (y = halfMask; y <= yDim + halfMask - 1; y++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(halfMask - 1 - x) + 
	        			                                                          (y - halfMask)* xDim];	
	        		}
	        		
	        		for (y = yDim + halfMask; y <= yDim + 2*halfMask - 1; y++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(yDim - 1)* xDim];	
	        		}
	        	} // for (x = 0; x < halfMask; x++)
	        	
	        	for (x = xDim + halfMask; x < xDim + 2 * halfMask; x++) {
	        		for (y = 0; y < halfMask; y++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[xDim-1];	
	        		}
	        		
	        		for (y = halfMask; y <= yDim + halfMask - 1; y++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(y - halfMask)* xDim + 
	        			                                                          + (xDim- 1 - (x - xDim - halfMask))];	
	        		}
	        		
	        		for (y = yDim + halfMask; y <= yDim + 2*halfMask - 1; y++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(yDim - 1)* xDim + xDim - 1];	
	        		}	
	        	} // for (x = xDim + halfMask; x < xDim + 2 * halfMask; x++)
	        	
	        	for (y = 0; y < halfMask; y++) {
	        		for (x = halfMask; x <= xDim + halfMask - 1; x++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[x - halfMask +
	        			                                                          (halfMask - 1 - y) * xDim];		
	        		}
	        	}
	        	
	        	for (y = yDim + halfMask; y < yDim + 2*halfMask; y++) {
	        		for (x = halfMask; x <= xDim + halfMask - 1; x++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = 
	        					srcBuffer[(yDim - 1 - (y - yDim - halfMask))* xDim + x - halfMask];		
	        		}
	        	}
	        	
	        	extents[0] = xDim + 2 * halfMask;
	        	extents[1] = yDim + 2 * halfMask;
	        	expandedImage = new ModelImage(ModelStorageBase.DOUBLE, extents, "expandedImage");
	        	try {
	        		expandedImage.importData(0, expandedBuffer, true);
	        	}
	        	catch(IOException e) {
	        		MipavUtil.displayError("IOException " + e + " on expandedImage.importData(0, expandedBuffer, true)");
	        		setCompleted(false);
	        		return;
	        	}
				kExtents[0] = 2*halfMask + 1;
	        	kExtents[1] = 2*halfMask + 1;
	        	GData = new double[kExtents[0] * kExtents[1]];
	        	if ((i == 1) || (i == 2)) {
		        	denom = 2.0 * sigma * sigma;
		        	kd = -1.0/(Math.PI * sigma * sigma);
		        	for (y = -halfMask; y <= halfMask; y++) {
		        		for (x = -halfMask; x <= halfMask; x++) {
		        		    distSquared = x * x + y * y;
		        		    GData[(x + halfMask) + (y + halfMask) * kExtents[0]] = (kd * (1.0 - distSquared/denom) *
		        		    		         Math.exp(-distSquared/denom));
		        		}
		        	} // for (y = -halfMask; y <= halfMask; y++)
	        	} // if ((i == 1) || ( i == 2))
	        	else { // i >= 3 && i <= 10
	        	    scale = 1.0/(2.0 * Math.PI * S * S);
	        	    for (y = -halfMask; y <= halfMask; y++) {
		        		for (x = -halfMask; x <= halfMask; x++) {
		        		    xPrime = x * Math.cos(theta) + y * Math.sin(theta);
		        		    yPrime = y * Math.cos(theta) - x * Math.sin(theta);
		        		    GData[(x + halfMask) + (y + halfMask) * kExtents[0]] = scale * 
		        		    		Math.exp(-0.5*(xPrime*xPrime + yPrime*yPrime)/(S*S)) *
		        		    		Math.cos(2.0 * Math.PI * f * xPrime);
		        		}
	        	    }
	        	} // else i >= 3 && i <= 10
	        	convolver = new AlgorithmDConvolver(expandedImage, GData, kExtents,entireImage, image25D);
		        convolver.addListener(this);
		        operationType = filterOp;
		        convolver.run();
		        for (y = halfMask; y <= yDim + halfMask -1; y++) {
		        	for (x = halfMask; x <= xDim + halfMask - 1; x++) {
		        	    Ig[y - halfMask][x - halfMask][i]= filter[x + y * (xDim + 2 * halfMask)];	
		        	}
		        }
			} // for (i = 1; i <= 10; i++)
			
			ws = windowSize/2;
			sh_mx = new double[filterNumber*binNumber][yDim][xDim];
			SHcomp(sh_mx,ws,Ig);
			for (y = 0; y < yDim; y++) {
				for (x = 0;x < xDim; x++) {
					Ig[y][x] = null;
				}
				Ig[y] = null;
			}
			Ig = null;
			bb = filterNumber * binNumber;
			Y = new double[bb][yDim * xDim];
			for (i = 0; i < bb; i++) {
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						Y[i][x + y * xDim] = sh_mx[i][y][x];
					}
				}
			}
			for (i = 0; i < bb; i++) {
				for (y = 0; y < yDim; y++) {
					sh_mx[i][y] = null;
				}
				sh_mx[i] = null;
			}
			sh_mx = null;
			matY = new Matrix(Y);
			SE = (matY.times(matY.transpose())).getArray();
			eigenvalue = new double[SE[0].length];
			eigenvector = new double[SE.length][SE[0].length];
			Eigenvalue.decompose(SE, eigenvector, eigenvalue);
			
			// Arrange the eigenvalues and corresponding eigenvectors
	        // in descending order so that e0 >= e1 >= e2
	        // Only the largest eigenvalue should be positive
			tempCol = new double[SE.length];
	        for (m = 0; m < SE[0].length; m++) {
	            index = m;

	            for (n = m + 1; n < SE[0].length; n++) {

	                if (eigenvalue[n] > eigenvalue[index]) {
	                    index = n;
	                }
	            } // for (m = m+1; n < SE[0].length; n++)

	            if (index != m) {
	                temp = eigenvalue[m];
	                eigenvalue[m] = eigenvalue[index];
	                eigenvalue[index] = temp;

	                for (n = 0; n < SE.length; n++) {
	                    tempCol[n] = eigenvector[n][m];
	                    eigenvector[n][m] = eigenvector[n][index];
	                    eigenvector[n][index] = tempCol[n];
	                }
	            } // if (index != m)
	        } // for (m = 0; m < Se[0].length; m++)
	        sqk = new double[SE[0].length];
	        for (i = 0; i < SE[0].length; i++) {
	        	sqk[i] = Math.sqrt(Math.abs(eigenvalue[i]));
	        }
	        
	        if (segmentNumber == 0) {
	        	// Estimate the segment number
	        	temp = 0.0;
	        	lse = new double[sqk.length];
	        	for (i = sqk.length-1; i >= 0; i--) {
	        	    temp = temp + sqk[i]*sqk[i];
	        	    lse[i] = temp/yDim/xDim;
	        	    if (lse[i] > omega) {
	        	    	segmentNumber++;
	        	    }
	        	} // for (i = sqk.length-1; i >= 0; i--)
	        	if (segmentNumber <= 1) {
	        		segmentNumber = 2;
	        		MipavUtil.displayWarning("Segment numnber is set to 2.  Adjust omega for better results.");
	        	}
	        } // if (segmentNumber == 0)
	        
	        dimn = segmentNumber;
	        U1 = new double[eigenvector.length][dimn];
	        for (y = 0; y < eigenvector.length; y++) {
	        	for (x = 0; x < dimn; x++) {
	        		U1[y][x] = eigenvector[y][x];
	        	}
	        }
	        matU1 = new Matrix(U1);
	        // Project features onto the subspace
	        // Y1 is dimn by yDim*xDim
	        Y1 = (((matY.transpose()).times(matU1)).transpose()).getArray();
	        Y2 = new double[dimn][yDim][xDim];
	        for (i = 0; i < dimn; i++) {
	        	for (y = 0; y < yDim; y++) {
	        		for (x = 0; x < xDim; x++) {
	        			Y2[i][y][x] = Y1[i][x + y * xDim];
	        		}
	        	}
	        }
	        tmp = new double[yDim][xDim];
	        SHedge_ls(tmp, ws, 2, Y2);
	        intreg = new byte[yDim][xDim];
	        maxtmp = -Double.MAX_VALUE;
	        for (y = 0; y < yDim; y++) {
	        	for (x = 0; x < xDim; x++) {
	        		if (tmp[y][x] > maxtmp) {
	        			maxtmp = tmp[y][x];
	        		}
	        	}
	        }
	        threshold = 0.4 * maxtmp;
	        len = 0;
	        for (y = ws; y < yDim-ws; y++) {
	        	for (x = ws; x < xDim-ws; x++) {
	        		if (tmp[y][x] < threshold) {
	        			intreg[y][x] = 1;
	        			len++;
	        		}
	        	}
	        }
	        Mx = new double[dimn][len];
	        idx = new int[len];
	        for (index = 0, x = ws; x < xDim - ws; x++) {
	        	for (y = ws; y < yDim - ws; y++) {
	        	    if (intreg[y][x] == 1) {
	        	        for (i = 0; i < dimn; i++) {
	        	        	Mx[i][index] = Y1[i][x + y * xDim];
	        	        }
	        	        idx[index] = x + y * xDim;
	        	        index++;
	        	    }
	        	}
	        }
	        
	        // Representative feature estimation
	        
	        tmplt = new double[dimn][segmentNumber];
	        L = new double[len];
	        maxL = -Double.MAX_VALUE;
	        for (x = 0; x < len; x++) {
	        	for (y = 0; y < dimn; y++) {
	        		L[x] = L[x] + (Mx[y][x]*Mx[y][x]);
	        	}
	        	if (L[x] > maxL) {
	        		maxL = L[x];
	        		rn = x;
	        	}
	        }
	        for (i = 0; i < dimn; i++) {
	        	tmplt[i][0] = Mx[i][rn];
	        }
	        n = 1;
	        
	        seedmap = new double[yDim][xDim];
	        y = idx[rn]/xDim;
	        x = idx[rn] % xDim;
	        seedmap[y][x] = 1;
	        
	        tn = n+1;
	        CY = new double[dimn][len];
	        for (y = 0; y < dimn; y++) {
	        	for (x = 0; x < len; x++) {
	        		CY[y][x] = tmplt[y][n-1];
	        	}
	        }
	        ccos = new double[len];
	        maxccos = -Double.MAX_VALUE;
	        for (x = 0; x < len; x++) {
	        	for (y = 0; y < dimn; y++) {
	        		diff = Mx[y][x] - CY[y][x];
	        	    ccos[x] = ccos[x] + diff * diff; 	
	        	}
	        	ccos[x] = Math.sqrt(ccos[x]);
	        	if (ccos[x] > maxccos) {
	        		maxccos = ccos[x];
	        		id = x;
	        	}
	        }
	        for (y = 0; y < dimn; y++) {
	        	tmplt[y][tn-1] = Mx[y][id];
	        }
	        y = idx[id]/xDim;
	        x = idx[id] % xDim;
	        seedmap[y][x] = 1;
	        
	        while (tn < segmentNumber) {
	        	tmp = new double[tn][len];
	        	for (i = 0; i < tn; i++) {
	        		for (y = 0; y < dimn; y++) {
	        			for (x = 0; x < len; x++) {
	        				CY[y][x] = tmplt[y][i];
	        			}
	        		}
	        		for (x = 0; x < len; x++) {
	    	        	for (y = 0; y < dimn; y++) {
	    	        		diff = Mx[y][x] - CY[y][x];
	    	        		tmp[i][x] = tmp[i][x] + diff * diff;
	    	        	}
	    	        	tmp[i][x] = Math.sqrt(tmp[i][x]);
	        		}
	        	} // for (i = 0; i < tn; i++)
	        	tn = tn + 1;
	        	maxccos = -Double.MAX_VALUE;
	        	for (x = 0; x < len; x++) {
	        	    ccos[x] = Double.MAX_VALUE;
	        	    for (y = 0; y < tn; y++) {
	        	    	ccos[x] = Math.min(ccos[x], tmp[y][x]);
	        	    }
	        	    if (ccos[x] > maxccos) {
	        	    	maxccos = ccos[x];
	        	    	id = x;
	        	    }
	        	} // for (x = 0; x < len; x++)
	        	for (y = 0; y < dimn; y++) {
		        	tmplt[y][tn-1] = Mx[y][id];
		        }
		        y = idx[id]/xDim;
		        x = idx[id] % xDim;
		        seedmap[y][x] = 1;
	        } // while (tn < segmentNumber)
	        
	        cenInt = new double[dimn][segmentNumber];
	        for (y = 0; y < dimn; y++) {
	        	for (x = 0; x < segmentNumber; x++) {
	        		cenInt[y][x] = tmplt[y][x];
	        	}
	        }
	        ccosArray = new double[segmentNumber][len];
	        clab = new int[len];
	        flag = 1;
	        
	        while (flag == 1) {
	            for (i = 0; i < segmentNumber; i++) {
	            	for (y = 0; y < dimn; y++) {
	            		for (x = 0; x < len; x++) {
	            			CY[y][x] = cenInt[y][i];
	            		}
	            	}
	            	for (x = 0; x < len; x++) {
	    	        	for (y = 0; y < dimn; y++) {
	    	        		diff = Mx[y][x] - CY[y][x];
	    	        		ccosArray[i][x] = ccosArray[i][x] + diff * diff;
	    	        	}
	    	        	ccosArray[i][x] = Math.sqrt(ccosArray[i][x]);
	        		}
	            } // for (i = 0; i < segmentNumber; i++)
	            
	            
	            for (x = 0; x < len; x++) {
	            	minccos = Double.MAX_VALUE;
	            	for (y = 0; y < segmentNumber; y++) {
	            		if (ccosArray[y][x] < minccos) {
	            			minccos = ccosArray[y][x];
	            			clab[x] = y;
	            		}
	            	}
	            } // for (x = 0; x < len; x++)
	            NcenInt = new double[dimn][segmentNumber];
	            
	            for (i = 0; i < segmentNumber; i++) {
	            	lengthtmind = 0;
	                for (x = 0; x < len; x++) {
	                    if (clab[x]  == i)	{
	                        lengthtmind++;	
	                    }
	                } // for (x = 0; x < len; x++)
	                tmp = new double[dimn][lengthtmind];
	                for (index = 0, x = 0; x < len; x++) {
	                	if (clab[x] == i) {
	                	    for (y = 0; y < dimn; y++) {
	                	    	tmp[y][index] = Mx[y][x];
	                	    }
	                	    index++;
	                	}
	                } // for (index = 0, x = 0; x < len; x++)
	                for (y = 0; y < dimn; y++) {
	                	for (x = 0; x < lengthtmind; x++) {
	                		NcenInt[y][i] = NcenInt[y][i] + tmp[y][x];
	                	}
	                	NcenInt[y][i] = NcenInt[y][i]/lengthtmind;
	                }
	            } // for (i = 0; i < segmentNumber; i++)
	            
	            equalArray = true;
	            for (y = 0; y < dimn && equalArray; y++) {
	            	for (x = 0; x < segmentNumber && equalArray; x++) {
	            		if (NcenInt[y][x] != cenInt[y][x]) {
	            		    equalArray = false;	
	            		}
	            	}
	            }
	            
	            if (equalArray) {
	            	flag = 0;
	            }
	            else {
	            	for (y = 0; y < dimn; y++) {
	            		for (x = 0; x < segmentNumber; x++) {
	            		    cenInt[y][x] = NcenInt[y][x];	
	            		}
	            	}
	            }
	        } // while (flag == 1)
	        
	        Ncenmat = new Matrix(NcenInt);
	        B = ((((Ncenmat.transpose().times(Ncenmat)).inverse()).times(Ncenmat.transpose())).times(new Matrix(Y1))).getArray();
	        slab = new int[yDim * xDim];
	        for (x = 0; x < yDim * xDim; x++) {
	        	maxB = -Double.MAX_VALUE;
	        	for (y = 0; y < segmentNumber; y++) {
	        	    if (B[y][x] > maxB) {
	        	    	maxB = B[y][x];
	        	    	slab[x] = y;
	        	    }
	        	}
	        }
	        
	        // Impose nonnegativity constraint
	        if (nonNegativity) {
	            w0 = (matU1.times(Ncenmat)).getArray();
	            for (y = 0; y < bb; y++) {
	            	for (x = 0; x < segmentNumber; x++) {
	            		if (w0[y][x] < 0.0) {
	            			w0[y][x] = 0.0;
	            		}
	            	}
	            }
	            
	            dnorm0 = 1.0E5;
	            for (i = 1; i <= 100; i++) {
	                matw0 = new Matrix(w0);
	                ww = ((matw0.transpose()).times(matw0)).getArray();
	                for (y = 0; y < segmentNumber; y++) {
	                	ww[y][y] = ww[y][y] + 0.1;
	                }
	                matww = new Matrix(ww);
	                matwy = (matw0.transpose()).times(matY);
	                h = ((matww.inverse()).times(matwy)).getArray();
	                for (y = 0; y < segmentNumber; y++) {
	                	for (x = 0; x < yDim * xDim; x++) {
	                		if (h[y][x] < 0.0) {
	                			h[y][x] = 0.0;
	                		}
	                	}
	                } // for (y = 0; y < segmentNumber; y++)
	                matH = new Matrix(h);
	                hh = (matH.times(matH.transpose())).getArray();
	                for (y = 0; y < segmentNumber; y++) {
	                	hh[y][y] = hh[y][y] + 0.1;
	                }
	                mathh = new Matrix(hh);
	                mathY = matH.times(matY.transpose());
	                w = ((mathh.inverse()).times(mathY)).getArray();
	                for (y = 0; y < segmentNumber; y++) {
	                	for (x = 0; x < bb; x++) {
	                		if (w[y][x] < 0.0) {
	                			w[y][x] = 0.0;
	                		}
	                	}
	                }
	                matw = new Matrix(w);
	                matw = matw.transpose();
	                
	                d = (matY.minus(matw.times(matH))).getArray();
	                dnorm = 0.0;
	                for (y = 0; y < bb; y++) {
	                	for (x = 0; x < yDim * xDim; x++) {
	                	    dnorm = dnorm + d[y][x] *d[y][x];	
	                	}
	                }
	                dnorm = Math.sqrt(dnorm);
	                dnorm = dnorm/(bb*yDim*xDim);
	                for (i = 0; i < d.length; i++) {
	                	d[i] = null;
	                }
	                d = null;
	                
	                // Check for convergence
	                if (i > 1) {
	                	if (Math.abs(dnorm0 - dnorm) <= 1.0E-3) {
	                		break;
	                	}
	                }
	                
	                for (y = 0; y < bb; y++) {
	                	for (x = 0; x < segmentNumber; x++) {
	                		w0[y][x] = w[y][x];
	                	}
	                }
	                dnorm0 = dnorm;
	            } // for (i = 1; i <= 100; i++)
	            for (x = 0; x < yDim * xDim; x++) {
		        	maxh = -Double.MAX_VALUE;
		        	for (y = 0; y < segmentNumber; y++) {
		        	    if (h[y][x] > maxh) {
		        	    	maxh = h[y][x];
		        	    	slab[x] = y;
		        	    }
		        	}
		        }
	        } // if (nonNegativity)
	        
	        segLabel = new int[yDim][xDim];
	        for (y = 0; y < yDim; y++) {
	            for (x = 0; x < xDim; x++) {
	                segLabel[y][x] = slab[x + y * xDim];	
	            }
	        }
	        
	        if (removeSmallRegions) {
	        	segLabelLarge = new int[yDim][xDim];
	        	RmSmRg(segLabelLarge, segLabel, 100);
	        }
		} // else not color
		
	}
	
	private void RmSmRg(int segLabelLarge[][], int segLabel[][], int minSize) {
	    int mark[] = new int[yDim * xDim];
	    int stk[] = new int[2 * yDim * xDim];
	    int labelN = 0;
	    int yy;
	    int xx;
	    int y;
	    int x;
	    int tgt;
	    int tb[] = new int[1];
	    int bb[] = new int[1];
	    int lb[] = new int[1];
	    int rb[] = new int[1];
	    int cnt[] = new int[1];
	    
	    for (yy = 0; yy < yDim; yy++) {
	    	for (xx = 0; xx < xDim; xx++) {
	    	    if (segLabelLarge[yy][xx] == 0) {
	    	        tgt = segLabel[yy][xx];
	    	        labelN++;
	    	        tb[0] = yy;
	    	        bb[0] = yy;
	    	        lb[0] = xx;
	    	        rb[0] = xx;
	    	        
	    	        cnt[0] = 0;
	    	        expand(yy, xx, stk, mark, segLabelLarge, segLabel, labelN, cnt, tb, bb, lb, rb, tgt);
	    	        if (cnt[0] < minSize) {
	    	            for (y = tb[0]; y <= bb[0]; y++) {
	    	            	
	    	            }
	    	        }
	    	    } // if (segLabelLarge[y][x] == 0) 
	    	} // for (x = 0; x < xDim; x++)
	    } // for (y = 0; y < yDim; y++)
	}
	
	// Non-recursive growing
	private void expand(int y, int x, int stk[], int mark[], int segLabelLarge[][], int segLabel[][], int labelN, int cnt[],
			            int tb[], int bb[], int lb[], int rb[], int tgt) {
	    int yy;
	    int xx;
	    int stkcnt;
	    
	    stkcnt = 1;
	    stk[stkcnt-1] = y;
	    stk[stkcnt-1 + yDim*xDim] = x;
	    
	    while (stkcnt > 0) {
	    	yy = stk[stkcnt-1];
	    	xx = stk[stkcnt-1 + yDim*xDim];
	        stkcnt = stkcnt-1;
	        if (segLabelLarge[yy][xx] == labelN) {
	            continue;	
	        }
	        segLabelLarge[yy][xx] = labelN;
	        cnt[0]++;
	        if (yy < tb[0]) {
	        	tb[0] = yy;
	        }
	        if (yy > bb[0]) {
	        	bb[0] = yy;
	        }
	        if (xx < lb[0]) {
	        	lb[0] = xx;
	        }
	        if (xx > rb[0]) {
	        	rb[0] = xx;
	        }
	        
	        if ((yy + 1 < yDim) && segLabel[yy+1][xx] == tgt && segLabelLarge[yy+1][xx] == 0) {
	        	stkcnt = stkcnt + 1;
	        	stk[stkcnt-1] = yy + 1;
	        	stk[stkcnt-1+yDim*xDim] = xx;
	        }
	        if ((xx + 1 < xDim) && segLabel[yy][xx+1] == tgt && segLabelLarge[yy][xx+1] == 0) {
	        	stkcnt = stkcnt + 1;
	        	stk[stkcnt-1] = yy;
	        	stk[stkcnt-1 + yDim*xDim] = xx + 1;
	        }
	        if (( yy - 1 >= 0) && segLabel[yy-1][xx] == tgt && segLabelLarge[yy-1][xx] == 0) {
	        	stkcnt = stkcnt + 1;
	        	stk[stkcnt-1] = yy - 1;
	        	stk[stkcnt-1 + yDim*xDim] = xx;
	        }
	        if ((xx - 1 >= 0) && segLabel[yy][xx-1] == tgt && segLabelLarge[yy][xx-1] == 0) {
	        	stkcnt = stkcnt + 1;
	        	stk[stkcnt-1] = yy;
	        	stk[stkcnt-1 + yDim*xDim] = xx-1;
	        }
	    } // while (stkcnt > 0)
	}
	
	private void SHedge_ls(double EdgeMap[][], int ws, int dism, double sh_mx[][][]) {
	    int y;
	    int x;
	    int k;
	    int dn = sh_mx.length;
	    double up[] = new double[dn];
	    double bt[] = new double[dn];
	    double lf[] = new double[dn];
	    double rt[] = new double[dn];
	    
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    	    for (k = 0; k < dn; k++) {
	    	        up[k] = sh_mx[k][Math.max(0, y-ws)][x];	
	    	    }
	    	    for (k = 0; k < dn; k++) {
	    	        bt[k] = sh_mx[k][Math.min(yDim-1, y + ws)][x];	
	    	    }
	    	    for (k = 0; k < dn; k++) {
	    	    	lf[k] = sh_mx[k][y][Math.max(0,  x - ws)];
	    	    }
	    	    for (k = 0; k < dn; k++) {
	    	    	rt[k] = sh_mx[k][y][Math.min(xDim-1,  x + ws)];
	    	    }
	    	    if (dism == 1) {
	    	        EdgeMap[y][x] = x2dist(up,bt,dn) + x2dist(lf,rt,dn);	
	    	    }
	    	    else {
	    	        EdgeMap[y][x] = Math.sqrt(l2dist(up,bt,dn) + l2dist(lf,rt,dn));	
	    	    }
	    	}
	    }
	}
	
	private double x2dist(double a[], double b[], int k) {
		int i;
		double T;
		double sum;
		double diff;
		T = 0.0;
		for (i = 0; i <k; i++) {
			sum = a[i] + b[i];
			if (sum != 0.0) {
				diff = a[i] - b[i];
				T = T + diff*diff/sum;
			}
		}
		return T;
	}
	
	private double l2dist(double a[], double b[], int k) {
	    int i;
	    double T;
	    double diff;
	    T = 0.0;
	    for (i = 0; i < k; i++) {
	    	diff = a[i] - b[i];
	    	T = T + diff*diff;
	    }
	    return T;
	}
	
	private void SHcomp(double sh_mx[][][], int ws, double Ig[][][]) {
	    // Compute integral histograms	
		double HImap[] = new double[binNumber*filterNumber*xDim*yDim];
		int b;
		double Imax;
		double Imin;
		int y;
		int x;
		double U;
		double binc[] = new double[binNumber];
		int k;
		double md;
		double tmp;
		int mdid = 0;
		double tmpm[] = new double[binNumber];
		int dims[] = new int[3];
		int wtl[] = new int[2];
		int wbr[] = new int[2];
		int sz;
		
		dims[0] = filterNumber*binNumber;
		dims[1] = yDim;
		dims[2] = xDim;
		
		for (b = 0; b < filterNumber; b++) {
			Imax = -Double.MAX_VALUE;
			Imin = Double.MAX_VALUE;
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					if (Ig[y][x][b] > Imax) {
						Imax = Ig[y][x][b];
					}
					if (Ig[y][x][b] < Imin) {
						Imin = Ig[y][x][b];
					}
				}
			} // for (y = 0; y < yDim; y++)
			U = (Imax - Imin)/binNumber;
			
			for (k = 0; k < binNumber; k++) {
				binc[k] = Imin + k*U + U/2.0;
			}
			
			md = Imax;
			for (k = 0; k < binNumber; k++) {
			    tmp = Math.abs(Ig[0][0][b] - binc[k]);
			    if (tmp < md) {
			    	md = tmp;
			    	mdid = k;
			    }
			} // for (k = 0; k < binNumber; k++)
			HImap[b*binNumber + mdid] = 1.0;
			
			for (x = 1; x < xDim; x++) {
			    md = Imax;
			    for (k = 0; k < binNumber; k++) {
			        tmp = Math.abs(Ig[0][x][b] - binc[k]);
			        if (tmp < md) {
			        	md = tmp;
			        	mdid = k;
			        }
			    } // for (k = 0; k < binNumber; k++)
			    HImap[binNumber*b+mdid+x*binNumber*filterNumber*yDim] = 1.0;
			    for (k = 0; k < binNumber; k++) {
			    	HImap[binNumber*b+k+x*dims[0]*dims[1]] = HImap[binNumber*b+k+x*dims[0]*dims[1]] +
                                                             HImap[binNumber*b+k+(x-1)*dims[0]*dims[1]];
			    }
			} // for (x = 1; x < xDim; x++)
			
			for (y = 1; y < yDim; y++) {
			    md = Imax;
			    for (k = 0; k < binNumber; k++) {
			        tmp = Math.abs(Ig[y][0][b] - binc[k]);
			        tmpm[k] = 0.0;
			        if (tmp < md) {
			        	md = tmp;
			        	mdid = k;
			        }
			    } // for (k = 0; k < binNumber; k++)
			    HImap[binNumber*b+mdid+y*dims[0]] = 1.0;
			    tmpm[mdid] = 1.0;
			    for (k = 0; k < binNumber; k++) {
			    	HImap[binNumber*b+k+y*dims[0]] = HImap[binNumber*b+k+y*dims[0]] + HImap[binNumber*b+k+(y-1)*dims[0]];
			    }
			    for (x = 1; x < xDim; x++) {
			        md = Imax;
			        for (k = 0; k < binNumber; k++) {
			            tmp = Math.abs(Ig[y][x][b] - binc[k]);
			            if (tmp < md) {
			            	md = tmp;
			            	mdid = k;
			            }
			        } // for (k = 0; k < binNumber; k++)
			        tmpm[mdid] = tmpm[mdid] + 1.0;
			        for (k = 0; k < binNumber; k++) {
			            HImap[binNumber*b+k+y*dims[0]+x*dims[0]*dims[1]] = HImap[binNumber*b+k+(y-1)*dims[0]+x*dims[0]*dims[1]] +
			            		                                           tmpm[k];
			        } // for (k = 0; k < binNumber; k++)
			    } // for (x = 1; x < xDim; x++)
			} // for (y = 1; y < yDim; y++)
		} // for (b = 0; b < filterNumber; b++)
		
		// Compute local spectral histograms
		for (y = 0; y < yDim; y++) {
		    for (x = 0; x < xDim; x++) {
		        if (y - ws > 0)	{
		        	wtl[0] = y - ws;
		        }
		        else {
		        	wtl[0] = 0;
		        }
		        if (x - ws > 0) {
		        	wtl[1] = x - ws;
		        }
		        else {
		        	wtl[1] = 0;
		        }
		        if (y + ws < yDim - 1) {
		        	wbr[0] = y + ws;
		        }
		        else {
		        	wbr[0] = yDim - 1;
		        }
		        if (x + ws < xDim - 1) {
		        	wbr[1] = x + ws;
		        }
		        else {
		        	wbr[1] = xDim - 1;
		        }
		        
		        sz = (wbr[1]-wtl[1]+1)*(wbr[0]-wtl[0]+1);

	            if (wtl[0]==0 && wtl[1]==0) {
	                for (k=0; k<filterNumber*binNumber; k++) {
	                    sh_mx[k][y][x] = HImap[k+wbr[0]*dims[0]+wbr[1]*dims[0]*dims[1]]; }}             
	            else if (wtl[0]==0 && wtl[1]!=0) {
	                for (k=0; k<filterNumber*binNumber; k++) {
	                   sh_mx[k][y][x] = HImap[k+wbr[0]*dims[0]+wbr[1]*dims[0]*dims[1]] 
	                                 - HImap[k+wbr[0]*dims[0]+(wtl[1]-1)*dims[0]*dims[1]];} }                        
	            else if (wtl[0]!=0 && wtl[1]==0) {
	                for (k=0; k<filterNumber*binNumber; k++) {
	                    sh_mx[k][y][x] = HImap[k+wbr[0]*dims[0]+wbr[1]*dims[0]*dims[1]] 
	                                 - HImap[k+(wtl[0]-1)*dims[0]+wbr[1]*dims[0]*dims[1]];} }
	            else   
	                for (k=0; k<filterNumber*binNumber; k++) {
	                    sh_mx[k][y][x] = HImap[k+wbr[0]*dims[0]+wbr[1]*dims[0]*dims[1]] 
	                                   + HImap[k+(wtl[0]-1)*dims[0]+(wtl[1]-1)*dims[0]*dims[1]] 
	                                   - HImap[k+wbr[0]*dims[0]+(wtl[1]-1)*dims[0]*dims[1]] 
									   - HImap[k+(wtl[0]-1)*dims[0]+wbr[1]*dims[0]*dims[1]];}
	            for (k=0; k<filterNumber*binNumber; k++) {
	                sh_mx[k][y][x] = sh_mx[k][y][x]/sz;} 
		    } // for (x = 0; x < xDim; x++)
		} // for (y = 0; y < yDim; y++)
	}
	
	public void algorithmPerformed(AlgorithmBase algorithm){
        if(!algorithm.isCompleted()){
            finalize();
            return;
        }
        if (algorithm instanceof AlgorithmDConvolver) {
            AlgorithmDConvolver convolver = (AlgorithmDConvolver) algorithm;
            if (operationType == filterOp) {
               filter = convolver.getOutputBuffer();
            }
            
        }
    }
	
}