package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

public class AlgorithmEfficientWatershed extends AlgorithmBase {
	
	/* Initial value of a threshold level */
	private static final int MASK = -2;
	
	/* Value of the pixels belonging to the watersheds */
	private static final int WSHED = 0;
	
	/* Initial value for labelBuffer */
	private static final int INIT = -1;
	
	
	private boolean neighbor8;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	public AlgorithmEfficientWatershed(ModelImage destImage, ModelImage srcImage, boolean neighbor8) {
		super(destImage, srcImage);
		this.neighbor8 = neighbor8;
	}
	
	public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int zDim;
    	int tDim;
    	int nDims;
    	int length;
    	double imgBuffer[];
    	int labelBuffer[] = null;
    	int distanceBuffer[] = null;
    	int x;
    	int y;
    	int z;
    	int t;
    	int i;
    	int j;
    	int numValues;
    	double histBins[];
    	int indexBins[][];
    	int lasti;
    	int currentLabel;
    	int currentDist;
    	ArrayList <indexValueItem> indexValueList = new ArrayList<indexValueItem>();
    	ArrayList <Integer> frequencyCount = new ArrayList<Integer>();
    	int ip;
    	double hmin;
    	double hmax;
    	double h;
    	int index;
    	boolean exists;
    	boolean added;
    	Queue <Integer> fifo = new LinkedList<Integer>();
    	int fictitiousIndex = -1;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Efficient Watershed ...");
    	
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        nDims = srcImage.getNDims();
    	
        if (nDims > 2) {
            zDim = srcImage.getExtents()[2];
        } else {
            zDim = 1;
        }
        if (nDims > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        
        try {
            imgBuffer = new double[length];
            labelBuffer = new int[length];
            distanceBuffer = new int[length];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Efficient Watershed: Out of memory creating buffers");
            setCompleted(false);

            return;
        }

        for (t = 0; t < tDim; t++) {
        for (z = 0; z < zDim; z++) {

            try {
                srcImage.exportData((z + t*zDim)*length, length, imgBuffer);
            } catch (IOException error) {
                displayError("Algorithm Efficient Watershed: image bounds exceeded");
                setCompleted(false);
                
                srcImage.releaseLock();

                return;
            }
            indexValueList.clear();
            frequencyCount.clear();
            fifo.clear();
            
            for (i = 0; i < length; i++) {
            	indexValueList.add(new indexValueItem(i, imgBuffer[i]));	
            }
            
            Collections.sort(indexValueList, new indexValueComparator());
            numValues = 1;
            lasti = 0;
            for (i = 1; i < length; i++) {
            	if (indexValueList.get(i).getValue() > indexValueList.get(i-1).getValue()) {
            		numValues++;
            		frequencyCount.add(i-lasti);
            		lasti = i;
            	}
            }
            frequencyCount.add(length-1-lasti);
            histBins = new double[numValues];
            indexBins = new int[numValues][];
            for (i = 0; i < frequencyCount.size(); i++) {
            	indexBins[i] = new int[frequencyCount.get(i)];
            }
            histBins[0] = indexValueList.get(0).getValue();
            indexBins[0][0] = indexValueList.get(0).getIndex();
            for (i = 1, j = 0, ip = 1; i < length; i++) {
            	if (indexValueList.get(i).getValue() > indexValueList.get(i-1).getValue()) {
            		histBins[j++] = indexValueList.get(i).getValue();
            		ip = 0;
            		indexBins[j][ip++] = indexValueList.get(i).getIndex();
            	}
            	else {
            		indexBins[j][ip++] = indexValueList.get(i).getIndex();
            	}
            }
            
            
            for (i = 0; i < length; i++) {
            	labelBuffer[i] = INIT;
            	distanceBuffer[i] = 0;
            }
            currentLabel = 0;
            hmin = histBins[0];
            hmax = histBins[histBins.length-1];
            
            for (i = 0; i < numValues; i++) {
            	for (j = 0; j < indexBins[i].length; j++) {
            		index = indexBins[i][j];
            		labelBuffer[index] = MASK;
            		x = index % xDim;
            		y = index / xDim;
            		exists = false;
            		if ((x > 0) && ((labelBuffer[index-1] > 0) || (labelBuffer[index-1] == WSHED))) {
            			exists = true;
            		}
            		else if ((!exists) && (x < xDim-1) && ((labelBuffer[index+1] > 0) || (labelBuffer[index+1] == WSHED))) {
            			exists = true;
            		}
            		else if ((!exists) && (y > 0) && ((labelBuffer[index-xDim] > 0) || (labelBuffer[index-xDim] == WSHED))) {
            			exists =true;
            		}
            		else if ((!exists) && (y < yDim-1) && ((labelBuffer[index+xDim] > 0) || (labelBuffer[index+xDim] == WSHED))) {
            			exists = true;
            		}
            		else if ((!exists) && neighbor8) {
                        if ((x > 0) && (y > 0) && ((labelBuffer[index-xDim-1] > 0) || (labelBuffer[index-xDim-1] == WSHED))) {
                        	exists = true;
                        }
                        else if ((!exists) &&(x > 0) && (y < yDim-1) && 
                        		((labelBuffer[index+xDim-1] > 0) || (labelBuffer[index+xDim-1] == WSHED))) {
                        	exists = true;
                        }
                        else if ((!exists) && (x < xDim-1) && ( y > 0) &&
                        		((labelBuffer[index-xDim+1] > 0) || (labelBuffer[index-xDim+1] == WSHED))) {
                        	exists = true;
                        }
                        else if ((!exists) && (x < xDim-1) && (y < yDim-1) &&
                        		((labelBuffer[index+xDim+1] > 0) || (labelBuffer[index+xDim+1] == WSHED))) {
                        	exists = true;
                        }
            		} // else if ((!exists) && neighbor8)
            		if (exists) {
            			distanceBuffer[index] = 1;
            			added = fifo.offer(index);
            			if (!added) {
            				MipavUtil.displayError("Failure to add " + index + " to the fifo");
            				setCompleted(false);
            				return;
            			}
            		} // if (exists)
            	} // for (j = 0; j < indexBins[i].length; j++)
            	
            	currentDist = 1;
            	added = fifo.offer(fictitiousIndex);
            	if (!added) {
    				MipavUtil.displayError("Failure to add fictitiousIndex to the fifo");
    				setCompleted(false);
    				return;
    			}
            	while (true) {
            		index = fifo.poll();
            		if (index == fictitiousIndex) {
            			if (fifo.isEmpty()) {
            				break;
            			}
            			else {
            				added = fifo.offer(fictitiousIndex);
            				if (!added) {
                				MipavUtil.displayError("Failure to add fictitiousIndex to the fifo");
                				setCompleted(false);
                				return;
                			}
            				currentDist++;
            				index = fifo.poll();
            			}
            		} // if (index == fictiousIndex)
            	} // while (true)
            } // for (i = 0; i < numValues; i++)
            
            
            try {
			    destImage.importData((z + t*zDim)*length, labelBuffer, false);
			}
			catch(IOException e) {
				MipavUtil.displayError("IOException " + e + " on destImage.importData");
				setCompleted(false);
				return;
			}
        } // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)
        destImage.calcMinMax();
        
        setCompleted(true);
        return;
	}
	
	private class indexValueComparator implements Comparator<indexValueItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(indexValueItem o1, indexValueItem o2) {
            double a = o1.getValue();
            double b = o2.getValue();
            int i = o1.getIndex();
            int j = o2.getIndex();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else if (i < j) {
            	return -1;
            } else if (i > j) {
            	return 1;
            } else {
                return 0;
            }
        }

    }
	
	private class indexValueItem {
		private int index;
		private double value;
		
		public indexValueItem(int index, double value) {
			this.index = index;
			this.value = value;
		}
		
		public int getIndex() {
			return index;
		}
		
		public double getValue() {
			return value;
		}
	}
}