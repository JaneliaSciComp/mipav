package gov.nih.mipav.model.algorithms;

import java.awt.Color;
import java.util.BitSet;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;


/**
 * Algorithm class for finding Hausdorff distance on 2 VOIs
 * @author ilb
 *
 */
public class AlgorithmVOIHausdorffDistance extends AlgorithmBase {
	
	/** Vector of all VOIs that will have calculations performed. */
    protected ViewVOIVector selectedVOIset;
    
    /** Model Images **/
    protected ModelImage image;
    
    
    /**
     * constructor
     * @param img
     * @param clonedImage
     * @param selectedVOIset
     */
	public AlgorithmVOIHausdorffDistance(ModelImage image , ViewVOIVector selectedVOIset) {
		this.image = image;
		this.selectedVOIset = selectedVOIset;
	}
	
	
	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		
		VOI voi1 = selectedVOIset.elementAt(0);
		VOI voi2 = selectedVOIset.elementAt(1);
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int zDim = 1;
        if (image.getNDims() > 2) {
        	zDim = image.getExtents()[2];
        }
        int sliceSize = xDim * yDim;
        int length = sliceSize * zDim;
		int i;
		boolean XOR = false;
		BitSet binaryMask = null;
		short mask[] = null;
		float res[] = image.getResolutions(0);
		Unit resXUnit = Unit.getUnitFromLegacyNum(image.getFileInfo(0).getUnitsOfMeasure(0));
		Unit resYUnit = Unit.getUnitFromLegacyNum(image.getFileInfo(0).getUnitsOfMeasure(1));
		if (resXUnit != resYUnit) {
			res[1] = (float) resYUnit.convertTo(res[1], resXUnit);	
		}
		if (image.getNDims() > 2) {
	        Unit resZUnit = Unit.getUnitFromLegacyNum(image.getFileInfo(0).getUnitsOfMeasure(2));
	        if (resXUnit != resZUnit) {
				 res[2] = (float) resZUnit.convertTo(res[2], resXUnit);
		    }
	    } // if (image.getNDims() > 2)

		try {
			binaryMask = new BitSet(length);
		} catch (OutOfMemoryError error) {
			MipavUtil.displayError("Out of memory: unable to make binaryMask.");

			setCompleted(false);
			return;
		}
		
		try {
			mask = new short[length];
		} catch (OutOfMemoryError error) {
			MipavUtil.displayError("Out of memory: unable to make mask.");

			setCompleted(false);
			return;
		}

		voi1.createBinaryMask3D(binaryMask, xDim, yDim, XOR, false);


		for (i = 0; i < length; i++) {

			if (binaryMask.get(i) == true) {
				mask[i] = 1;
			}
		}
		
		binaryMask.clear();
		
		voi2.createBinaryMask3D(binaryMask, xDim, yDim, XOR, false);


		for (i = 0; i < length; i++) {

			if (binaryMask.get(i) == true) {
				mask[i] += 2;
			}
		}
		
		double hausdorffDistanceSquared = -1.0;
		double hausdorffDistance;
		double pixelDistanceSquared;
		double currentDistanceSquared;
		int selectedXi = -1;
		int selectedYi = -1;
		int selectedZi = -1;
		int selectedXj = -1;
		int selectedYj = -1;
		int selectedZj = -1;
		int finalXj = -1;
		int finalYj = -1;
		int finalZj = -1;
		int j;
		int x, y, z, xj, yj, zj;
		double diffX;
		double diffY;
		double diffZ;
		int sourceVOI = -1;
		i = 0;
        for (z = 0; z < zDim; z++) {
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++, i++) {
        		    if ((mask[i] == 1) || (mask[i] == 3)) {
        		        j = 0;
        		        selectedXj = -1;
        		        selectedYj = -1;
        		        selectedZj = -1;
        		        pixelDistanceSquared = Double.MAX_VALUE;
        		        for (zj = 0; zj < zDim; zj++) {
        		            for (yj = 0; yj < yDim; yj++) {
        		            	for (xj = 0; xj < xDim; xj++, j++) {
        		            		if ((mask[j] == 2) || (mask[j] == 3)) {
        		            		    diffX = res[0] * (x - xj);
        		            		    diffY = res[1] * (y - yj);
        		            		    diffZ = res[2] * (z - zj);
        		            		    currentDistanceSquared = diffX * diffX + diffY * diffY + diffZ * diffZ;
        		            		    if (currentDistanceSquared < pixelDistanceSquared) {
        		            		    	pixelDistanceSquared = currentDistanceSquared;
        		            		    	selectedXj = xj;
        		            		    	selectedYj = yj;
        		            		    	selectedZj = zj;		
        		            		    } // if (currentDistanceSquared < pixelDistanceSquared)
        		            		} // if ((mask[j] == 2) || (mask[j] == 3))
        		            	} // for (xj = 0; xj < xDim; xj++, j++)
        		            } // for (yj = 0; yj < yDim; yj++)
        		        } //  for (zj = 0; zj < zDim; zj++)
        		        if (pixelDistanceSquared > hausdorffDistanceSquared) {
        		        	hausdorffDistanceSquared = pixelDistanceSquared;
        		        	selectedXi = x;
        		        	selectedYi = y;
        		        	selectedZi = z;
        		        	sourceVOI = 1;
        		        	finalXj = selectedXj;
        		        	finalYj = selectedYj;
        		        	finalZj = selectedZj;
        		        }
        		    } // if ((mask[i] == 1) || (mask[i] == 3)) 	
        		} // for (x = 0; x < xDim; x++, i++)
        	} // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)
        
        i = 0;
        for (z = 0; z < zDim; z++) {
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++, i++) {
        		    if ((mask[i] == 2) || (mask[i] == 3)) {
        		        j = 0;
        		        selectedXj = -1;
        		        selectedYj = -1;
        		        selectedZj = -1;
        		        pixelDistanceSquared = Double.MAX_VALUE;
        		        for (zj = 0; zj < zDim; zj++) {
        		            for (yj = 0; yj < yDim; yj++) {
        		            	for (xj = 0; xj < xDim; xj++, j++) {
        		            		if ((mask[j] == 1) || (mask[j] == 3)) {
        		            		    diffX = res[0] * (x - xj);
        		            		    diffY = res[1] * (y - yj);
        		            		    diffZ = res[2] * (z - zj);
        		            		    currentDistanceSquared = diffX * diffX + diffY * diffY + diffZ * diffZ;
        		            		    if (currentDistanceSquared < pixelDistanceSquared) {
        		            		    	pixelDistanceSquared = currentDistanceSquared;
        		            		    	selectedXj = xj;
        		            		    	selectedYj = yj;
        		            		    	selectedZj = zj;		
        		            		    } // if (currentDistanceSquared < pixelDistanceSquared)
        		            		} // if ((mask[j] == 1) || (mask[j] == 3))
        		            	} // for (xj = 0; xj < xDim; xj++, j++)
        		            } // for (yj = 0; yj < yDim; yj++)
        		        } //  for (zj = 0; zj < zDim; zj++)
        		        if (pixelDistanceSquared > hausdorffDistanceSquared) {
        		        	hausdorffDistanceSquared = pixelDistanceSquared;
        		        	selectedXi = x;
        		        	selectedYi = y;
        		        	selectedZi = z;
        		        	sourceVOI = 2;
        		        	finalXj = selectedXj;
        		        	finalYj = selectedYj;
        		        	finalZj = selectedZj;
        		        }
        		    } // if ((mask[i] == 2) || (mask[i] == 3)) 	
        		} // for (x = 0; x < xDim; x++, i++)
        	} // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)
        
        hausdorffDistance = Math.sqrt(hausdorffDistanceSquared);
        ViewUserInterface UI = ViewUserInterface.getReference();
        UI.setDataText("The Hausdorff distance = " + hausdorffDistance +  " " + resXUnit.toString() + "\n");
        String sourceName = null;
        String destName = null;
        if (sourceVOI == 1) {
            sourceName = voi1.getName();
            destName = voi2.getName();
        }
        else {
        	sourceName = voi2.getName();
        	destName = voi1.getName();
        }
        if (image.getNDims() > 2) {
            UI.setDataText("Starts at x = " + selectedXi + " y = " + selectedYi + " z = " + selectedZi + " in " + sourceName + "\n");
        }
        else {
        	UI.setDataText("Starts at x = " + selectedXi + " y = " + selectedYi + " in " + sourceName + "\n");	
        }
        
        if (image.getNDims() > 2) {
            UI.setDataText("Ends at x = " + finalXj + " y = " + finalYj + " z = " + finalZj + " in " + destName + "\n");
        }
        else {
        	UI.setDataText("Ends at x = " + finalXj + " y = " + finalYj + " in " + destName + "\n");	
        }
        
        if (selectedZi == finalZj) {
        	VOI hLineVOI = new VOI((short) 0, "hLine", VOI.LINE, -1.0f);
        	int xArray[] = new int[]{selectedXi, finalXj};
        	int yArray[] = new int[]{selectedYi, finalYj};
        	int zArray[] = new int[]{selectedZi, finalZj};
            hLineVOI.importCurve(xArray, yArray, zArray);
            image.registerVOI(hLineVOI);
            hLineVOI.setFixed(true);
            hLineVOI.setColor(Color.orange);
            image.notifyImageDisplayListeners();
        }
        
        setCompleted(true);
        return;
	}


	

}
