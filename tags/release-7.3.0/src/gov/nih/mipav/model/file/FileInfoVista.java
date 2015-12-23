package gov.nih.mipav.model.file;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogEditor;
import gov.nih.mipav.view.dialogs.JDialogFileInfo;



/**
 * File Info Vista
 * @author pandyan
 * 
 * 
 * Spec: http://static.cbs.mpg.de/lipsia/START/index3.html
 *
 */
public class FileInfoVista extends FileInfoBase {

    /** image info **/
    private ArrayList<HashMap<String,String>> imagesInfo;
    
    /** history info **/
    private ArrayList<String> historyInfo;
    
    int[] extents;
    
	
	/** constructor **/
	public FileInfoVista(String name, String directory, int format) {
		 super(name, directory, format);
	}
	
	
	

    /**
     * Displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogFileInfo dialog = (JDialogFileInfo) dlog;

        int i;
        int[] editorChoice = new int[1];
        editorChoice[0] = JDialogEditor.STRING;

        dialog.displayAboutInfo(this); // setup layout in the dialog

        extents = super.getExtents();

        for (i = 0; i < extents.length; i++) {
            dialog.appendPrimaryData("Dimension " + i, Integer.toString(extents[i]));
        }

        dialog.appendPrimaryData("Type", ModelStorageBase.getBufferTypeStr(getDataType()));

        if (ModelImage.isColorImage(getDataType())) {
            dialog.appendPrimaryData("Min red", Double.toString(getMinR()));
            dialog.appendPrimaryData("Max red", Double.toString(getMaxR()));
            dialog.appendPrimaryData("Min green", Double.toString(getMinG()));
            dialog.appendPrimaryData("Max green", Double.toString(getMaxG()));
            dialog.appendPrimaryData("Min blue", Double.toString(getMinB()));
            dialog.appendPrimaryData("Max blue", Double.toString(getMaxB()));

        } else {
            dialog.appendPrimaryData("Min", Double.toString(getMin()));
            dialog.appendPrimaryData("Max", Double.toString(getMax()));
        }

        dialog.appendPrimaryData("Modality", FileInfoBase.getModalityStr(getModality()));

        dialog.appendPrimaryData("Orientation", getImageOrientationStr(getImageOrientation()));

        float[] resolutions; // = new float[5];
        resolutions = getResolutions();

        int[] measure; // = new int[5];
        measure = getUnitsOfMeasure();

        for (i = 0; i < extents.length; i++) {

            if (resolutions[i] > 0.0) {
                String pixelRes = "Pixel resolution " + i;
                dialog.appendPrimaryData(pixelRes,
                                         Float.toString(resolutions[i]) + " " +
                                         (Unit.getUnitFromLegacyNum(measure[i])).toString());
            } // end of if (resolutions[i] > 0.0)
        } // for (i=0; i < 5; i++)

        if (getEndianess() == FileBase.LITTLE_ENDIAN) {
            dialog.appendPrimaryData("Endianess", "Little Endian");
        } else {
            dialog.appendPrimaryData("Endianess", "Big Endian");
        }

        if (matrix != null) {

            // when using displayAboutInfo(dialog) this doesn't appear
            // calling prg might use an editing panel to adjust this matrix
            dialog.appendPrimaryData("Matrix", matrix.matrixToString(10, 4));
        }
        
        
        
        if(historyInfo != null && historyInfo.size() > 0) {
        	String name = "";
        	String value = "";
        	
        	for(int k=0;k<historyInfo.size();k++) {
        		name = "history";
        		value = historyInfo.get(k);
        		dialog.appendSecondaryData(name, value);
        	}
        }
        
        HashMap<String,String> info = imagesInfo.get(0);
        Set<String> keySet = info.keySet();
        Iterator<String> iter = keySet.iterator();
        while(iter.hasNext()) {
        	String key = (String)iter.next();
        	if(extents.length == 4) {
	        	if(!key.equals("data") && !key.equals("length")) {
	        		String val = info.get(key);
	        		dialog.appendSecondaryData(key, val);
	        		
	        	}
        	}else {
        		String val = info.get(key);
        		dialog.appendSecondaryData(key, val);
        	}
        	
        }



    }




	public ArrayList<HashMap<String, String>> getImagesInfo() {
		return imagesInfo;
	}




	public void setImagesInfo(ArrayList<HashMap<String, String>> imagesInfo) {
		this.imagesInfo = imagesInfo;
	}




	public ArrayList<String> getHistoryInfo() {
		return historyInfo;
	}




	public void setHistoryInfo(ArrayList<String> historyInfo) {
		this.historyInfo = historyInfo;
	}
	
	
	
	
	
	
	
	
	
	

}
