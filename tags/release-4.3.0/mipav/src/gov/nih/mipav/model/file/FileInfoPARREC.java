package gov.nih.mipav.model.file;

import java.util.HashMap;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogFileInfo;
import gov.nih.mipav.view.dialogs.JDialogEditor;
import gov.nih.mipav.view.dialogs.JDialogText;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.ModelImage;

/**
 * Created by IntelliJ IDEA.
 * User: bennett
 * Date: Mar 16, 2007
 * Time: 9:18:41 AM
 * To change this template use File | Settings | File Templates.
 */
public class FileInfoPARREC extends FileInfoBase {
	
	

    /** vol parameters **/
    private static HashMap VolParameters;

    
    /** slice specific info **/
    private String sliceInfo;
    
    

    //default constructor
    public FileInfoPARREC(String name, String directory, int format) {
            super(name, directory, format);
           
    }
   
    //required by FileInfoBase
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogFileInfo dialog ;
        try {
        dialog= (JDialogFileInfo) dlog;
        } catch (Exception e){
            displayPrimaryInfo((JDialogText)dlog,matrix);
            return;
        }


                int[] extents;
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
                                                 Float.toString(resolutions[i]) + " " + getUnitsOfMeasureStr(measure[i]));
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
    }
    
    
    

    
    


	/** getter for vol parameters **/
	public HashMap getVolParameters() {
		return VolParameters;
	}

	public void setVolParameters(HashMap volParameters) {
		VolParameters = volParameters;
	}

	public String getSliceInfo() {
		return sliceInfo;
	}

	public void setSliceInfo(String sliceInfo) {
		this.sliceInfo = sliceInfo;
	}
	
	

	
	
}
