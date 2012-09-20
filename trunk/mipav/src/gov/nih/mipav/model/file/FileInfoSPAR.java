package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogEditor;
import gov.nih.mipav.view.dialogs.JDialogFileInfo;
import gov.nih.mipav.view.dialogs.JDialogText;

public class FileInfoSPAR extends FileInfoBase {

    /** par/rec slice Angulation **/
    private double[] sliceAng;

    /** Off-centre translations **/
    private double[] offCentre;
    
    /** Mask extents **/
    private int[] maskExtents;
    
    // default constructor
    public FileInfoSPAR(String name, String directory, int format) {
        super(name, directory, format);
    }
    
    @Override
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {

        if(dlog instanceof JDialogFileInfo) {

            JDialogFileInfo dialog = (JDialogFileInfo)dlog;
            
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
            
            String ori0, ori1, ori2;
            
            ori0 = FileInfoBase.axisOrientationStr[getAxisOrientation(0)];
            ori1 = FileInfoBase.axisOrientationStr[getAxisOrientation(1)];
            ori2 = FileInfoBase.axisOrientationStr[getAxisOrientation(2)];
            
            if(sliceAng != null) {
                dialog.appendPrimaryData(ori0+" angulation: ", Double.toString(sliceAng[0]));
                dialog.appendPrimaryData(ori1+" angulation: ", Double.toString(sliceAng[1]));
                dialog.appendPrimaryData(ori2+" angulation: ", Double.toString(sliceAng[2]));
            }
            
            if(offCentre != null) {
                dialog.appendPrimaryData(ori0+" off centre: ", Double.toString(offCentre[0]));
                dialog.appendPrimaryData(ori1+" off centre: ", Double.toString(offCentre[1]));
                dialog.appendPrimaryData(ori2+" off centre: ", Double.toString(offCentre[2]));
            }
            
            if(maskExtents != null) {
                dialog.appendPrimaryData(ori0+" extents: ", Double.toString(maskExtents[0]));
                dialog.appendPrimaryData(ori1+" extents: ", Double.toString(maskExtents[1]));
                dialog.appendPrimaryData(ori2+" extents: ", Double.toString(maskExtents[2]));
            }
            
            if (matrix != null) {

                // when using displayAboutInfo(dialog) this doesn't appear
                // calling prg might use an editing panel to adjust this matrix
                dialog.appendPrimaryData("Matrix", matrix.matrixToString(10, 4));
            }
        }
    }
    
    public void setAngulation(double[] sliceAng) {
        this.sliceAng = sliceAng;
    }

    public double[] getSliceAngulation() {
        return sliceAng;
    }

    public void setOffCentre(double[] offCentre) {
        this.offCentre = offCentre;
    }

    public double[] getOffCentre() {
        return offCentre;
    }

    /**
     * @return the maskExtents
     */
    public int[] getMaskExtents() {
        return maskExtents;
    }

    /**
     * @param maskExtents the maskExtents to set
     */
    public void setMaskExtents(int[] maskExtents) {
        this.maskExtents = maskExtents;
    }

}
