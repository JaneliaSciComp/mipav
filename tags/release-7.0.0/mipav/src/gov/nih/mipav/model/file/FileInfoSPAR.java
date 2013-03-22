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

        if(dlog instanceof JDialogText) {

            JDialogText dialog = (JDialogText)dlog;
            displayPrimaryInfo((JDialogText) dlog, matrix);
            
            int[] extents;
            int i;
            int[] editorChoice = new int[1];
            editorChoice[0] = JDialogEditor.STRING;
            
            extents = super.getExtents();
    
            for (i = 0; i < extents.length; i++) {
                dialog.append("Dimension "+ i+": "+ Integer.toString(extents[i])+"\n");
            }
    
            dialog.append("Type: "+ModelStorageBase.getBufferTypeStr(getDataType())+"\n");
            
            String ori0, ori1, ori2;
            
            ori0 = FileInfoBase.axisOrientationStr[getAxisOrientation(0)];
            ori1 = FileInfoBase.axisOrientationStr[getAxisOrientation(1)];
            ori2 = FileInfoBase.axisOrientationStr[getAxisOrientation(2)];
            
            if(sliceAng != null) {
                dialog.append(ori0+" angulation: "+ Double.toString(sliceAng[0])+ "\n");
                dialog.append(ori1+" angulation: "+ Double.toString(sliceAng[1])+ "\n");
                dialog.append(ori2+" angulation: "+ Double.toString(sliceAng[2])+ "\n");
            }
            
            if(offCentre != null) {
                dialog.append(ori0+" off centre: "+ Double.toString(offCentre[0])+ "\n");
                dialog.append(ori1+" off centre: "+ Double.toString(offCentre[1])+ "\n");
                dialog.append(ori2+" off centre: "+ Double.toString(offCentre[2])+ "\n");
            }
            
            if(maskExtents != null) {
                dialog.append(ori0+" extents: "+ Double.toString(maskExtents[0])+ "\n");
                dialog.append(ori1+" extents: "+ Double.toString(maskExtents[1])+ "\n");
                dialog.append(ori2+" extents: "+ Double.toString(maskExtents[2])+ "\n");
            }
        }
    }
    
    /**
     * Sets slice angulation field.
     */
    public void setAngulation(double[] sliceAng) {
        this.sliceAng = sliceAng;
    }

    /**
     * @return the slice angulation
     */
    public double[] getSliceAngulation() {
        return sliceAng;
    }

    /**
     * Sets off-centre field.
     */
    public void setOffCentre(double[] offCentre) {
        this.offCentre = offCentre;
    }

    /**
     * @return the off-centre transformations
     */
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
