package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.util.*;


/**
 *
 * @see  FileMGH
 */

public class FileInfoMGH extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
    
    /** Currently version number is 1 */
    private int version = 1;
    
    private int dof;
    
    private float leftCenter;
    
    private float posteriorCenter;
    
    private float superiorCenter;
    
    private float tr = Float.NaN;
    
    private float flipAngle = Float.NaN;
    
    private float te = Float.NaN;
    
    private float ti = Float.NaN;
    
    private float fov = Float.NaN;
    
    private String transformFileName = null;
    
    private String cmdlines[] = null;
    @SuppressWarnings("unused")
    private TransMatrix matrix;

    
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * file info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoMGH(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogFileInfo dialog = (JDialogFileInfo) dlog;
        int[] extents;
        int i, j;
        int[] editorChoice = new int[1];
        editorChoice[0] = JDialogEditor.STRING;

        dialog.displayAboutInfo(this); // setup layout in the dialog

        extents = super.getExtents();

        for (i = 0; i < extents.length; i++) {
            dialog.appendPrimaryData("Dimension " + i, Integer.toString(extents[i]));
        }

        dialog.appendPrimaryData("Type", ModelStorageBase.getBufferTypeStr(getDataType()));

        
        dialog.appendPrimaryData("Min", Double.toString(getMin()));
        dialog.appendPrimaryData("Max", Double.toString(getMax()));

        dialog.appendPrimaryData("Orientation", getImageOrientationStr(getImageOrientation()));

        float[] resolutions; // = new float[5];
        resolutions = getResolutions();

        for (i = 0; i < extents.length; i++) {

            if (resolutions[i] > 0.0) {
                String pixelRes = "Pixel resolution " + i;
                dialog.appendPrimaryData(pixelRes,
                                         Float.toString(resolutions[i]));
            } // end of if (resolutions[i] > 0.0)
        } // for (i=0; i < 5; i++)

        
        dialog.appendPrimaryData("Endianess", "Big Endian");

        if (matrix != null) {

            // when using displayAboutInfo(dialog) this doesn't appear
            // calling prg might use an editing panel to adjust this matrix
            dialog.appendPrimaryData("Matrix", matrix.matrixToString(10, 4));
        }
        
        editorChoice[0] = JDialogEditor.ANALYZE_AXIS_ORIENTATION;
        dialog.appendSecondaryData("Axis: x-orientation", getAxisOrientationStr(super.getAxisOrientation(0)),
                                   editorChoice);
        dialog.appendSecondaryData("Axis: y-orientation", getAxisOrientationStr(super.getAxisOrientation(1)),
                                   editorChoice);
        dialog.appendSecondaryData("Axis: z-orientation", getAxisOrientationStr(super.getAxisOrientation(2)),
                                   editorChoice);


        editorChoice[0] = JDialogEditor.FLOAT_STRING;
        dialog.appendSecondaryData("X Origin: ", Float.toString(super.getOrigin(0)), editorChoice);
        dialog.appendSecondaryData("Y Origin: ", Float.toString(super.getOrigin(1)), editorChoice);
        dialog.appendSecondaryData("Z Origin: ", Float.toString(super.getOrigin(2)), editorChoice);

        dialog.appendSecondaryData("Version number", String.valueOf(version));
        
        dialog.appendSecondaryData("Degrees of freedom", String.valueOf(dof));
        dialog.appendSecondaryData("Left center", String.valueOf(leftCenter));
        dialog.appendSecondaryData("Posterior Center", String.valueOf(posteriorCenter));
        dialog.appendSecondaryData("Superior center", String.valueOf(superiorCenter));
        
        if (!Float.isNaN(tr)) {
            dialog.appendSecondaryData("Recovery time", String.valueOf(tr).concat( " milliseconds"));
        }
        
        if (!Float.isNaN(flipAngle)) {
            dialog.appendSecondaryData("Flip angle", String.valueOf(flipAngle).concat(" radians"));
        }
        
        if (!Float.isNaN(te)) {
            dialog.appendSecondaryData("Echo time", String.valueOf(te).concat(" milliseconds"));
        }
        
        if (!Float.isNaN(ti)) {
            dialog.appendSecondaryData("Inversion time", String.valueOf(ti).concat(" milliseconds"));
        }
        
        if (!Float.isNaN(fov)) {
            dialog.appendSecondaryData("Field of view", String.valueOf(fov).concat(" millimeters"));
        }
        
        if (transformFileName != null) {
            dialog.appendSecondaryData("Talairach transform file name", transformFileName.trim() + "\n");
        }
        
        if (cmdlines != null) {
            i = 0;
            while (cmdlines[i] != null) {
                dialog.appendSecondaryData("Command line " + String.valueOf(i+1), 
                       cmdlines[i].trim().substring(0,Math.min(80,cmdlines[i].trim().length())) + "\n");
                j = 80;
                while (cmdlines[i].trim().length() > j) {
                    dialog.appendSecondaryData(" ", cmdlines[i].substring(j,
                            Math.min(j+80, cmdlines[i].trim().length())) + "\n");
                    j += 80;
                }
                i++;
            }
        }
        
    }
    
    
    /**
     * 
     * @param version
     */
    public void setVersion(int version) {
        this.version = version;
    }
    
    /**
     * 
     * @param dof
     */
    public void setDOF(int dof) {
        this.dof = dof;
    }
    
    /**
     * 
     * @param leftCenter
     */
    public void setLeftCenter(float leftCenter) {
        this.leftCenter = leftCenter;
    }
    
    /**
     * 
     * @param posteriorCenter
     */
    public void setPosteriorCenter(float posteriorCenter) {
        this.posteriorCenter = posteriorCenter;
    }
    
    /**
     * 
     * @param superiorCenter
     */
    public void setSuperiorCenter(float superiorCenter) {
        this.superiorCenter = superiorCenter;
    }
    
    /**
     * 
     * @param tr
     */
    public void setTR(float tr) {
        this.tr = tr;
    }
    
    /**
     * 
     * @param flipAngle
     */
    public void setFlipAngle(float flipAngle) {
        this.flipAngle = flipAngle;
    }
    
    /**
     * 
     * @param te
     */
    public void setTE(float te) {
        this.te = te;
    }
    
    /**
     * 
     * @param ti
     */
    public void setTI(float ti) {
        this.ti = ti;
    }
    
    /**
     * 
     * @param fov
     */
    public void setFOV(float fov) {
        this.fov = fov;
    }
    
    /**
     * 
     * @param transformFileName
     */
    public void setTransformFileName(String transformFileName) {
        this.transformFileName = transformFileName;
    }
    
    /**
     * 
     * @param cmdlines
     */
    public void setCmdlines(String cmdlines[]) {
        this.cmdlines = cmdlines;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  matrix  DOCUMENT ME!
     */
    public void setMatrix(TransMatrix matrix) {
        this.matrix = matrix;
    }
    
    public TransMatrix getMatrix() {
    	return matrix;
    }

    
    /**
     * .
     *
     * <table>
     *   <tr>
     *     <td>ce[0] = table</td>
     *     <td>0 = primary, 1 = secondary, etC</td>
     *   </tr>
     *   <tr>
     *     <td>ce[1] = line of table</td>
     *     <td></td>
     *   </tr>
     *   <tr>
     *     <td>ce[2] = string name</td>
     *     <td>eg, "Type"</td>
     *   </tr>
     *   <tr>
     *     <td>ce[3] = Vector codeValue</td>
     *     <td>eg, "B"</td>
     *   </tr>
     *   <tr>
     *     <td>ce[4] = string value</td>
     *     <td>eg, "Big"</td>
     *   </tr>
     * </table>
     *
     * "ce" comes from ChangeEvent upon which this is based. care to make our own ChangeEvent to store and handle this?
     *
     * @param  ce  DOCUMENT ME!
     */
    @SuppressWarnings("unchecked")
    public void stateChanged(Vector ce) {
        String tname = (String) ce.elementAt(2); // [t]able [name]
        Vector tcvalue = (Vector) ce.elementAt(3); // [t]able [c]ode [value]
        //String tvalue = (String) ce.elementAt(4); // [t]able [value]

        
        if (tname.equalsIgnoreCase("Orientation")) {
            super.setImageOrientation(((Integer) tcvalue.elementAt(0)).intValue());
            // setImageOrientation(((Byte) tcvalue.elementAt(0)).byteValue());
        } else if (tname.startsWith("Axis: x-orientation")) {
            super.setAxisOrientation(((Integer) tcvalue.elementAt(0)).intValue(), 0);
        } else if (tname.startsWith("Axis: y-orientation")) {
            super.setAxisOrientation(((Integer) tcvalue.elementAt(0)).intValue(), 1);
        } else if (tname.startsWith("Axis: z-orientation")) {
            super.setAxisOrientation(((Integer) tcvalue.elementAt(0)).intValue(), 2);
        } else if (tname.startsWith("Start Location: x-axis")) {
            super.setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 0);

        } else if (tname.startsWith("Start Location: y-axis")) {
            super.setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 1);
        } else if (tname.startsWith("Start Location: z-axis")) {
            super.setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 2);
        } else if (tname.equalsIgnoreCase("Orientation")) {
            setImageOrientation(((Integer) tcvalue.elementAt(0)).intValue());
            // setOrientation(((Byte)tcvalue.elementAt(0)).byteValue());

        } else {
            Preferences.debug("tname: " + tname + ", not found.", Preferences.DEBUG_FILEIO);
        }
    }


    /**
     * Propogates the current file info to another FileInfoMGH.
     *
     * <p>It does not copy over the datatypeCode. (though, aside from, "it isn't in the about table", I can't think of a
     * reason why it shouldn't. but it doesn't.) Also, copied over is bitPix, aux_file.</p>
     *
     * @param  fInfo  DOCUMENT ME!
     */
    public void updateFileInfos(FileInfoMGH fInfo) {

        if (this == fInfo) {
            return;
        }

        
        fInfo.setImageOrientation(this.getImageOrientation());
        
    }


    /**
     * verifies string is not larger than len length; strings larger than len, are clipped before being returned.
     *
     * @see     String#substring(int, int)
     *
     * @return  String new substring
     */
    protected String setString(String str, int len) {

        if (str.length() < len) {
            return str;
        } else {
            return str.substring(0, len);
        }
    }
}
