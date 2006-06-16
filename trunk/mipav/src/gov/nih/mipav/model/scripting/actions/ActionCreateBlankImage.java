package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;


/**
 * TODO: comment
 */
public class ActionCreateBlankImage implements ScriptableActionInterface {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        /*tokens.nextToken(); // get rid of the word "attributes:" so tokenizer is in the right place.
        UI.createBlankImage(getRawFileInfo(false));
        
        /// getRawFileInfo()
        getNextString(); // "Attributes"

        if (multiFile) {
            fileInfo = new FileInfoImageXML((String) fileNames.elementAt(currFileIndex),
                                            (String) fileDirs.elementAt(currFileIndex), FileBase.RAW_MULTIFILE);
        } else {
            fileInfo = new FileInfoImageXML((String) fileNames.elementAt(currFileIndex),
                                            (String) fileDirs.elementAt(currFileIndex), FileBase.RAW);
        }

        fileInfo.setDataType(getNextInteger());
        fileInfo.setEndianess(getNextBoolean());
        fileInfo.setOffset(getNextInteger());

        int length = getNextInteger();
        int[] extents = new int[length];
        float[] res = new float[length];
        int[] measure = new int[length];

        for (int i = 0; i < length; i++) {
            extents[i] = getNextInteger();
            res[i] = getNextFloat();
            measure[i] = getNextInteger();
        }

        fileInfo.setExtents(extents);
        fileInfo.setUnitsOfMeasure(measure);
        fileInfo.setResolutions(res);

        return fileInfo;*/
    }
}
